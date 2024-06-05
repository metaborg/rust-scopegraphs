use include_dir::{include_dir, Dir};
use itertools::Itertools;
use proc_macro::Span;
use proc_macro2::TokenStream;
use proc_macro_error::{abort, emit_call_site_warning, emit_error};
use quote::quote;
use std::env::temp_dir;
use std::fs::{read_to_string, write};
use std::io::{stderr, stdout, Write};
use std::path::Path;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::{fs, io};
use syn::{Attribute, Ident, MetaNameValue};
use uuid::Uuid;

// embedded JS code being inserted as html script elements
static MERMAID_JS_DIR: Dir = include_dir!("$CARGO_MANIFEST_DIR/doc/js/");

// Note: relative path depends on sub-module the macro is invoked in:
//  base=document.getElementById("rustdoc-vars").attributes["data-root-path"]
const MERMAID_JS_LOCAL: &str = "static.files.mermaid/mermaid.esm.min.mjs";
const MERMAID_JS_LOCAL_DIR: &str = "static.files.mermaid";
const MERMAID_JS_CDN: &str = "https://unpkg.com/mermaid@10/dist/mermaid.esm.min.mjs";

const UNEXPECTED_ATTR_ERROR: &str =
    "unexpected attribute inside a diagram definition: only #[doc] is allowed";

#[derive(Clone, Default)]
pub struct Attrs(Vec<Attr>);

#[derive(Clone)]
pub enum Attr {
    /// Attribute that is to be forwarded as-is
    Forward(Attribute),
    /// Doc comment that cannot be forwarded as-is
    DocComment(Ident, String),
    /// Diagram start token
    DiagramStart(Ident),
    /// Diagram entry (line)
    DiagramEntry(Ident, String),
    /// Diagram end token
    DiagramEnd(Ident),
    /// Include Anchor
    DiagramIncludeAnchor(Ident, PathBuf),
}

impl Attr {
    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Attr::Forward(attr) => attr.path().get_ident(),
            Attr::DocComment(ident, _) => Some(ident),
            Attr::DiagramStart(ident) => Some(ident),
            Attr::DiagramEntry(ident, _) => Some(ident),
            Attr::DiagramEnd(ident) => Some(ident),
            Attr::DiagramIncludeAnchor(ident, _) => Some(ident),
        }
    }

    pub fn is_diagram_end(&self) -> bool {
        matches!(self, Attr::DiagramEnd(_))
    }

    pub fn is_diagram_start(&self) -> bool {
        matches!(self, Attr::DiagramStart(_))
    }

    pub fn expect_diagram_entry_text(&self) -> &str {
        match self {
            Attr::DiagramEntry(_, body) => body.as_str(),
            _ => abort!(self.as_ident(), UNEXPECTED_ATTR_ERROR),
        }
    }
}
impl From<Vec<Attribute>> for Attrs {
    fn from(attrs: Vec<Attribute>) -> Self {
        let mut out = Attrs::default();
        out.push_attrs(attrs);
        out
    }
}

impl quote::ToTokens for Attrs {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut attrs = self.0.iter();
        while let Some(attr) = attrs.next() {
            match attr {
                Attr::Forward(attr) => attr.to_tokens(tokens),
                Attr::DocComment(_, comment) => tokens.extend(quote! {
                    #[doc = #comment]
                }),
                Attr::DiagramStart(_) => {
                    let diagram = attrs
                        .by_ref()
                        .take_while(|x| !x.is_diagram_end())
                        .map(Attr::expect_diagram_entry_text)
                        .collect::<Vec<_>>();

                    if !diagram
                        .iter()
                        .filter(|i| !i.trim().is_empty())
                        .all(|i| i.trim().starts_with('#'))
                        && !diagram.is_empty()
                    {
                        tokens.extend(quote! {#[doc = "```rust"]});
                        for i in &diagram {
                            tokens.extend(quote! {
                                #[doc = #i]
                            });
                        }
                        tokens.extend(quote! {#[doc = "```"]});
                    }

                    match generate_diagram_rustdoc(&diagram) {
                        Ok(i) => {
                            tokens.extend(i);
                        }
                        Err(e) => match e {
                            EvalError::CreateDir(i) => {
                                emit_error!(
                                    Span::call_site(),
                                    "failed to create temporary directory to generate mermaid files {:?}",
                                    i,
                                );
                            }
                            EvalError::WriteProject(i) => {
                                emit_error!(
                                    Span::call_site(),
                                    "failed to write project file in temporary directory to generate mermaid files {:?}",
                                    i,
                                );
                            }
                            EvalError::RunCargo(i) => {
                                emit_error!(Span::call_site(), "error while running cargo {:?}", i);
                            }
                            EvalError::ReadDir(i) => {
                                emit_error!(
                                    Span::call_site(),
                                    "error while looking for output files {:?}",
                                    i
                                );
                            }
                        },
                    }
                }
                // If that happens, then the parsing stage is faulty: doc comments outside of
                // in between Start and End tokens are to be emitted as Attr::Forward or Attr::DocComment
                Attr::DiagramEntry(_, body) => {
                    emit_call_site_warning!("encountered an unexpected attribute that's going to be ignored, this is a bug! ({})", body);
                }
                Attr::DiagramEnd(_) => (),
                Attr::DiagramIncludeAnchor(_, path) => {
                    // get cargo manifest dir
                    let manifest_dir =
                        std::env::var("CARGO_MANIFEST_DIR").unwrap_or(".".to_string());

                    // append path to cargo manifest dir using PathBuf
                    let path = &PathBuf::new().join(manifest_dir).join(path);

                    let data = match fs::read_to_string(path) {
                        Ok(data) => data,
                        Err(e) => {
                            emit_error!(
                                Span::call_site(),
                                "failed to read mermaid file from path {:?}: {}",
                                path,
                                e,
                            );
                            continue;
                        }
                    };
                    tokens.extend(generate_diagram_rustdoc(&[data.as_str()]))
                }
            }
        }
    }
}

fn place_mermaid_js() -> io::Result<()> {
    let target_dir = std::env::var("CARGO_TARGET_DIR").unwrap_or("./target".to_string());
    let docs_dir = Path::new(&target_dir).join("doc");
    // extract mermaid module iff rustdoc folder exists already
    if docs_dir.exists() {
        let static_files_mermaid_dir = docs_dir.join(MERMAID_JS_LOCAL_DIR);
        if static_files_mermaid_dir.exists() {
            Ok(())
        } else {
            fs::create_dir_all(&static_files_mermaid_dir).unwrap();
            MERMAID_JS_DIR.extract(static_files_mermaid_dir)?;
            Ok(())
        }
    } else {
        // no rustdocs rendering
        Ok(())
    }
}

const MERMAID_INIT_SCRIPT: &str = r#"
    const mermaidModuleFile = "{mermaidModuleFile}";
    const fallbackRemoteUrl = "{fallbackRemoteUrl}";
    const rustdocVarsId= "rustdoc-vars";
    const dataRootPathAttr = "data-root-path";


    function initializeMermaid(mermaid) {
     var amrn_mermaid_theme =
         window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches
         ? 'dark'
         : 'default';

      mermaid.initialize({
        'startOnLoad':'true',
        'theme': amrn_mermaid_theme,
        'logLevel': 3 });
      mermaid.run();
    }

	function failedToLoadWarnings() {
		for(var elem of document.getElementsByClassName("mermaid")) {
			 elem.innerHTML =
			 `<div> <mark>
			  &#9888; Cannot render diagram! Failed to import module from local
			  file and remote location also!
			  Either access the rustdocs via HTTP/S using a
			  <a href="https://developer.mozilla.org/en-US/docs/Learn/Common_questions/Tools_and_setup/set_up_a_local_testing_server">
			   local web server
			  </a>, for example:
			   python3 -m http.server --directory target/doc/, <br> or enable local file access in your
			   Safari/Firefox/Chrome browser, for example
			  starting Chrome with flag '--allow-file-access-from-files'.
			  </mark></div> `;
		}
	}


    // If rustdoc is read from file directly, the import of mermaid module
    // from file will fail. In this case falling back to remote location.
    // If neither succeeds, the mermaid markdown is replaced by notice to
    // enable file acecss in browser.
    try {
       var rootPath = document
         .getElementById(rustdocVarsId)
         .attributes[dataRootPathAttr]
         .value;
       const {
         default: mermaid,
       } = await import(rootPath + mermaidModuleFile);
	   initializeMermaid(mermaid);
    } catch (e) {
       try {
         const {
            default: mermaid,
         } = await import(fallbackRemoteUrl);
	     initializeMermaid(mermaid);
       } catch (e) {
		 failedToLoadWarnings();
	   }
    }
"#;

pub enum EvalError {
    CreateDir(io::Error),
    WriteProject(io::Error),
    RunCargo(io::Error),
    ReadDir(io::Error),
}

fn find_diagrams(path: &Path) -> Result<Vec<String>, EvalError> {
    let mut diagrams = Vec::new();

    for entry in fs::read_dir(path).map_err(EvalError::ReadDir)? {
        let Ok(entry) = entry else {
            continue;
        };
        if let Some(true) = entry
            .path()
            .extension()
            .map(|i| i.to_string_lossy() == "mmd")
        {
            diagrams.push(read_to_string(entry.path()).map_err(EvalError::ReadDir)?)
        }
    }

    Ok(diagrams)
}

fn run_code(code: &str) -> Result<Vec<String>, EvalError> {
    let manifest_dir = PathBuf::from(
        std::env::var("CARGO_MANIFEST_DIR").expect("$CARGO_MANIFEST_DIR is set during compilation"),
    );
    let target_dir = std::env::var("CARGO_TARGET_DIR").unwrap_or("./target".to_string());

    // this if is so it works on docs.rs,
    // which sets a custom target and compiles in a folder called "workdir"
    let (sg_dir, target_dir, offline) = if manifest_dir.join("..").join("workdir").exists() {
        (
            manifest_dir
                .join("..")
                .join("workdir")
                .to_string_lossy()
                .to_string(),
            "/opt/rustwide/target".to_string(),
            true,
        )
    } else {
        (
            manifest_dir
                .join("..")
                .join("scopegraphs")
                .to_string_lossy()
                .to_string(),
            target_dir,
            true,
        )
    };

    fn copy_dir_all(src: impl AsRef<Path>, dst: impl AsRef<Path>) -> io::Result<()> {
        fs::create_dir_all(&dst)?;
        for entry in fs::read_dir(src)? {
            let entry = entry?;
            let ty = entry.file_type()?;
            if ty.is_dir() {
                copy_dir_all(entry.path(), dst.as_ref().join(entry.file_name()))?;
            } else {
                fs::copy(entry.path(), dst.as_ref().join(entry.file_name()))?;
            }
        }
        Ok(())
    }

    let code_hash = Uuid::new_v4().to_string();
    let out_dir = &temp_dir().join("render-docs").join(&code_hash);
    println!("testing in {out_dir:?}");

    let sg_target_dir = temp_dir().join(format!("SG_TARGET-{}", code_hash));
    if sg_target_dir.exists() {
        let _ = fs::remove_dir_all(&sg_target_dir);
    }
    let _ = copy_dir_all(target_dir, &sg_target_dir);
    if sg_target_dir.join("debug").exists() {
        let _ = fs::remove_file(sg_target_dir.join("debug").join(".cargo-lock"));
        let _ = fs::remove_file(sg_target_dir.join("debug").join(".fingerprint"));
        let _ = fs::remove_file(sg_target_dir.join("debug").join("tmp"));
        let _ = fs::remove_file(sg_target_dir.join("debug").join("incremental"));
    }
    if sg_target_dir.join("release").exists() {
        let _ = fs::remove_file(sg_target_dir.join("release").join(".cargo-lock"));
        let _ = fs::remove_file(sg_target_dir.join("release").join(".fingerprint"));
        let _ = fs::remove_file(sg_target_dir.join("release").join("tmp"));
        let _ = fs::remove_file(sg_target_dir.join("release").join("incremental"));
    }

    let cargo = PathBuf::from(std::env::var("CARGO").expect("$CARGO is set during compilation"));

    if out_dir.exists() {
        let _ = fs::remove_dir_all(out_dir);
    }

    fs::create_dir_all(out_dir).map_err(EvalError::CreateDir)?;
    fs::create_dir_all(out_dir.join("src")).map_err(EvalError::CreateDir)?;

    write(
        out_dir.join("Cargo.toml"),
        format!(
            "
[package]
name = \"render_docs_{code_hash}\"
edition=\"2021\"

[dev-dependencies]
scopegraphs = {{path = \"{}\"}}
    ",
            sg_dir
        ),
    )
    .map_err(EvalError::WriteProject)?;
    write(
        out_dir.join("src").join("lib.rs"),
        format!(
            "
/// ```rust
{}
/// ```
#[allow(unused)]
fn documented() {{}}
    ",
            code.lines().map(|i| format!("/// {i}")).join("\n")
        ),
    )
    .map_err(EvalError::WriteProject)?;

    let mut command = Command::new(cargo);
    let command = command
        .current_dir(out_dir)
        .env("CARGO_TARGET_DIR", &sg_target_dir)
        .env_remove("CARGO_MAKEFLAGS")
        .arg("test");

    let output = if offline {
        command.arg("--offline")
    } else {
        command
    }
    .arg("--doc")
    .arg("documented")
    .stdout(Stdio::inherit())
    .stderr(Stdio::inherit())
    .output()
    .map_err(EvalError::RunCargo)?;

    if !output.status.success() {
        stdout().write_all(&output.stdout).unwrap();
        stderr().write_all(&output.stderr).unwrap();
        panic!("build docs didn't work")
    }

    let _ = fs::remove_dir_all(&sg_target_dir);

    find_diagrams(out_dir)
}

fn generate_diagram_rustdoc(parts: &[&str]) -> Result<TokenStream, EvalError> {
    let code = parts.join("\n");
    let diagrams = run_code(&code)?;

    let preamble = r#"<div class="mermaid">"#;
    let postamble = "</div>";

    let mermaid_js_init = format!(
        r#"<script type="module">{}</script>"#,
        MERMAID_INIT_SCRIPT
            .replace("{mermaidModuleFile}", MERMAID_JS_LOCAL)
            .replace("{fallbackRemoteUrl}", MERMAID_JS_CDN)
    );

    let bodies: Vec<_> = diagrams
        .into_iter()
        .map(|diagram| format!("{preamble}\n{diagram}\n{postamble}"))
        .collect();

    place_mermaid_js().unwrap_or_else(|e| {
        eprintln!("failed to place mermaid.js on the filesystem: {}", e);
    });

    Ok(quote! {
        #[doc = #mermaid_js_init]
        #(
            #[doc = #bodies]
        )*
    })
}

impl Attrs {
    pub fn push_attrs(&mut self, attrs: Vec<Attribute>) {
        use syn::Expr;
        use syn::ExprLit;
        use syn::Lit::*;

        let mut current_location = Location::OutsideDiagram;
        let mut diagram_start_ident = None;

        for attr in attrs {
            match attr.meta.require_name_value() {
                Ok(MetaNameValue {
                    value: Expr::Lit(ExprLit { lit: Str(s), .. }),
                    path,
                    ..
                }) if path.is_ident("doc") => {
                    let ident = path.get_ident().unwrap();
                    for attr in split_attr_body(ident, &s.value(), &mut current_location) {
                        if attr.is_diagram_start() {
                            diagram_start_ident.replace(ident.clone());
                        }
                        self.0.push(attr);
                    }
                }
                _ => {
                    if let Location::InsideDiagram = current_location {
                        abort!(attr, UNEXPECTED_ATTR_ERROR)
                    } else {
                        self.0.push(Attr::Forward(attr))
                    }
                }
            }
        }

        if current_location.is_inside() {
            abort!(diagram_start_ident, "diagram code block is not terminated");
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Location {
    OutsideDiagram,
    InsideDiagram,
}

impl Location {
    fn is_inside(self) -> bool {
        matches!(self, Location::InsideDiagram)
    }
}

const TICKS: &str = "```";

fn split_attr_body(ident: &Ident, input: &str, loc: &mut Location) -> Vec<Attr> {
    use self::Location::*;

    const RUST: &str = "rust";

    let mut tokens = tokenize_doc_str(input).peekable();

    // Special case: empty strings outside the diagram span should be still generated
    if tokens.peek().is_none() && !loc.is_inside() {
        return vec![Attr::DocComment(ident.clone(), String::new())];
    };

    // To aid rustc with type inference in closures
    #[derive(Default)]
    struct Ctx<'a> {
        attrs: Vec<Attr>,
        buffer: Vec<&'a str>,
    }

    let mut ctx: Ctx<'_> = Default::default();

    let flush_buffer_as_doc_comment = |ctx: &mut Ctx| {
        if !ctx.buffer.is_empty() {
            ctx.attrs.push(Attr::DocComment(
                ident.clone(),
                ctx.buffer.drain(..).join(" "),
            ));
        }
    };

    let flush_buffer_as_diagram_entry = |ctx: &mut Ctx| {
        let s = ctx.buffer.drain(..).join(" ");
        // if !s.trim().is_empty() {
        ctx.attrs.push(Attr::DiagramEntry(ident.clone(), s));
        // }
    };

    while let Some(token) = tokens.next() {
        match (*loc, token, tokens.peek()) {
            // Detect include anchor
            (OutsideDiagram, token, _) if token.starts_with("include_mmd!") => {
                // cleanup
                let path = token.trim_start_matches("include_mmd!").trim();
                let path = path.trim_start_matches('(').trim_end_matches(')');
                let path = path.trim_matches('"');
                let path = PathBuf::from(path);
                ctx.attrs
                    .push(Attr::DiagramIncludeAnchor(ident.clone(), path));
            }
            // Flush the buffer, then open the diagram code block
            (OutsideDiagram, TICKS, Some(&RUST)) => {
                tokens.next();
                *loc = InsideDiagram;
                flush_buffer_as_doc_comment(&mut ctx);
                ctx.attrs.push(Attr::DiagramStart(ident.clone()));
            }
            // Flush the buffer, close the code block
            (InsideDiagram, TICKS, _) => {
                *loc = OutsideDiagram;
                flush_buffer_as_diagram_entry(&mut ctx);
                ctx.attrs.push(Attr::DiagramEnd(ident.clone()))
            }
            _ => ctx.buffer.push(token),
        }
    }

    // if !ctx.buffer.is_empty() {
    if loc.is_inside() {
        flush_buffer_as_diagram_entry(&mut ctx);
    } else {
        flush_buffer_as_doc_comment(&mut ctx);
    };
    // }

    ctx.attrs
}

fn tokenize_doc_str(input: &str) -> impl Iterator<Item = &str> {
    split_inclusive(input, TICKS).flat_map(|token| {
        // not str::split_whitespace because we don't wanna filter-out the whitespace tokens
        token.split(' ')
    })
}

// TODO: remove once str::split_inclusive is stable
fn split_inclusive<'a, 'b: 'a>(input: &'a str, delim: &'b str) -> impl Iterator<Item = &'a str> {
    let mut tokens = vec![];
    let mut prev = 0;

    for (idx, matches) in input.match_indices(delim) {
        tokens.extend(nonempty(&input[prev..idx]));

        prev = idx + matches.len();

        tokens.push(matches);
    }

    if prev < input.len() {
        tokens.push(&input[prev..]);
    }

    tokens.into_iter()
}

fn nonempty(s: &str) -> Option<&str> {
    if s.is_empty() {
        None
    } else {
        Some(s)
    }
}

#[cfg(test)]
mod tests {
    use super::{split_inclusive, Attr};
    use std::fmt;

    #[cfg(test)]
    impl fmt::Debug for Attr {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Attr::Forward(..) => f.write_str("Attr::Forward"),
                Attr::DocComment(_, body) => write!(f, "Attr::DocComment({:?})", body),
                Attr::DiagramStart(..) => f.write_str("Attr::DiagramStart"),
                Attr::DiagramEntry(_, body) => write!(f, "Attr::DiagramEntry({:?})", body),
                Attr::DiagramEnd(..) => f.write_str("Attr::DiagramEnd"),
                Attr::DiagramIncludeAnchor(_, path) => {
                    write!(f, "Attr::DiagramIncludeAnchor({:?})", path)
                }
            }
        }
    }

    #[cfg(test)]
    impl Eq for Attr {}

    #[cfg(test)]
    impl PartialEq for Attr {
        fn eq(&self, other: &Self) -> bool {
            use std::mem::discriminant;
            use Attr::*;
            match (self, other) {
                (DocComment(_, a), DocComment(_, b)) => a == b,
                (DiagramEntry(_, a), DiagramEntry(_, b)) => a == b,
                (a, b) => discriminant(a) == discriminant(b),
            }
        }
    }

    #[test]
    fn temp_split_inclusive() {
        let src = "```";
        let out: Vec<_> = split_inclusive(src, "```").collect();
        assert_eq!(&out, &["```",]);

        let src = "```abcd```";
        let out: Vec<_> = split_inclusive(src, "```").collect();
        assert_eq!(&out, &["```", "abcd", "```"]);

        let src = "left```abcd```right";
        let out: Vec<_> = split_inclusive(src, "```").collect();
        assert_eq!(&out, &["left", "```", "abcd", "```", "right",]);
    }

    mod split_attr_body_tests {
        use super::super::*;

        use proc_macro2::Ident;
        use proc_macro2::Span;

        use pretty_assertions::assert_eq;

        fn i() -> Ident {
            Ident::new("fake", Span::call_site())
        }

        struct TestCase<'a> {
            ident: Ident,
            location: Location,
            input: &'a str,
            expect_location: Location,
            expect_attrs: Vec<Attr>,
        }

        fn check(case: TestCase) {
            let mut loc = case.location;
            let attrs = split_attr_body(&case.ident, case.input, &mut loc);
            println!("{attrs:?}");
            println!("---");
            println!("{:?}", case.expect_attrs);
            assert_eq!(loc, case.expect_location);
            assert_eq!(attrs, case.expect_attrs);
        }

        #[test]
        fn one_line_one_diagram() {
            let case = TestCase {
                ident: i(),
                location: Location::OutsideDiagram,
                input: "```rust abcd```",
                expect_location: Location::OutsideDiagram,
                expect_attrs: vec![
                    Attr::DiagramStart(i()),
                    Attr::DiagramEntry(i(), "abcd".into()),
                    Attr::DiagramEnd(i()),
                ],
            };

            check(case)
        }

        #[test]
        fn one_line_multiple_diagrams() {
            let case = TestCase {
                ident: i(),
                location: Location::OutsideDiagram,
                input: "```rust abcd``` ```rust efgh``` ```rust ijkl```",
                expect_location: Location::OutsideDiagram,
                expect_attrs: vec![
                    Attr::DiagramStart(i()),
                    Attr::DiagramEntry(i(), "abcd".into()),
                    Attr::DiagramEnd(i()),
                    Attr::DocComment(i(), " ".into()),
                    Attr::DiagramStart(i()),
                    Attr::DiagramEntry(i(), "efgh".into()),
                    Attr::DiagramEnd(i()),
                    Attr::DocComment(i(), " ".into()),
                    Attr::DiagramStart(i()),
                    Attr::DiagramEntry(i(), "ijkl".into()),
                    Attr::DiagramEnd(i()),
                ],
            };

            check(case)
        }

        #[test]
        fn other_snippet() {
            let case = TestCase {
                ident: i(),
                location: Location::OutsideDiagram,
                input: "```python print()```",
                expect_location: Location::OutsideDiagram,
                expect_attrs: vec![Attr::DocComment(i(), "``` python print() ```".into())],
            };

            check(case)
        }

        #[test]
        fn carry_over() {
            let case = TestCase {
                ident: i(),
                location: Location::OutsideDiagram,
                input: "left```rust abcd```right",
                expect_location: Location::OutsideDiagram,
                expect_attrs: vec![
                    Attr::DocComment(i(), "left".into()),
                    Attr::DiagramStart(i()),
                    Attr::DiagramEntry(i(), "abcd".into()),
                    Attr::DiagramEnd(i()),
                    Attr::DocComment(i(), "right".into()),
                ],
            };

            check(case)
        }

        #[test]
        fn multiline_termination() {
            let case = TestCase {
                ident: i(),
                location: Location::InsideDiagram,
                input: "abcd```",
                expect_location: Location::OutsideDiagram,
                expect_attrs: vec![
                    Attr::DiagramEntry(i(), "abcd".into()),
                    Attr::DiagramEnd(i()),
                ],
            };

            check(case)
        }

        #[test]
        fn multiline_termination_single_token() {
            let case = TestCase {
                ident: i(),
                location: Location::InsideDiagram,
                input: "```",
                expect_location: Location::OutsideDiagram,
                expect_attrs: vec![
                    Attr::DiagramEntry(i(), "".to_string()),
                    Attr::DiagramEnd(i()),
                ],
            };

            check(case)
        }

        #[test]
        fn multiline_termination_carry() {
            let case = TestCase {
                ident: i(),
                location: Location::InsideDiagram,
                input: "abcd```right",
                expect_location: Location::OutsideDiagram,
                expect_attrs: vec![
                    Attr::DiagramEntry(i(), "abcd".into()),
                    Attr::DiagramEnd(i()),
                    Attr::DocComment(i(), "right".into()),
                ],
            };

            check(case)
        }
    }
}
