//! Render_scopegraph is a procedural macro extension for [rustdoc](https://doc.rust-lang.org/rustdoc/what-is-rustdoc.html),
//! that aims to improve the visual component of Rust documentation through use of the [mermaid.js](https://mermaid-js.github.io/mermaid/#/) diagrams.
//!
//! > NOTE: This is a custom, heavily modified version of the [Aquamarine](https://github.com/mersinvald/aquamarine) library, distributed under the MIT license.
//!
//! `#[render_scopegraphs]` macro works through embedding the [mermaid.js](https://github.com/mermaid-js/mermaid) into the generated rustdoc HTML page, modifying the doc comment attributes.
//!
//! To inline a diagram into the documentation, use the `mermaid` snippet in a doc-string:
//!
//! ```rust
//! #  use scopegraphs_render_docs::render_scopegraphs;
//! #[cfg_attr(doc, render_scopegraph)]
//! /// ```rust
//! /// todo!()
//! ///
//! /// ```
//! pub fn example() {}
//! ```
//! The diagram will appear in place of the `mermaid` code block, preserving all the comments around it.
//!
//! You can even add multiple diagrams!
//!
//! To see it in action, go to the [demo crate](https://docs.rs/aquamarine-demo-crate/0.5.0/aquamarine_demo_crate/fn.example.html) docs.rs page.
//!
//! ### Dark-mode
//!
//! Aquamarine will automatically select the `dark` theme as a default, if the current `rustdoc` theme is either `ayu` or `dark`.
//!
//! You might need to reload the page to redraw the diagrams after changing the theme.
//!
//! ### Custom themes
//!
//! Theming is supported on per-diagram basis, through the mermaid's `%%init%%` attribute.
//!
//! *Note*: custom theme will override the default theme
//!
//! ```no_run
//! /// ```mermaid
//! /// %%{init: {
//! ///     'theme': 'base',
//! ///     'themeVariables': {
//! ///            'primaryColor': '#ffcccc',
//! ///            'edgeLabelBackground':'#ccccff',
//! ///            'tertiaryColor': '#fff0f0' }}}%%
//! /// graph TD
//! ///      A(Diagram needs to be drawn) --> B{Does it have 'init' annotation?}
//! ///      B -->|No| C(Apply default theme)
//! ///      B -->|Yes| D(Apply customized theme)
//! /// ```
//! # fn example() {}
//! ```
//! [Demo on docs.rs](https://docs.rs/aquamarine-demo-crate/0.5.0/aquamarine_demo_crate/fn.example_with_styling.html)
//!
//! To learn more, see the [Theming Section](https://mermaid-js.github.io/mermaid/#/theming) of the mermaid.js book
//!
//! ### Loading from a file
//!
//! When describing complex logic, a diagram can get quite big.
//!
//! To reduce clutter in your doc comments, you can load a diagram from file using the `include_mmd!` macro-like syntax.
//!
//! ```no_run
//! /// Diagram #1
//! /// include_mmd!("diagram_1.mmd")
//! ///
//! /// Diagram #2
//! /// include_mmd!("diagram_2.mmd")
//! # fn example() {}
//! ```
//! [Demo on docs.rs](https://docs.rs/aquamarine-demo-crate/0.5.0/aquamarine_demo_crate/fn.example_load_from_file.html)

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro_error::{abort, proc_macro_error};

use quote::quote;
use syn::{parse_macro_input, Attribute};

mod attrs;
mod parse;

/// Aquamarine is a proc-macro that adds [Mermaid](https://mermaid-js.github.io/mermaid/#/) diagrams to rustdoc
///
/// To inline a diagram into the documentation, use the `mermaid` snippet:
///
/// ```rust
/// # use scopegraphs_render_docs::render_scopegraphs;
/// #[render_scopegraphs]
/// /// ```rust
/// /// # use scopegraphs::*;
/// /// # use completeness::{UncheckedCompleteness};
/// /// # use resolve::{DataWellformedness, Resolve, ResolvedPath};
/// /// # use render::{RenderSettings, RenderScopeData, RenderScopeLabel};
/// /// #
/// /// # #[derive(Label, Hash, PartialEq, Eq, Debug, Clone, Copy)]
/// /// # enum Lbl {
/// /// #     Lex,
/// /// #     Imp,
/// /// #     Def,
/// /// # }
/// /// # use Lbl::*;
/// /// #
/// /// # #[derive(Hash, PartialEq, Eq, Debug, Default, Clone)]
/// /// # enum TData<'a> {
/// /// #     #[default]
/// /// #     NoData,
/// /// #     Data {
/// /// #         name: &'a str,
/// /// #         data: usize,
/// /// #     },
/// /// # }
/// /// #
/// /// # use TData::*;
/// /// #
/// /// # impl RenderScopeData for TData<'_> {
/// /// #     fn render_node(&self) -> Option<String> {
/// /// #         match self {
/// /// #             NoData => None,
/// /// #             Data {name, data} => Some(format!("{name}: {data}")),
/// /// #         }
/// /// #     }
/// /// # }
/// /// #
/// /// # impl RenderScopeLabel for Lbl {
/// /// #     fn render(&self) -> String {
/// /// #         match self {
/// /// #             Lex => "lex",
/// /// #             Imp => "imp",
/// /// #             Def => "def",
/// /// #         }.to_string()
/// /// #     }
/// /// # }
/// /// #
/// /// # impl<'a> TData<'a> {
/// /// #     fn matches(&self, n: &str) -> bool {
/// /// #         match self {
/// /// #             NoData => false,
/// /// #             Data { name, .. } => *name == n,
/// /// #         }
/// /// #     }
/// /// #
/// /// #     fn matcher(n: &'a str) -> impl DataWellformedness<Self> {
/// /// #         |data: &Self| data.matches(n)
/// /// #     }
/// /// #
/// /// #     fn from_default(name: &'a str) -> Self {
/// /// #         Data { name, data: 0 }
/// /// #     }
/// /// # }
/// /// # let storage = Storage::new();
/// /// # let sg: ScopeGraph<Lbl, TData, UncheckedCompleteness> =
/// /// # unsafe { ScopeGraph::raw(&storage) };
/// ///
/// /// let s0 = sg.add_scope_default();
/// /// let s1 = sg.add_scope_default();
/// /// sg.add_edge(s0, Lex, s1);
/// /// sg.add_decl(s0, Def, Data { name: "x", data: 0 });
/// /// sg.add_decl(s1, Def, Data { name: "x", data: 1 });
/// ///
/// /// sg.render_to("output.dot", RenderSettings::default()).unwrap();
/// /// ```
/// struct Foo;
/// ```
#[proc_macro_attribute]
#[proc_macro_error]
pub fn render_scopegraphs(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as parse::Input);

    check_input_attrs(&input.attrs);

    let attrs = attrs::Attrs::from(input.attrs);
    let forward = input.rest;

    let tokens = quote! {
        #attrs
        #forward
    };

    tokens.into()
}

fn check_input_attrs(input: &[Attribute]) {
    for attr in input {
        if attr.path().is_ident("render_scopegraph") {
            abort!(
                attr,
                "multiple `render_scopegraph` attributes on one entity are illegal"
            );
        }
    }
}
