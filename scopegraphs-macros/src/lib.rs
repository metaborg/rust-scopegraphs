use crate::label::impl_label;
use proc_macro::TokenStream;
use quote::quote_spanned;
use scopegraphs_regular_expressions::Regex;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Attribute, DeriveInput, Ident, Meta, Token, Type};

#[cfg(feature = "dot")]
use std::fs::File;
#[cfg(feature = "dot")]
use syn::{Expr, ExprLit, Lit, MetaNameValue};

mod label;

#[proc_macro_derive(Label)]
pub fn label_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    impl_label(input)
}

struct RegexInput {
    attrs: Vec<Attribute>,
    _type: Token![type],
    name: Ident,
    _open: Token![<],
    alphabet_type: Type,
    _close: Token![>],
    _equals: Token![=],
    regex: Regex,
}

impl Parse for RegexInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            attrs: input.call(Attribute::parse_outer)?,
            _type: input.parse()?,
            name: input.parse()?,
            _open: input.parse()?,
            alphabet_type: input.parse()?,
            _close: input.parse()?,
            _equals: input.parse()?,
            regex: input.parse()?,
        })
    }
}

/// Syntax: `$attrs type $type<$alphabet_type> = regex`.
/// For example:
///
/// ```rust
/// # use std::borrow;
/// use scopegraphs::*;
///
/// pub enum Alphabet {
///     A,
///     B,
///     C,
/// }
/// use Alphabet::*;
///
/// compile_regex!(type Machine<Alphabet> = A* B);
/// assert!(Machine::new().accepts([A, B]));
/// ```
///
/// # Attributes
/// * `#[graph="$path"]` location to put a graphviz dot file representing the generated finite state machine. (only with the `dot` feature)
#[proc_macro]
pub fn compile_regex(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as RegexInput);

    #[cfg(feature = "dot")]
    let mut graph = None;

    for i in input.attrs {
        match i.meta {
            #[cfg(feature = "dot")]
            Meta::NameValue(MetaNameValue {
                path,
                value:
                    Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }),
                ..
            }) if path.is_ident("graph") => {
                graph = Some(s);
            }
            i => {
                return quote_spanned!(i.span() => compile_error!("unexpected attribute");).into();
            }
        }
    }

    let compiled = input.regex.compile();

    #[cfg(feature = "dot")]
    if let Some(path) = graph {
        let path = path.value();
        let mut f = File::create(&path)
            .unwrap_or_else(|e| panic!("can't open dot file for graphing at {path}: {e}"));
        compiled
            .output_dot(&mut f)
            .unwrap_or_else(|e| panic!("failed while graphing at {path}: {e}"));
    }

    compiled.emit(&input.name, &input.alphabet_type).into()
}
