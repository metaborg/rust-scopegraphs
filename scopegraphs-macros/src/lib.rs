use crate::label::impl_label;
use proc_macro::TokenStream;
use scopegraphs_regular_expressions::Regex;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, DeriveInput, Ident, Token, Type};

mod label;

#[proc_macro_derive(Label)]
pub fn label_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    impl_label(input)
}

struct RegexInput {
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

/// Syntax: `type $type<$alphabet_type> = regex`.
/// For example:
///
/// ```rust
/// # use scopegraphs::*;
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
#[proc_macro]
pub fn compile_regex(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as RegexInput);
    let compiled = input.regex.compile();

    compiled.emit(&input.name, &input.alphabet_type).into()
}
