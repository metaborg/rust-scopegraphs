use crate::label::impl_label;
use proc_macro::TokenStream;
use quote::quote;
use scopegraphs_regular_expressions::Regex;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, DeriveInput, Token, Type};

mod label;

#[proc_macro_derive(Label)]
pub fn label_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    impl_label(input)
}

struct RegexInput {
    alphabet_type: Type,
    _semi: Token![;],
    regex: Regex,
}

impl Parse for RegexInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            alphabet_type: input.parse()?,
            _semi: input.parse()?,
            regex: input.parse()?,
        })
    }
}

#[proc_macro]
pub fn regex(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as RegexInput);
    let compiled = input.regex.compile();
    let alphabet_type = input.alphabet_type;

    quote!({
        type A = #alphabet_type;

        #compiled

        Machine::new()
    })
    .into()
}
