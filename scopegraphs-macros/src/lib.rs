use crate::label::impl_label;
use proc_macro::TokenStream;
use quote::quote;
use scopegraphs_regular_expressions::Regex;
use syn::{parse_macro_input, DeriveInput};

mod label;

#[proc_macro_derive(Label)]
pub fn label_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    impl_label(input)
}

#[proc_macro]
pub fn regex(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Regex);
    let compiled = input.compile();
    quote!(compiled).into()
}
