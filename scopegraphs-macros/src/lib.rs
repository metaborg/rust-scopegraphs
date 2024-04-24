// actually, don't warn! We should have the docs on `scopegraphs`
// re-exports so we can link to other things in `scopegraphs`
#![allow(missing_docs)]
///! See <https://docs.rs/scopegraphs> for all documentation
use crate::label::impl_label;
use crate::order::OrderInput;
use crate::regex::RegexInput;

use proc_macro::TokenStream;

use syn::{parse_macro_input, DeriveInput};

mod label;
mod order;
mod regex;

#[proc_macro_derive(Label)]
pub fn label_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    impl_label(input)
}

#[proc_macro]
pub fn compile_regex(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as RegexInput);
    input.compile()
}

#[proc_macro]
pub fn label_order(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as OrderInput);
    input.compile().into()
}
