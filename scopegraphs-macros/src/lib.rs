use crate::label::impl_label;
use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod label;

#[proc_macro_derive(Label)]
pub fn label_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    impl_label(input)
}
