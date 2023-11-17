use proc_macro2::{LexError, TokenStream};
use thiserror::Error;

mod compile;
mod parse;
mod regex;

pub use regex::Regex;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("lex error: {0}")]
    Lex(#[from] LexError),
    #[error("parse error: {0}")]
    Parse(#[from] syn::Error),
}

pub fn parse_regex(input: impl AsRef<str>) -> Result<Regex, ParseError> {
    let stream: TokenStream = input.as_ref().parse()?;
    Ok(parse_token_stream(stream)?)
}

pub fn parse_token_stream(input: TokenStream) -> syn::Result<Regex> {
    syn::parse2(input)
}

// impl ToTokens for Regex {
//     fn to_tokens(&self, tokens: &mut TokenStream2) {
//         tokens.extend(match self {
//             Regex::Empty => quote! {scopegraphs::regex::Regex::Empty},
//             Regex::Epsilon => quote! {scopegraphs::regex::Regex::Epsilon},
//             Regex::Symbol(s) => quote! {scopegraphs::regex::Regex::Symbol(#s)},
//             Regex::Repeat(r) => quote! {scopegraphs::regex::Regex::Repeat(&#r)},
//             Regex::Complement(c) => quote! {scopegraphs::regex::Regex::Complement(&#c)},
//             Regex::Or(l, r) => {
//                 quote! {scopegraphs::regex::Regex::Or(&#l, &#r)}
//             }
//             Regex::And(l, r) => {
//                 quote! {scopegraphs::regex::Regex::And(&#l, &#r)}
//             }
//             Regex::Concat(l, r) => {
//                 quote! {scopegraphs::regex::Regex::Concat(&#l, &#r)}
//             }
//         })
//     }
// }
