use proc_macro2::{LexError, TokenStream};
use thiserror::Error;

mod compile;
mod parse;
mod regex;

#[cfg(feature = "rust-code-emitting")]
mod emit;

#[cfg(feature = "dot")]
mod dot;

pub use compile::{CompiledRegex, MatchState};
pub use regex::Regex;

pub trait RegexMatcher {
    type Alphabet;

    /// accepts the specified symbol.
    ///
    /// If accepting failed, the new state is empty.
    fn accept(&mut self, inp: Self::Alphabet);
    fn accept_many(&mut self, inp: impl Iterator<Item = Self::Alphabet>) {
        for i in inp {
            self.accept(i);
        }
    }

    fn is_final(&self) -> bool;
    fn is_accepting(&self) -> bool;
    fn is_oblivion(&self) -> bool;
    fn is_empty(&self) -> bool {
        self.is_final() && !self.is_accepting()
    }
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("lex error: {0}")]
    Lex(#[from] LexError),
    #[error("parse error: {0}")]
    Parse(#[from] syn::Error),
}

pub fn parse_regex(input: impl AsRef<str>) -> Result<Regex, ParseError> {
    let stream: TokenStream = input.as_ref().parse()?;
    Ok(parse_regex_token_stream(stream)?)
}

pub fn parse_regex_token_stream(input: TokenStream) -> syn::Result<Regex> {
    syn::parse2(input)
}
