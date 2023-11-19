use proc_macro2::{LexError, TokenStream};
use thiserror::Error;

mod compile;
#[cfg(feature = "dynamic")]
mod dynamic;
mod parse;
mod regex;

#[cfg(feature = "rust-code-emitting")]
mod emit;

#[cfg(feature = "dot")]
mod dot;

pub use compile::{CompiledRegex, MatchState};
pub use regex::Regex;

/// A type that can match a regex. Can be created at compile time
/// through the [`compile_regex`](scopegraphs::compile_regex) macro,
/// or at runtime with the [`dynamic`] feature through [`CompiledRegex::matcher`].
pub trait RegexMatcher<A> {
    /// accepts the specified symbol.
    ///
    /// If accepting failed, the new state is empty.
    fn accept(&mut self, inp: A);
    fn accept_many(&mut self, inp: impl IntoIterator<Item = A>) {
        for i in inp {
            self.accept(i);
        }
    }

    /// Returns true if the regular expression accepts the input iterator
    fn accepts(&mut self, iter: impl IntoIterator<Item = A>) -> bool {
        for i in iter {
            self.accept(i);
            if self.is_empty() {
                return false;
            }
        }

        self.is_accepting()
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

/// parse a string to a regular expression
pub fn parse_regex(input: impl AsRef<str>) -> Result<Regex, ParseError> {
    let stream: TokenStream = input.as_ref().parse()?;
    Ok(parse_regex_token_stream(stream)?)
}

/// parse a rust [`TokenStream`](TokenStream) to a regular expression
pub fn parse_regex_token_stream(input: TokenStream) -> syn::Result<Regex> {
    syn::parse2(input)
}
