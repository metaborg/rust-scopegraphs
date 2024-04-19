#![deny(missing_docs)]

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

pub use compile::{Automaton, MatchState};
pub use regex::Regex;

/// A type that can match a regex. Can be created at compile time
/// through the [`compile_regex`](scopegraphs::compile_regex) macro,
/// or at runtime with the [`dynamic`] feature through [`Automaton::matcher`].
pub trait RegexMatcher<A>: Clone {
    /// Takes a transition in the state machine, accepting a single symbol.
    ///
    /// If the symbol was not accepted, because the regex doesn't allow it, the new state is empty,
    /// which can be checked using [`RegexMatcher::is_empty`].
    fn step(&mut self, inp: A);

    /// Steps once for each element in the iterator
    fn step_many(&mut self, inp: impl IntoIterator<Item = A>) {
        for i in inp {
            self.step(i);
        }
    }

    /// Steps once for each element in the iterator
    fn step_unless_empty(&mut self, inp: impl IntoIterator<Item = A>) {
        for i in inp {
            self.step(i);
            if self.is_empty() {
                return;
            }
        }
    }

    /// Returns true if the regular expression accepts the input iterator
    fn accepts(&self, iter: impl IntoIterator<Item = A>) -> bool {
        let mut this = self.clone();
        this.step_unless_empty(iter);
        this.is_accepting()
    }

    /// Returns true if the regular expression accepts a word of which the input iterator is a prefix
    fn accepts_prefix(&self, iter: impl IntoIterator<Item = A>) -> bool {
        let mut this = self.clone();
        this.step_unless_empty(iter);
        !this.is_empty()
    }

    fn is_final(&self) -> bool;
    fn is_accepting(&self) -> bool;
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
