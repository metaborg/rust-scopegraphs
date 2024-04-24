//! # Scope Graphs Regular Expressions
//!
//! To query a scope graph,
//! you have to specify what paths are valid for the query to take.
//! You do so using a regular expression over the labels on the edges of the scope graph.
//!
//! See <https://docs.rs/scopegraphs> for all documentation
#![warn(missing_docs)]

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

pub use compile::Automaton;
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

    /// Returns whether this state is a final state.
    ///
    /// A Final state is a state without outgoing edges
    fn is_final(&self) -> bool;

    /// Returns whether the current state would accept the regular expression.
    fn is_accepting(&self) -> bool;

    /// Returns whether the regular expression is stuck:
    /// there are no more outgoing edges and it's not currently accepting.
    fn is_empty(&self) -> bool {
        self.is_final() && !self.is_accepting()
    }
}

/// An error that can ocur while parsing a regular expression.
#[derive(Error, Debug)]
pub enum ParseError {
    /// You used tokens that aren't valid Rust tokens
    ///
    /// (scope graph regular expressions need to both be valid Rust tokens,
    /// as well as having a valid regular expression syntax)
    #[error("lex error: {0}")]
    Lex(#[from] LexError),

    /// Incorrect syntax for regular expressions.
    ///
    /// For correct syntax, see [`parse_regex`]
    #[error("parse error: {0}")]
    Parse(#[from] syn::Error),
}

/// parse a string to a regular expression
///
/// the syntax of a regular expression is:
/// ```bnf
/// E -> 0
/// E -> e
/// E -> <ident>
/// E -> ~ E
/// E -> E *
/// E -> E +
/// E -> E ?
/// E -> E E
/// E -> E | E
/// E -> E & E
/// ```
///
/// The `<ident>` needs to be a valid rust identifier
pub fn parse_regex(input: impl AsRef<str>) -> Result<Regex, ParseError> {
    let stream: TokenStream = input.as_ref().parse()?;
    Ok(parse_regex_token_stream(stream)?)
}

/// parse a rust [`TokenStream`](TokenStream) to a regular expression
///
/// // TODO: figure out to add a link to scopegraphs-macros
/// Implementation detail for [`scopegraphs-macros`]()
#[doc(hidden)]
pub fn parse_regex_token_stream(input: TokenStream) -> syn::Result<Regex> {
    syn::parse2(input)
}
