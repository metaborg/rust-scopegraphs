use crate::label::impl_label;
use crate::regex::RegexInput;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod label;
mod regex;

/// Derive [`Label`] implementation.
///
/// ```rust
/// # use std::borrow;
/// use scopegraphs::*;
/// use scopegraphs::label::Label;
///
/// #[derive(Label, Debug, PartialEq, Eq)]
/// pub enum Alphabet {
///     A,
///     B,
///     C,
/// }
/// use Alphabet::*;
///
/// assert_eq!(vec![A, B, C], Alphabet::iter().collect::<Vec<_>>());
/// ```
#[proc_macro_derive(Label)]
pub fn label_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    impl_label(input)
}

/// Syntax: `$attrs type $type<$alphabet_type> = regex`.
/// For example:
///
/// ```rust
/// # use std::borrow;
/// use scopegraphs::*;
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
///
/// # Attributes
/// * `#[graph="$path"]` location to put a graphviz dot file representing the generated finite state machine. (only with the `dot` feature)
#[proc_macro]
pub fn compile_regex(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as RegexInput);
    input.compile()
}
