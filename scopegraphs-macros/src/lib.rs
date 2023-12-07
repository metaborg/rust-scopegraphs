use crate::label::impl_label;
use crate::order::OrderInput;
use crate::regex::RegexInput;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod label;
mod order;
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

/// Syntax: `$type: $( $label_order ),*`
/// with
/// `$label_order`: `$( $label_group )<.`
/// `$label_group`: `{ $($variant),* } | $variant`
/// `$variant`: `$ident | '$'`
///
/// For example
/// ```rust
/// # use scopegraphs_macros::label_order;
///
/// #[derive(Copy, Clone)]
/// enum Lbl {
///     Def,
///     Mod,
///     Lex,
///     Imp,
/// }
///
/// label_order!(Lbl: $ < { Def, Mod } < Lex, Imp < Lex);
/// ```
///
/// Here, `$` means the end-of-path label: i.e., the current scope.
///
/// Using `{ ... }`, labels with equal priority can be grouped.
///
/// Multiple partial orders can be combined using `,`-separators: `X < Y, Z < Y`.
///
/// Sequences of multiple `<` can be chained (`X < Y < Z`).
/// This is equivalent to the 2-windowed decomposition: `X < Y, Y < Z` (which, by transitivity, also includes `X < Z`).
///
/// The macro will at compile time validate whether the order is a strict partial order, and report
/// any symmetric or reflexive entries.
#[proc_macro]
pub fn label_order(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as OrderInput);
    input.compile().into()
}
