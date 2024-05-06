#![warn(missing_docs)]
#![doc = include_str!("../README.md")]
//! ## This Documentation
//!
//! * [Explanation of Concepts used in scope graphs](concepts)
//! * Examples:
//!     * [Standard patterns](patterns)
//! * [API Docs](https://docs.rs/scopegraphs)
#![cfg_attr(docsrs, feature(doc_auto_cfg, doc_cfg))]
#![cfg_attr(not(docsrs), allow(rustdoc::broken_intra_doc_links))]
#![allow(unknown_lints)]
#![allow(unexpected_cfgs)]
#![allow(clippy::empty_docs)]

#[cfg(feature = "documentation")]
pub mod concepts;

#[cfg(feature = "documentation")]
pub mod patterns;

#[macro_use]
mod label;
pub use label::Label;

#[cfg(feature = "dot")]
mod render;

pub use scopegraphs_regular_expressions::*;

pub mod completeness;
pub mod containers;
mod future_wrapper;

pub mod resolve;

mod scopegraph;
pub use scopegraph::{Scope, ScopeGraph};

mod storage;
pub use storage::Storage;

/// Derive [`Label`] implementation.
///
/// ```rust
/// # use std::borrow;
/// use scopegraphs::*;
/// use scopegraphs::Label;
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
pub use scopegraphs_macros::Label;

/// Compile a regular expression into Rust code.
///
/// Generates a struct implementing [`RegexMatcher`].
/// Instances of this struct can match the regular expression that is specified.
///
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
/// # Supported Attributes
/// * `#[graph="$path"]` location to put a graphviz dot file representing the generated finite state machine. (only with the `dot` feature)
///
/// # Query Directly
///
/// Instead of using [`compile_regex`], you can also use [`query_regex`], which has a simpler syntax
/// and can immediately be used to match a string. This is useful if you want to use a regular expression
/// only once.
/// ```
/// # use std::borrow;
/// # use scopegraphs::*;
/// #
/// # pub enum Alphabet {
/// #    A,
/// #    B,
/// #    C,
/// # }
/// # use Alphabet::*;
/// use scopegraphs::query_regex;
/// assert!(query_regex!(Alphabet: A* B).accepts([A, B]));
/// ```
pub use scopegraphs_macros::compile_regex;

/// Define a [label ordering](crate::concepts::label_ordering), an implementation of [`LabelOrder`](crate::resolve::LabelOrder)
///
/// Syntax:
/// ```grammar
/// $type: $( $label_order ),*
/// ```
/// with
/// ```grammar
/// $label_order: $( $label_group )<*
/// $label_group: { $($variant),* } | $variant
/// $variant: $ident | '$'
/// ```
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
pub use scopegraphs_macros::label_order;
