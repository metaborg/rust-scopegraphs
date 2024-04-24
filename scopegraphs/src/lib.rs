#![warn(missing_docs)]
//! Scope graphs are an abstraction that allow you to express the complicated
//! name resolution patterns that many programming languages have.
//! Put simply, a scope graph encodes what names are defined in which scopes of
//! a program, and how scopes relate to each other.
//! Then, we can run queries over this graph to create links from usages of names
//! to definitions of names.
//! However, to make name resolution flexible,
//! the building of the graph and the querying over the graph can happen concurrently:
//! we don't need an entire graph before we can start querying it.
//!
//! This library, and its documentation serve as both a kind of reference implementation of scope graphs,
//! a usable library for your programming language,
//! a tutorial of how to use scope graphs
//! and a tutorial of how you could implement scope graphs yourself.
//!
//! ## Research
//!
//! Scope graphs are based on research.
//! These are some papers that introduce the topic in a more scientific fashion than we will here.
//! That is on purpose: The documentation of this library are meant to be the more informal explanation of scope graphs.
//!
//! * [Néron, P., Tolmach, A., Visser, E., & Wachsmuth, G. (2015). A theory of name resolution.](https://web.cecs.pdx.edu/~apt/esop15.pdf)
//!   Containing first introduction of scope graphs.
//! * [van Antwerpen, H., Bach Poulsen, C., Rouvoet, A., & Visser, E. (2018). Scopes as types.](https://repository.tudelft.nl/islandora/object/uuid:9aad733b-23d4-45d7-b52f-331b80c5d029/datastream/OBJ/download)
//!   Presents a refinement of the older scope graphs, which this library is based on.
//! * [Zwaan, A., & van Antwerpen, H. (2023). Scope graphs: The story so far.](https://repository.tudelft.nl/islandora/object/uuid:3024d587-7c5d-44bd-8471-27b7c2e59160/datastream/OBJ/download)
//!   Provides a more detailed overview of all work that involved scope graphs until the date of publication.
//!
//! But more research is ongoing! See our [research overview page]() for an overview of all research that involves scope graphs.
//!
//! ## This Documentation
//!
//! * [Explanation of Concepts used in scope graphs](concepts)
//! * Examples:
//!     * [Standard patterns](patterns)
//! * API Docs (you're there!)
#![cfg_attr(any(RUSTC_IS_NIGHTLY, docsrs), feature(doc_auto_cfg, doc_cfg))]

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
mod containers;
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

/// Define a [label ordering](crate::concepts::label_ordering), an instance of [`LabelOrder`](crate::resolve::LabelOrder)
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
