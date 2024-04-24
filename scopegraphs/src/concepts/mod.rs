//! # Concepts of scope graphs
//!
//! To use a scope graph,
//! you need to understand a few basic concepts.
//! First of all, the components that comprise a scope graph:
//!
//! * [A scope](scope)
//! * [A scope's data](scope_data)
//! * [Labelled edges between scopes](edges)
//!
//! Once you know what a scope graph is,
//! we can start talking about running queries over them.
//! For a query to return the desired result, we may need to
//! specify the following properties:
//!
//! * A [Path Well-Formedness](path_wellformedness), dictating what paths are valid for the query to take. Comes in the form of a Regular Expression.
//! * A [Data Well-Formedness](data_wellformedness), dictating which scopes are valid for the query to return. Only if a scope's data satisfies the data well-formedness then a query can return it.
//! * A [Label Ordering](label_ordering), dictating ...
//! * A [Data Equivalence](data_equivalence), dictating ... 
//!
//! To resolve queries over a scope graph, that scope graph should have a notion of [Completeness](completeness)
//!
#![cfg_attr(rustfmt, rustfmt_skip)]

pub mod scope;
pub mod scope_data;
pub mod edges;

pub mod completeness;
pub mod data_wellformedness;
pub mod data_equivalence;
pub mod path_wellformedness;
pub mod label_ordering;
