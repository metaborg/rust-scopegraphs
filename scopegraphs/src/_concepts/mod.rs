#![cfg_attr(rustfmt, rustfmt_skip)]
//! # Concepts of scope graphs

/// What do we mean by a scope?
pub mod scope;
/// Scopes can have data
pub mod scope_data;
/// Scopes can be connected by edges
pub mod edges;
/// And edges can have labels
pub mod labels;
/// All together, forming a scope graph
pub mod scope_graph;
