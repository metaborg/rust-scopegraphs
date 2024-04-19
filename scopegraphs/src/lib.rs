//! A scope graph is an abstraction over name resolution.
//!
//!
//! * [Explanation of Concepts used in ScopeGraphs](crate::_concepts)
//!
#![cfg_attr(any(RUSTC_IS_NIGHTLY, docsrs), feature(doc_auto_cfg, doc_cfg))]

pub use scopegraphs_lib::*;
pub use scopegraphs_macros::*;
pub use scopegraphs_regular_expressions::*;

#[cfg(feature = "unstable-doc")]
pub mod _concepts;
