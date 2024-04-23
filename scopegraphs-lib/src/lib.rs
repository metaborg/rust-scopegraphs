#![warn(missing_docs)]

pub mod completeness;
pub mod containers;
pub mod future_wrapper;
pub mod label;
pub mod resolve;
mod scopegraph;
pub mod storage;

pub use label::Label;
pub use scopegraph::*;
pub use scopegraphs_macros;

#[cfg(feature = "dot")]
pub mod render;
