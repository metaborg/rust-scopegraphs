//! This module contains some generic containers for data used by the resolution algorithm.
//!
//! Using these interfaces, the resolution algorithms can deal with custom behavior introduced
//! by [`Completeness`](crate::completeness::Completeness) implementations.

mod scope;
pub use scope::*;

mod path;
pub use path::*;

mod env;
pub use env::*;
