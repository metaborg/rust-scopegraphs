//! This module contains some generic containers for data used by the resolution algorithm.
//!
//! Using these interfaces, the resolution algorithms can deal with custom behavior introduced
//! by [`Completeness`](crate::completeness::Completeness) implementations.

/// Union of errors during resolution (i.e., delays) and error during predicate evaluation.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum ResolveOrUserError<RE, UE> {
    /// Resolution error.
    Resolve(RE),
    /// User error (predicates)
    User(UE),
}

mod scope;

pub use scope::*;

mod path;
pub use path::*;

mod env;
pub use env::*;
