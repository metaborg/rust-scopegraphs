use crate::resolve::{Env, ResolvedPath};
use std::hash::Hash;
use std::rc::Rc;

/// Interface for environment containers that support the operations required for query resolution.
pub trait EnvContainer<'sg, LABEL: 'sg, DATA: 'sg>: From<Env<'sg, LABEL, DATA>> {
    /// Creates a new, container with an empty environment.
    fn empty() -> Self;

    /// Maps the current container to a new one, based a provided mapping of the underlying environment.
    fn flat_map(&self, map: impl FnOnce(&Env<'sg, LABEL, DATA>) -> Self) -> Self;
}

impl<'sg, LABEL, DATA> EnvContainer<'sg, LABEL, DATA> for Env<'sg, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
{
    fn empty() -> Self {
        Self::new()
    }

    fn flat_map(&self, map: impl FnOnce(&Env<'sg, LABEL, DATA>) -> Self) -> Self {
        map(self)
    }
}

impl<'sg, LABEL, DATA> EnvContainer<'sg, LABEL, DATA> for Rc<Env<'sg, LABEL, DATA>>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
{
    fn empty() -> Self {
        Self::new(Env::empty())
    }

    fn flat_map(&self, map: impl FnOnce(&Env<'sg, LABEL, DATA>) -> Self) -> Self {
        map(self)
    }
}

// Implementations for Results
impl<'sg, LABEL: 'sg, DATA: 'sg, E> From<Env<'sg, LABEL, DATA>>
    for Result<Env<'sg, LABEL, DATA>, E>
{
    fn from(value: Env<'sg, LABEL, DATA>) -> Self {
        Ok(value)
    }
}

impl<'sg, LABEL: 'sg, DATA: 'sg, E> EnvContainer<'sg, LABEL, DATA>
    for Result<Env<'sg, LABEL, DATA>, E>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
    E: Clone,
{
    fn empty() -> Self {
        Ok(Env::empty())
    }

    fn flat_map(&self, map: impl FnOnce(&Env<'sg, LABEL, DATA>) -> Self) -> Self {
        match self {
            Ok(env) => map(env),
            Err(err) => Err(err.clone()),
        }
    }
}
