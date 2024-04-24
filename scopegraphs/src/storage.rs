use bumpalo::Bump;
use std::fmt::{Debug, Formatter};

/// Required to construct a [`ScopeGraph`](crate::ScopeGraph).
/// A `ScopeGraph` will use the storage object to allocate in,
/// and lifetimes of items in the scope graph will be tied to an instance of `Storage`.
#[derive(Default)]
pub struct Storage(pub Bump);

impl Debug for Storage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<scopegraph storage>")
    }
}

impl Storage {
    /// Creates a new storage object.
    pub fn new() -> Self {
        Self(Bump::new())
    }
}
