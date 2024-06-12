use bumpalo::Bump;

/// Holds data for a [`ScopeGraph`](crate::ScopeGraph). Required to construct a `ScopeGraph`.
///
/// A `ScopeGraph` will use the storage object to allocate in,
/// and lifetimes of items in the scope graph will be tied to an instance of `Storage`.
#[derive(Default)]
pub struct Storage(pub Bump);

impl Storage {
    /// Creates a new storage object.
    pub fn new() -> Self {
        Self(Bump::new())
    }
}
