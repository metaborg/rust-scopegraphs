use bumpalo::Bump;
use std::fmt::{Debug, Formatter};

#[derive(Default)]
pub struct Storage(pub(crate) Bump);

impl Debug for Storage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<scopegraph storage>")
    }
}

impl Storage {
    pub fn new() -> Self {
        Self(Bump::new())
    }
}
