use crate::resolve::{DataEquiv, Env, ResolvedPath};
use std::fmt::Debug;
use std::hash::Hash;

/// Interface for environment containers that support the operations required for query resolution.
pub trait EnvContainer<'sg, LABEL: 'sg, DATA: 'sg>: From<Env<'sg, LABEL, DATA>> {
    /// Creates a new, container with an empty environment.
    fn empty() -> Self;

    /// Lifts a merge operation into this container.
    ///
    /// The resulting container should contain all paths in `self` and all paths in `other`.
    // has functional interface (-> Self) to accommodate Result/Option implementations.
    // e.g., `&mut self` cannot be changed from `Ok(_)` to `Err(_)`
    // implementations that use the interior mutability should return `self`
    fn lift_merge(self, other: Self) -> Self;

    /// Lifts a shadowing operation into this container.
    ///
    /// The resulting container should contain all paths in `self` and all paths in `sub_env`
    /// that are not shadowed by some path in `self` according to the data order.
    fn lift_shadow<DO: DataEquiv<DATA>>(self, sub_env: Self, data_equiv: DO) -> Self;
}

impl<'sg, LABEL, DATA: Debug> EnvContainer<'sg, LABEL, DATA> for Env<'sg, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
{
    fn empty() -> Self {
        Env::default()
    }

    fn lift_merge(mut self, other: Self) -> Self {
        for path in other.into_iter() {
            self.insert(path)
        }
        self
    }

    fn lift_shadow<DO: DataEquiv<DATA>>(mut self, sub_env: Self, data_equiv: DO) -> Self {
        let filtered_env = sub_env
            .into_iter()
            .filter(|p1| {
                if let Some(p2) = self.iter().find(|p2| data_equiv(p1.data, p2.data)) {
                    log::info!(
                        "Discarding {:?} in {:?}; shadowed by {:?} in {:?}",
                        p1.data,
                        p1.path.target(),
                        p2.data,
                        p2.path.target()
                    );
                    false
                } else {
                    true
                }
            })
            .collect::<Vec<_>>();
        for path in filtered_env {
            self.insert(path)
        }
        self
    }
}

impl<'sg, LABEL: 'sg, DATA: 'sg, EC: EnvContainer<'sg, LABEL, DATA>, E> From<Env<'sg, LABEL, DATA>>
    for Result<EC, E>
{
    fn from(value: Env<'sg, LABEL, DATA>) -> Self {
        Ok(EC::from(value))
    }
}

impl<'sg, LABEL: 'sg, DATA: 'sg, EC: EnvContainer<'sg, LABEL, DATA>, E>
    EnvContainer<'sg, LABEL, DATA> for Result<EC, E>
{
    fn empty() -> Self {
        Ok(EC::empty())
    }

    fn lift_merge(self, other: Self) -> Self {
        // Lift the merge if both results are ok.
        // Otherwise, retain the leftmost error value.
        self.and_then(|ec1| other.map(|ec2| ec1.lift_merge(ec2)))
    }

    fn lift_shadow<DO: DataEquiv<DATA>>(self, sub_env: Self, data_equiv: DO) -> Self {
        // Lift the shadow operations if both results are ok.
        // Otherwise, retain the leftmost error value.
        self.and_then(|ec1| sub_env.map(|ec2| ec1.lift_shadow(ec2, data_equiv)))
    }
}
