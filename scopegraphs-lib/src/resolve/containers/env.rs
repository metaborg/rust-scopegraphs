use crate::resolve::{Env, ResolvedPath};
use std::hash::Hash;

/// Interface for environment containers that support the operations required for query resolution.
pub trait EnvContainer<'sg, LABEL: 'sg, DATA: 'sg>: From<Env<'sg, LABEL, DATA>> {
    /// Creates a new, container with an empty environment.
    fn empty() -> Self;

    /// Maps the current container to a new one, based a provided mapping of the underlying environment.
    fn flat_map(self, map: impl FnOnce(Env<'sg, LABEL, DATA>) -> Self) -> Self;
}

impl<'sg, LABEL, DATA> EnvContainer<'sg, LABEL, DATA> for Env<'sg, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
{
    fn empty() -> Self {
        Self::new()
    }

    fn flat_map(self, map: impl FnOnce(Env<'sg, LABEL, DATA>) -> Self) -> Self {
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
{
    fn empty() -> Self {
        Ok(Env::empty())
    }

    fn flat_map(self, map: impl FnOnce(Env<'sg, LABEL, DATA>) -> Self) -> Self {
        self.and_then(map)
    }
}

/*


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

    fn lift_shadow<DEq: DataEquiv<DATA>>(mut self, sub_env: ThunkOf<Self>, data_equiv: &DEq) -> Self {
        let filtered_env = sub_env
            .into_iter()
            .filter(|p1| {
                if let Some(p2) = self
                    .iter()
                    .find(|p2| data_equiv.data_equiv(p1.data, p2.data))
                {
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

    fn lift_shadow<DEq: DataEquiv<DATA>>(self, sub_env: ThunkOf<Self>, data_equiv: &DEq) -> Self {
        // Lift the shadow operations if both results are ok.
        // Otherwise, retain the leftmost error value.
        self.and_then(|ec1| sub_env.map(|ec2| ec1.lift_shadow(ec2, data_equiv)))
    }
}

 */
