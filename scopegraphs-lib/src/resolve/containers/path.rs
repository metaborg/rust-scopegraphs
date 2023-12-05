use crate::resolve::{Env, Path, ResolvedPath};
use std::hash::Hash;

pub trait PathContainer<LABEL, DATA> {
    type EnvContainer<'sg>
    where
        DATA: 'sg,
        LABEL: 'sg;

    /// Computes sub-environments for each path in the container,
    /// and composes them using the [`crate::resolve::containers::EnvContainer::lift_merge`] method.
    fn map_into_env<'sg, F: FnMut(Path<LABEL>) -> Self::EnvContainer<'sg>>(
        self,
        f: F,
    ) -> Self::EnvContainer<'sg>;
}

pub fn flat_map_iterator<
    'sg,
    LABEL: 'sg,
    DATA: 'sg,
    F: FnMut(Path<LABEL>) -> Env<'sg, LABEL, DATA>,
>(
    iter: impl Iterator<Item = Path<LABEL>>,
    f: F,
) -> Env<'sg, LABEL, DATA>
where
    LABEL: Clone + Hash + Eq,
    for<'a> ResolvedPath<'a, LABEL, DATA>: Hash + Eq,
{
    let mut env = Env::new();
    for sub_env in iter.map(f) {
        env.merge(sub_env)
    }
    env
}

impl<LABEL, DATA> PathContainer<LABEL, DATA> for Vec<Path<LABEL>>
where
    LABEL: Clone + Hash + Eq,
    for<'a> ResolvedPath<'a, LABEL, DATA>: Hash + Eq,
{
    type EnvContainer<'sg> = Env<'sg, LABEL, DATA> where LABEL: 'sg, DATA: 'sg;

    fn map_into_env<'sg, F: FnMut(Path<LABEL>) -> Self::EnvContainer<'sg>>(
        self,
        f: F,
    ) -> Self::EnvContainer<'sg> {
        flat_map_iterator(self.into_iter(), f)
    }
}

// TODO: can this be generalized to arbitrary results of PathContainers?
// (challenge is converting between the different `::EnvContainer`s.)
impl<LABEL, DATA, E> PathContainer<LABEL, DATA> for Result<Vec<Path<LABEL>>, E>
where
    LABEL: Clone + Hash + Eq,
    DATA: Hash,
    for<'a> ResolvedPath<'a, LABEL, DATA>: Hash + Eq,
{
    type EnvContainer<'sg> = Result<Env<'sg, LABEL, DATA>, E> where DATA: 'sg, LABEL: 'sg;

    fn map_into_env<'sg, F: FnMut(Path<LABEL>) -> Self::EnvContainer<'sg>>(
        self,
        f: F,
    ) -> Self::EnvContainer<'sg> {
        match self {
            Ok(paths) => paths
                .into_iter()
                .map(f)
                .collect::<Result<Env<'sg, LABEL, DATA>, E>>(),
            Err(err) => Err(err),
        }
    }
}
