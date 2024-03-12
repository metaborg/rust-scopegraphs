use crate::{
    future_wrapper::FutureWrapper,
    resolve::{Env, Path, ResolvedPath},
};
use futures::future::join_all;
use std::hash::Hash;

/// Interface for path containers that support the operations required for query resolution.
pub trait PathContainer<'sg, 'rslv, LABEL: 'sg, DATA: 'sg>: 'rslv {
    /// Type returned by resolving a path to its sub-environment.
    type EnvContainer<'env>
    where
        'sg: 'env;

    /// Computes sub-environments for each path in the container,
    /// and composes them using the [`crate::containers::EnvContainer::lift_merge`] method.
    fn map_into_env<'env, F: 'rslv + FnMut(Path<LABEL>) -> Self::EnvContainer<'env>>(
        self,
        f: F,
    ) -> Self::EnvContainer<'env>
    where
        'sg: 'env;
}

impl<'rslv, 'sg, LABEL: 'sg, DATA: 'sg> PathContainer<'sg, 'rslv, LABEL, DATA> for Vec<Path<LABEL>>
where
    Self: 'rslv,
    LABEL: Clone + Hash + Eq,
    DATA: Hash + Eq,
{
    type EnvContainer<'env> = Env<'sg, LABEL, DATA> where 'sg: 'env;

    fn map_into_env<'env, F: FnMut(Path<LABEL>) -> Self::EnvContainer<'env>>(
        self,
        f: F,
    ) -> Self::EnvContainer<'env>
    where
        'sg: 'env,
    {
        self.into_iter().map(f).collect()
    }
}

// TODO: can this be generalized to arbitrary results of PathContainers?
// (challenge is converting between the different `::EnvContainer`s.)
impl<'rslv, 'sg, LABEL: 'sg, DATA: 'sg, E: 'rslv> PathContainer<'sg, 'rslv, LABEL, DATA>
    for Result<Vec<Path<LABEL>>, E>
where
    Self: 'rslv,
    LABEL: Clone + Hash + Eq,
    DATA: Hash,
    for<'a> ResolvedPath<'a, LABEL, DATA>: Hash + Eq,
{
    type EnvContainer<'env> = Result<Env<'sg, LABEL, DATA>, E> where 'sg: 'env;

    fn map_into_env<'env, F: FnMut(Path<LABEL>) -> Self::EnvContainer<'env>>(
        self,
        f: F,
    ) -> Self::EnvContainer<'env>
    where
        'sg: 'env,
    {
        self.and_then(|paths| {
            paths
                .into_iter()
                .map(f)
                .collect::<Result<Env<'sg, LABEL, DATA>, E>>()
        })
    }
}
impl<'sg, 'rslv, LABEL: 'sg, DATA: 'sg> PathContainer<'sg, 'rslv, LABEL, DATA>
    for FutureWrapper<'rslv, Vec<Path<LABEL>>>
where
    Self: 'rslv,
    LABEL: Clone + Hash + Eq,
    DATA: Hash,
    for<'a> ResolvedPath<'a, LABEL, DATA>: Hash + Eq,
{
    type EnvContainer<'env> = FutureWrapper<'rslv, Env<'sg, LABEL, DATA>> where 'sg: 'env;

    fn map_into_env<'env, F: 'rslv + FnMut(Path<LABEL>) -> Self::EnvContainer<'env>>(
        self,
        f: F,
    ) -> Self::EnvContainer<'sg>
    where
        'sg: 'env,
    {
        let p_self = Box::pin(self);
        let future = async move {
            let paths = p_self.await;
            let env_futures = paths.into_iter().map(f);
            let envs = join_all(env_futures).await;
            envs.into_iter().collect::<Env<_, _>>()
        };
        FutureWrapper(Box::new(future))
    }
}
