use crate::{future_wrapper::FutureWrapper, resolve::{Env, Path, ResolvedPath}};
use std::{future::{poll_fn, Future}, hash::Hash, task::Poll};
use futures::future::join_all;

/// Interface for path containers that support the operations required for query resolution.
pub trait PathContainer<LABEL, DATA> {
    /// Type returned by resolving a path to its sub-environment.
    type EnvContainer<'sg>
    where
        DATA: 'sg,
        LABEL: 'sg;

    /// Computes sub-environments for each path in the container,
    /// and composes them using the [`crate::containers::EnvContainer::lift_merge`] method.
    fn map_into_env<'sg, F: FnMut(Path<LABEL>) -> Self::EnvContainer<'sg>>(
        self,
        f: F,
    ) -> Self::EnvContainer<'sg>;
}

impl<LABEL, DATA> PathContainer<LABEL, DATA> for Vec<Path<LABEL>>
where
    LABEL: Clone + Hash + Eq,
    DATA: Hash + Eq,
{
    type EnvContainer<'sg> = Env<'sg, LABEL, DATA> where LABEL: 'sg, DATA: 'sg;

    fn map_into_env<'sg, F: FnMut(Path<LABEL>) -> Self::EnvContainer<'sg>>(
        self,
        f: F,
    ) -> Self::EnvContainer<'sg> {
        self.into_iter().map(f).collect()
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
        self.and_then(|paths| {
            paths
                .into_iter()
                .map(f)
                .collect::<Result<Env<'sg, LABEL, DATA>, E>>()
        })
    }
}
impl<'fut, LABEL, DATA> PathContainer<LABEL, DATA> for FutureWrapper<'fut, Vec<Path<LABEL>>>
where
    LABEL: Clone + Hash + Eq,
    DATA: Hash,
    for<'a> ResolvedPath<'a, LABEL, DATA>: Hash + Eq,
{
    type EnvContainer<'sg> = FutureWrapper<'fut, Env<'sg, LABEL, DATA>> where DATA: 'sg, LABEL: 'sg;

    fn map_into_env<'sg, F: FnMut(Path<LABEL>) -> Self::EnvContainer<'sg>>(
        self,
        f: F,
    ) -> Self::EnvContainer<'sg> {
        let p_self = Box::pin(self);
        let future = async move {
            let paths = p_self.await.clone();
            let env_futures = paths.into_iter().map(f);
            let envs = join_all(env_futures).await;
            envs.into_iter().collect::<Env<_, _>>()
        };
        FutureWrapper(Box::new(future))
    }
}



