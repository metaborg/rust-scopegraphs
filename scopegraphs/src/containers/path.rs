use crate::future_wrapper::FutureWrapper;
use crate::resolve::{DataEquivalence, Env, Path, ResolvedPath};
use futures::future::join_all;
use std::hash::Hash;
use std::fmt::Debug;

use super::EnvContainer;

/// Interface for path containers that support the operations required for query resolution.
pub trait PathContainer<'sg, 'rslv, LABEL: 'sg, DATA: 'sg, DWFO>: Debug + 'rslv 
{
    /// Type returned by resolving a path to its sub-environment.
    type EnvContainer: EnvContainer<'sg, 'rslv, LABEL, DATA, DWFO>;

    /// Computes sub-environments for each path in the container.
    fn map_into_env<F: 'rslv + FnMut(Path<LABEL>) -> Self::EnvContainer>(
        self,
        f: F,
    ) -> Self::EnvContainer;
}

impl<'rslv, 'sg, LABEL: Debug + 'sg, DATA: 'sg, DWFO> PathContainer<'sg, 'rslv, LABEL, DATA, DWFO> for Vec<Path<LABEL>>
where
    Self: 'rslv,
    LABEL: Clone + Hash + Eq,
    DATA: Hash + Eq,
    Env<'sg, LABEL, DATA>: EnvContainer<'sg, 'rslv, LABEL, DATA, DWFO>
{
    type EnvContainer = Env<'sg, LABEL, DATA>;

    fn map_into_env<F: FnMut(Path<LABEL>) -> Self::EnvContainer>(self, f: F) -> Self::EnvContainer {
        self.into_iter().map(f).collect()
    }
}

// TODO: can this be generalized to arbitrary results of PathContainers?
// (challenge is converting between the different `::EnvContainer`s.)
impl<'rslv, 'sg, LABEL: Debug + 'sg, DATA: 'sg, E: Debug + 'rslv, DWFO> PathContainer<'sg, 'rslv, LABEL, DATA, DWFO>
    for Result<Vec<Path<LABEL>>, E>
where
    Self: 'rslv,
    LABEL: Clone + Hash,
    DATA: Hash,
    for<'a> ResolvedPath<'a, LABEL, DATA>: Hash + Eq,
    Result<Env<'sg, LABEL, DATA>, E>: EnvContainer<'sg, 'rslv, LABEL, DATA, DWFO>
{
    type EnvContainer = Result<Env<'sg, LABEL, DATA>, E>;

    fn map_into_env<F: FnMut(Path<LABEL>) -> Self::EnvContainer>(self, f: F) -> Self::EnvContainer {
        self.and_then(|paths| {
            paths
                .into_iter()
                .map(f)
                .collect::<Result<Env<'sg, LABEL, DATA>, E>>()
        })
    }
}
impl<'sg, 'rslv, LABEL: 'sg, DATA: 'sg, DWFO> PathContainer<'sg, 'rslv, LABEL, DATA, DWFO>
    for FutureWrapper<'rslv, Vec<Path<LABEL>>>
where
    Self: 'rslv,
    LABEL: Clone + Hash,
    DATA: Hash,
    FutureWrapper<'rslv, Env<'sg, LABEL, DATA>>: EnvContainer<'sg, 'rslv, LABEL, DATA, DWFO>,
    for<'a> ResolvedPath<'a, LABEL, DATA>: Hash + Eq,
{
    type EnvContainer = FutureWrapper<'rslv, Env<'sg, LABEL, DATA>>;

    fn map_into_env<F: 'rslv + FnMut(Path<LABEL>) -> Self::EnvContainer>(
        self,
        f: F,
    ) -> Self::EnvContainer {
        let future = async move {
            let paths = self.0.await;
            let env_futures = paths.into_iter().map(f).map(|i| i.0);
            let envs = join_all(env_futures).await;
            envs.into_iter().collect::<Env<_, _>>()
        };
        FutureWrapper::new(future)
    }
}
