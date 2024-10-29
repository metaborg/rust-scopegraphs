use crate::future_wrapper::FutureWrapper;
use crate::resolve::{DataEquivalence, Env, ResolvedPath};
use futures::future::Shared;
use std::hash::Hash;
use std::rc::Rc;

/// Interface for environment containers that support the operations required for query resolution.
pub trait EnvContainer<'sg, 'rslv, LABEL: 'sg, DATA: 'sg>:
    From<Env<'sg, LABEL, DATA>> + 'rslv
where
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
{
    /// Creates a new container with an empty environment.
    fn empty() -> Self {
        Self::from(Env::new())
    }

    /// Creates a new container with a single path.
    fn single(path: ResolvedPath<'sg, LABEL, DATA>) -> Self {
        Self::from(Env::single(path))
    }

    /// Maps the current container to a new one, based a provided mapping of the underlying environment.
    fn flat_map(
        &self,
        map: impl 'rslv + for<'short> FnOnce(&'short Env<'sg, LABEL, DATA>) -> Self,
    ) -> Self;
}

impl<'sg: 'rslv, 'rslv, LABEL: Eq, DATA: Eq> EnvContainer<'sg, 'rslv, LABEL, DATA>
    for Env<'sg, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Clone,
{
    fn flat_map(
        &self,
        map: impl 'rslv + for<'short> FnOnce(&'short Env<'sg, LABEL, DATA>) -> Self,
    ) -> Self {
        map(self)
    }
}

impl<'sg: 'rslv, 'rslv, LABEL, DATA> EnvContainer<'sg, 'rslv, LABEL, DATA>
    for Rc<Env<'sg, LABEL, DATA>>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash,
    LABEL: 'sg + Eq + Clone,
    DATA: 'sg + Eq,
{
    fn flat_map(
        &self,
        map: impl for<'short> FnOnce(&'short Env<'sg, LABEL, DATA>) -> Self,
    ) -> Self {
        map(self)
    }
}

impl<'sg, LABEL: 'sg, DATA: 'sg, E> From<Env<'sg, LABEL, DATA>>
    for Result<Env<'sg, LABEL, DATA>, E>
{
    fn from(value: Env<'sg, LABEL, DATA>) -> Self {
        Ok(value)
    }
}

impl<'sg: 'rslv, 'rslv, LABEL: 'sg + Eq, DATA: 'sg + Eq, E: 'rslv>
    EnvContainer<'sg, 'rslv, LABEL, DATA> for Result<Env<'sg, LABEL, DATA>, E>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Clone,
    E: Clone,
{
    fn flat_map(&self, map: impl for<'short> FnOnce(&Env<'sg, LABEL, DATA>) -> Self) -> Self {
        match self {
            Ok(env) => map(env),
            Err(err) => Err(err.clone()),
        }
    }
}

impl<'sg: 'rslv, 'rslv, LABEL, DATA> From<Env<'sg, LABEL, DATA>>
    for FutureWrapper<'rslv, Env<'sg, LABEL, DATA>>
where
    LABEL: Clone,
{
    fn from(value: Env<'sg, LABEL, DATA>) -> Self {
        FutureWrapper::new(std::future::ready(value))
    }
}

impl<'sg: 'rslv, 'rslv, LABEL: 'sg + Eq, DATA: 'sg + Eq> EnvContainer<'sg, 'rslv, LABEL, DATA>
    for FutureWrapper<'rslv, Env<'sg, LABEL, DATA>>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Clone,
    LABEL: Clone,
{
    fn flat_map(
        &self,
        map: impl 'rslv + for<'short> FnOnce(&'short Env<'sg, LABEL, DATA>) -> Self,
    ) -> Self {
        let fut = Shared::clone(&self.0);
        FutureWrapper::new(async move {
            let env = fut.await;
            map(&env).0.await
        })
    }
}

// Injectable

/// Environment Container in which a path can be injected based on a condition.
pub trait Injectable<'sg, 'rslv, LABEL: 'sg, DATA: 'sg, DWFO>:
    EnvContainer<'sg, 'rslv, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
{
    /// Creates a new environment that contains path when `data_ok` is `true`, and is empty otherwise.
    fn inject_if(data_ok: DWFO, path: ResolvedPath<'sg, LABEL, DATA>) -> Self;
}

impl<'sg: 'rslv, 'rslv, LABEL: Eq + 'sg, DATA: Eq + 'sg, ENVC>
    Injectable<'sg, 'rslv, LABEL, DATA, bool> for ENVC
where
    ENVC: EnvContainer<'sg, 'rslv, LABEL, DATA>,
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
{
    fn inject_if(data_ok: bool, path: ResolvedPath<'sg, LABEL, DATA>) -> Self {
        if data_ok {
            Self::single(path)
        } else {
            Self::empty()
        }
    }
}

impl<'sg: 'rslv, 'rslv, LABEL: 'sg + Eq, DATA: 'sg + Eq, E: 'rslv>
    Injectable<'sg, 'rslv, LABEL, DATA, Result<bool, E>> for Result<Env<'sg, LABEL, DATA>, E>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Clone,
    E: Clone,
{
    fn inject_if(data_ok: Result<bool, E>, path: ResolvedPath<'sg, LABEL, DATA>) -> Self {
        data_ok.map(|ok| if ok { Env::single(path) } else { Env::empty() })
    }
}

impl<'sg: 'rslv, 'rslv, LABEL: 'sg + Eq, DATA: 'sg + Eq>
    Injectable<'sg, 'rslv, LABEL, DATA, FutureWrapper<'rslv, bool>>
    for FutureWrapper<'rslv, Env<'sg, LABEL, DATA>>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Clone,
    LABEL: Clone,
{
    fn inject_if(
        data_ok: FutureWrapper<'rslv, bool>,
        path: ResolvedPath<'sg, LABEL, DATA>,
    ) -> Self {
        FutureWrapper::new(async move {
            let ok = data_ok.await;
            if ok {
                Env::single(path)
            } else {
                Env::empty()
            }
        })
    }
}

// Mergeable

/// Environment Container in which a path can be injected based on a condition.
pub trait Mergable<'sg, 'rslv, LABEL: 'sg, DATA: 'sg, DEQO>:
    EnvContainer<'sg, 'rslv, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
{
    /// Creates a new environment that contains path when `data_ok` is `true`, and is empty otherwise.
    fn merge_if(
        &self,
        data_equiv: &'rslv impl DataEquivalence<'sg, DATA, Output = DEQO>,
        sub_env: Self,
    ) -> Self;
}

impl<'sg: 'rslv, 'rslv, LABEL: Eq + 'sg, DATA: Eq + 'sg, ENVC>
    Mergable<'sg, 'rslv, LABEL, DATA, bool> for ENVC
where
    ENVC: EnvContainer<'sg, 'rslv, LABEL, DATA>,
    Env<'sg, LABEL, DATA>: Clone,
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
{
    fn merge_if(
        &self,
        data_equiv: &'rslv impl DataEquivalence<'sg, DATA, Output = bool>,
        sub_env: Self,
    ) -> Self {
        let base_env = self;
        base_env.flat_map(move |base_env| {
            if !base_env.is_empty() && data_equiv.always_equivalent() {
                <ENVC as From<Env<'sg, LABEL, DATA>>>::from(base_env.clone())
            } else {
                let base_env = base_env.clone();

                sub_env.flat_map(move |sub_env| {
                    let filtered_env = sub_env
                        .iter()
                        .filter(|p1| {
                            base_env
                                .iter()
                                .find(|p2| data_equiv.data_equiv(p1.data, p2.data))
                                .is_none()
                        })
                        .collect::<Vec<_>>();

                    let mut new_env = base_env;
                    for path in filtered_env {
                        new_env.insert(path.clone())
                    }
                    new_env.into()
                })
            }
        })
    }
}
