use crate::future_wrapper::FutureWrapper;
use crate::resolve::{Env, ResolvedPath};
use futures::future::Shared;
use std::hash::Hash;
use std::rc::Rc;

/// Interface for environment containers that support the operations required for query resolution.
pub trait EnvContainer<'sg, 'rslv, LABEL: 'sg, DATA: 'sg, DWFO>:
    From<Env<'sg, LABEL, DATA>> + 'rslv
{
    /// Creates a new, container with an empty environment.
    fn empty() -> Self;

    /// Creates a new environment that contains path when `data_ok` is `true`, and is empty otherwise.
    fn inject_if(data_ok: DWFO, path: ResolvedPath<'sg, LABEL, DATA>) -> Self;

    /// Maps the current container to a new one, based a provided mapping of the underlying environment.
    fn flat_map(
        &self,
        map: impl 'rslv + for<'short> FnOnce(&'short Env<'sg, LABEL, DATA>) -> Self,
    ) -> Self;
}

impl<'sg: 'rslv, 'rslv, LABEL: Eq, DATA: Eq> EnvContainer<'sg, 'rslv, LABEL, DATA, bool>
    for Env<'sg, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Clone,
{
    fn empty() -> Self {
        Self::new()
    }

    fn inject_if(data_ok: bool, path: ResolvedPath<'sg, LABEL, DATA>) -> Self {
        if data_ok {
            Self::single(path)
        } else {
            Self::empty()
        }
    }

    fn flat_map(
        &self,
        map: impl 'rslv + for<'short> FnOnce(&'short Env<'sg, LABEL, DATA>) -> Self,
    ) -> Self {
        map(self)
    }
}

impl<'sg: 'rslv, 'rslv, LABEL, DATA> EnvContainer<'sg, 'rslv, LABEL, DATA, bool>
    for Rc<Env<'sg, LABEL, DATA>>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash,
    LABEL: 'sg + Eq + Clone,
    DATA: 'sg + Eq,
{
    fn empty() -> Self {
        Self::new(Env::empty())
    }

    fn inject_if(data_ok: bool, path: ResolvedPath<'sg, LABEL, DATA>) -> Self {
        if data_ok {
            Env::single(path).into()
        } else {
            Self::empty()
        }
    }

    fn flat_map(
        &self,
        map: impl for<'short> FnOnce(&'short Env<'sg, LABEL, DATA>) -> Self,
    ) -> Self {
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

impl<'sg: 'rslv, 'rslv, LABEL: 'sg + Eq, DATA: 'sg + Eq, E: 'rslv>
    EnvContainer<'sg, 'rslv, LABEL, DATA, bool> for Result<Env<'sg, LABEL, DATA>, E>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Clone,
    E: Clone,
{
    fn empty() -> Self {
        Ok(Env::empty())
    }

    fn inject_if(data_ok: bool, path: ResolvedPath<'sg, LABEL, DATA>) -> Self {
        if data_ok {
            Env::single(path).into()
        } else {
            Env::empty().into()
        }
    }

    fn flat_map(&self, map: impl for<'short> FnOnce(&Env<'sg, LABEL, DATA>) -> Self) -> Self {
        match self {
            Ok(env) => map(env),
            Err(err) => Err(err.clone()),
        }
    }
}

impl<'sg: 'rslv, 'rslv, LABEL: 'sg + Eq, DATA: 'sg + Eq, E: 'rslv>
    EnvContainer<'sg, 'rslv, LABEL, DATA, Result<bool, E>> for Result<Env<'sg, LABEL, DATA>, E>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Clone,
    E: Clone,
{
    fn empty() -> Self {
        Ok(Env::empty())
    }

    fn inject_if(data_ok: Result<bool, E>, path: ResolvedPath<'sg, LABEL, DATA>) -> Self {
        data_ok.map(|ok| {
            if ok {
                Env::single(path)
            } else {
                Env::empty()
            }
        })
    }

    fn flat_map(&self, map: impl for<'short> FnOnce(&Env<'sg, LABEL, DATA>) -> Self) -> Self {
        match self {
            Ok(env) => map(env),
            Err(err) => Err(err.clone()),
        }
    }
}
impl<'sg: 'rslv, 'rslv, LABEL: 'sg + Eq, DATA: 'sg + Eq> EnvContainer<'sg, 'rslv, LABEL, DATA, bool>
    for FutureWrapper<'rslv, Env<'sg, LABEL, DATA>>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Clone,
    LABEL: Clone,
{
    fn empty() -> Self {
        FutureWrapper::new(std::future::ready(Env::empty()))
    }

    fn inject_if(data_ok: bool, path: ResolvedPath<'sg, LABEL, DATA>) -> Self {
        if data_ok {
            Env::single(path).into()
        } else {
            Env::empty().into()
        }
    }

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

impl<'sg: 'rslv, 'rslv, LABEL: 'sg + Eq, DATA: 'sg + Eq>
    EnvContainer<'sg, 'rslv, LABEL, DATA, FutureWrapper<'rslv, bool>>
    for FutureWrapper<'rslv, Env<'sg, LABEL, DATA>>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Clone,
    LABEL: Clone,
{
    fn empty() -> Self {
        FutureWrapper::new(std::future::ready(Env::empty()))
    }

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

impl<'sg: 'rslv, 'rslv, LABEL, DATA> From<Env<'sg, LABEL, DATA>>
    for FutureWrapper<'rslv, Env<'sg, LABEL, DATA>>
where
    LABEL: Clone,
{
    fn from(value: Env<'sg, LABEL, DATA>) -> Self {
        FutureWrapper::new(std::future::ready(value))
    }
}
