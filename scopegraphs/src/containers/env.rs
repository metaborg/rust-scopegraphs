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

// Shadowable

/// Sub trait of [EnvContainer] that validates that shadowin operations can be applied on it.
pub trait Shadowable<'sg, 'rslv, LABEL: 'sg, DATA: 'sg, DEQO>:
    EnvContainer<'sg, 'rslv, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
{
    /// Implementation of the shadow operation on this container.
    fn shadow(
        base_env: Env<'sg, LABEL, DATA>,
        sub_env: &Env<'sg, LABEL, DATA>,
        equiv: &'rslv impl DataEquivalence<'sg, DATA, Output = DEQO>,
    ) -> Self;
}

impl<'sg: 'rslv, 'rslv, LABEL: 'sg, DATA: 'sg, ENVC> Shadowable<'sg, 'rslv, LABEL, DATA, bool>
    for ENVC
where
    ENVC: EnvContainer<'sg, 'rslv, LABEL, DATA>,
    Env<'sg, LABEL, DATA>: Clone,
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
{
    fn shadow(
        base_env: Env<'sg, LABEL, DATA>,
        sub_env: &Env<'sg, LABEL, DATA>,
        equiv: &'rslv impl DataEquivalence<'sg, DATA, Output = bool>,
    ) -> Self {
        let filtered_env = sub_env
            .iter()
            .filter(|p1| !base_env.iter().any(|p2| equiv.data_equiv(p1.data, p2.data)))
            .collect::<Vec<_>>();

        // FIXME: factor out this part?
        let mut new_env = base_env;
        for path in filtered_env {
            new_env.insert(path.clone())
        }
        new_env.into()
    }
}

impl<'sg: 'rslv, 'rslv, LABEL: Clone + Eq + 'sg, DATA: Eq + 'sg, E: Clone + 'rslv>
    Shadowable<'sg, 'rslv, LABEL, DATA, Result<bool, E>> for Result<Env<'sg, LABEL, DATA>, E>
where
    Env<'sg, LABEL, DATA>: Clone,
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
{
    fn shadow(
        base_env: Env<'sg, LABEL, DATA>,
        sub_env: &Env<'sg, LABEL, DATA>,
        equiv: &'rslv impl DataEquivalence<'sg, DATA, Output = Result<bool, E>>,
    ) -> Self {
        let sub_env = sub_env.clone();
        let filtered_env = sub_env.into_iter().try_fold(
            Vec::<ResolvedPath<'sg, LABEL, DATA>>::new(),
            |mut filtered_env: Vec<ResolvedPath<'sg, LABEL, DATA>>,
             p1: ResolvedPath<'sg, LABEL, DATA>| {
                let shadowed = base_env.iter().try_fold(
                    /* initially, not shadowed */ false,
                    |previously_shadowed: bool, p2: &ResolvedPath<'sg, LABEL, DATA>| {
                        if previously_shadowed {
                            Ok(true) // if it was shadowed, it will be
                        } else {
                            // not yet shadowed, try if current path shadows
                            equiv.data_equiv(p1.data, p2.data)
                        }
                    },
                )?;
                // p1 is not shadowed, so add it to accumulator
                if !shadowed {
                    filtered_env.push(p1);
                }

                Ok(filtered_env)
            },
        )?;
        let mut new_env = base_env;
        filtered_env
            .into_iter()
            .for_each(|path| new_env.insert(path));
        new_env.into()
    }
}

impl<'sg: 'rslv, 'rslv, LABEL: Clone + Eq + 'sg, DATA: Eq + 'sg>
    Shadowable<'sg, 'rslv, LABEL, DATA, FutureWrapper<'rslv, bool>>
    for FutureWrapper<'rslv, Env<'sg, LABEL, DATA>>
where
    Env<'sg, LABEL, DATA>: Clone,
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
{
    fn shadow(
        base_env: Env<'sg, LABEL, DATA>,
        sub_env: &Env<'sg, LABEL, DATA>,
        equiv: &'rslv impl DataEquivalence<'sg, DATA, Output = FutureWrapper<'rslv, bool>>,
    ) -> Self {
        let sub_env = sub_env.clone();
        FutureWrapper::new(async move {
            let mut filtered_env: Vec<ResolvedPath<'sg, LABEL, DATA>> = Vec::new();
            'outer: for sub_path in sub_env {
                for base_path in &base_env {
                    if equiv.data_equiv(sub_path.data, base_path.data).await {
                        continue 'outer;
                    }
                }
                filtered_env.push(sub_path.clone());
            }
            let mut new_env = base_env;
            for path in filtered_env {
                new_env.insert(path);
            }
            new_env
        })
    }
}
