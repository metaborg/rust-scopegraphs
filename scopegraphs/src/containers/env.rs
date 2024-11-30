use crate::future_wrapper::FutureWrapper;
use crate::resolve::{DataEquivalence, Env, ResolvedPath};
use futures::future::Shared;
use std::hash::Hash;
use std::rc::Rc;

use super::ResolveOrUserError;

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

/* impl<'sg: 'rslv, 'rslv, LABEL: 'sg + Eq, DATA: 'sg + Eq, E: 'rslv>
    Injectable<'sg, 'rslv, LABEL, DATA, Result<bool, E>> for Result<Env<'sg, LABEL, DATA>, E>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Clone,
    E: Clone,
{
    fn inject_if(data_ok: Result<bool, E>, path: ResolvedPath<'sg, LABEL, DATA>) -> Self {
        data_ok.map(|ok| if ok { Env::single(path) } else { Env::empty() })
    }
} */

impl<'sg: 'rslv, 'rslv, LABEL: 'sg + Eq, DATA: 'sg + Eq, RE, UE>
    Injectable<'sg, 'rslv, LABEL, DATA, Result<bool, UE>>
    for Result<Env<'sg, LABEL, DATA>, ResolveOrUserError<RE, UE>>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Clone,
    ResolveOrUserError<RE, UE>: Clone + 'rslv,
{
    fn inject_if(data_ok: Result<bool, UE>, path: ResolvedPath<'sg, LABEL, DATA>) -> Self {
        data_ok
            .map(|ok| if ok { Env::single(path) } else { Env::empty() })
            .map_err(|err| ResolveOrUserError::User(err))
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

// Filtering

/// Sub trait of [EnvContainer] that validates that filtering operations (for shadowing) can be applied on it.
pub trait Filterable<'sg, 'rslv, LABEL: 'sg, DATA: 'sg, DEQO>:
    EnvContainer<'sg, 'rslv, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
{
    /// Implementation of the filter operation on this container.
    fn filter(
        base_env: &Env<'sg, LABEL, DATA>,
        sub_env: &Env<'sg, LABEL, DATA>,
        equiv: &'rslv impl DataEquivalence<'sg, DATA, Output = DEQO>,
    ) -> Self;
}

impl<'sg: 'rslv, 'rslv, LABEL: 'sg, DATA: 'sg, ENVC> Filterable<'sg, 'rslv, LABEL, DATA, bool>
    for ENVC
where
    ENVC: EnvContainer<'sg, 'rslv, LABEL, DATA>,
    Env<'sg, LABEL, DATA>: Clone,
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
{
    fn filter(
        base_env: &Env<'sg, LABEL, DATA>,
        sub_env: &Env<'sg, LABEL, DATA>,
        equiv: &'rslv impl DataEquivalence<'sg, DATA, Output = bool>,
    ) -> Self {
        sub_env
            .iter()
            .filter(|p1| !base_env.iter().any(|p2| equiv.data_equiv(p1.data, p2.data)))
            .cloned()
            .collect::<Env<_, _>>()
            .into()
    }
}

impl<'sg: 'rslv, 'rslv, LABEL: Clone + Eq + 'sg, DATA: Eq + 'sg, RE, UE>
    Filterable<'sg, 'rslv, LABEL, DATA, Result<bool, UE>>
    for Result<Env<'sg, LABEL, DATA>, ResolveOrUserError<RE, UE>>
where
    Env<'sg, LABEL, DATA>: Clone,
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
    ResolveOrUserError<RE, UE>: Clone + 'rslv,
{
    fn filter(
        base_env: &Env<'sg, LABEL, DATA>,
        sub_env: &Env<'sg, LABEL, DATA>,
        equiv: &'rslv impl DataEquivalence<'sg, DATA, Output = Result<bool, UE>>,
    ) -> Self {
        let sub_env = sub_env.clone();
        sub_env.into_iter().try_fold(
            Env::new(),
            |mut filtered_env: Env<'sg, LABEL, DATA>, p1: ResolvedPath<'sg, LABEL, DATA>| {
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
                );
                match shadowed {
                    // p1 is not shadowed, so add it to accumulator
                    Ok(false) => filtered_env.insert(p1),
                    Ok(true) => {} // ignore
                    Err(err) => return Err(ResolveOrUserError::User(err)),
                }

                Ok(filtered_env)
            },
        )
    }
}

impl<'sg: 'rslv, 'rslv, LABEL: Clone + Eq + 'sg, DATA: Eq + 'sg>
    Filterable<'sg, 'rslv, LABEL, DATA, FutureWrapper<'rslv, bool>>
    for FutureWrapper<'rslv, Env<'sg, LABEL, DATA>>
where
    Env<'sg, LABEL, DATA>: Clone,
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
{
    fn filter(
        base_env: &Env<'sg, LABEL, DATA>,
        sub_env: &Env<'sg, LABEL, DATA>,
        equiv: &'rslv impl DataEquivalence<'sg, DATA, Output = FutureWrapper<'rslv, bool>>,
    ) -> Self {
        let base_env = base_env.clone();
        let sub_env = sub_env.clone();
        FutureWrapper::new(async move {
            let mut filtered_env = Env::new();
            'outer: for sub_path in sub_env {
                for base_path in &base_env {
                    if equiv.data_equiv(sub_path.data, base_path.data).await {
                        continue 'outer;
                    }
                }
                filtered_env.insert(sub_path.clone());
            }
            filtered_env
        })
    }
}


#[cfg(test)]
mod test {
    use scopegraphs_macros::{label_order, Label};

    use crate::{completeness::{Delay, ExplicitClose}, containers::ResolveOrUserError, resolve::{DataEquivalence, DataWellformedness, Resolve}, ScopeGraph, Storage};

    pub mod scopegraphs {
        pub use crate::*;
    }


    macro_rules! t {
        ($DWFO: ty, $DEQO: ty, $OUT: ty) => {
            {
            #[derive(Label, Clone, Copy, Debug, PartialEq, Eq, Hash)]
            enum Lbl { }
        
            fn test_dwf<'a>(_ : impl DataWellformedness<'a, u32, Output = $DWFO>) { }
            fn test_equiv<'a>(_ : impl DataEquivalence<'a, u32, Output = $DEQO>) { }
        
            fn some_dwf(_: &u32) -> $DWFO {
                todo!()
            }
        
            fn some_equiv(_: &u32, _: &u32) -> $DEQO {
                todo!()
            }
    
    
            let storage = Storage::new();
            let sg: ScopeGraph<Lbl, u32, _> = ScopeGraph::new(&storage, ExplicitClose::default());
    
            let dwf = some_dwf;
            let equiv = some_equiv;
    
            test_dwf(dwf);
            test_equiv(equiv);
    
            let query: $OUT = sg.query()
                .with_path_wellformedness(query_regex!(Lbl: e))
                .with_data_wellformedness(dwf)
                .with_data_equivalence(equiv)
                // .with_label_order(label_order!(Lbl))
                .resolve(todo!());
        }
    };
    }

    #[test]
    fn test_type() {
        if false {
            t![bool, bool, Result<_, Delay<_>>];
            t![Result<bool, ()>, bool, Result<_, ResolveOrUserError<Delay<_>, ()>>];
            t![bool, Result<bool, ()>, Result<_, ResolveOrUserError<Delay<_>, ()>>];
            t![Result<bool, ()>, Result<bool, ()>, Result<_, ResolveOrUserError<Delay<_>, ()>>];
        }
    }

}
