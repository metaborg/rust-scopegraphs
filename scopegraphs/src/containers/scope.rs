use std::hash::Hash;
use std::fmt::Debug;

use crate::future_wrapper::FutureWrapper;
use crate::resolve::Path;
use crate::Scope;

use super::PathContainer;

/// Interface for scope containers that support the operations required for query resolution.
pub trait ScopeContainer<'sg, 'rslv, LABEL: Debug + 'sg, DATA: 'sg, DWFO>: Debug {
    /// The type containing paths obtained after stepping to this scope.
    type PathContainer<DWFO_INNER> : PathContainer<'sg, 'rslv, LABEL, DATA, DWFO>;

    /// Lift the [`Path::step`] operation into this container.
    ///
    /// Should retain the contract that for all scopes `s` in `self`, `prefix.step(lbl, s)` is
    /// included in the resulting path container (except cyclic paths).
    fn lift_step<DWFO_INNER>(self, lbl: LABEL, prefix: Path<LABEL>) -> Self::PathContainer<DWFO_INNER>;
}

impl<'sg: 'rslv, 'rslv, LABEL: Debug + Copy + Eq + Hash + 'sg, DATA: Eq + Hash + 'sg, DWFO> ScopeContainer<'sg, 'rslv, LABEL, DATA, DWFO> for Vec<Scope> 
    where
        Vec<Path<LABEL>>: PathContainer<'sg, 'rslv, LABEL, DATA, DWFO>
{
    type PathContainer = Vec<Path<LABEL>>;

    fn lift_step(self, lbl: LABEL, prefix: Path<LABEL>) -> Self::PathContainer {
        self.into_iter()
            .filter_map(move |s| prefix.step(lbl, s))
            .collect()
    }
}

impl<'sg: 'rslv, 'rslv, LABEL: Debug + Copy + Eq + Hash + 'sg, DATA: Eq + Hash + 'sg, DWFO, SC: ScopeContainer<'sg, 'rslv, LABEL, DATA, DWFO>, E: Debug> ScopeContainer<'sg, 'rslv, LABEL, DATA, DWFO> for Result<SC, E>
    where
        Result<SC::PathContainer, E>: PathContainer<'sg, 'rslv, LABEL, DATA, DWFO>
{
    type PathContainer = Result<SC::PathContainer, E>;

    fn lift_step(self, lbl: LABEL, prefix: Path<LABEL>) -> Self::PathContainer {
        self.map(|sc| sc.lift_step(lbl, prefix))
    }
}

impl<'sg: 'rslv, 'rslv, LABEL: Debug + Copy + Eq + Hash + 'sg, DATA: Eq + Hash + 'sg, DWFO, SC: ScopeContainer<'sg, 'rslv, LABEL, DATA, DWFO> + Clone> ScopeContainer<'sg, 'rslv, LABEL, DATA, DWFO>
    for FutureWrapper<'rslv, SC>
where
    LABEL: Copy,
    Self: 'rslv,
    LABEL: 'rslv,
{
    type PathContainer<DWFO_INNER> = FutureWrapper<'rslv, SC::PathContainer<DWFO_INNER>>
        where
        SC::PathContainer<DWFO_INNER>: PathContainer<'sg, 'rslv, LABEL, DATA, DWFO_INNER> + Clone
        ;

    fn lift_step<DWFO_INNER>(self, lbl: LABEL, prefix: Path<LABEL>) -> Self::PathContainer<DWFO_INNER> {
        FutureWrapper::new(async move { self.0.await.lift_step(lbl, prefix.clone()) })
    }
}
