use std::fmt::Debug;
use std::hash::Hash;

use crate::resolve::Path;
use crate::Scope;
use crate::{future_wrapper::FutureWrapper, resolve::ResolvedPath};

use super::{PathContainer, PathContainerWf};

/// Interface for scope containers that support the operations required for query resolution.
pub trait ScopeContainer<'sg, 'rslv, LABEL: Debug + 'sg, DATA: 'sg>: Debug {
    /// The type containing paths obtained after stepping to this scope.
    type PathContainer;

    /// Lift the [`Path::step`] operation into this container.
    ///
    /// Should retain the contract that for all scopes `s` in `self`, `prefix.step(lbl, s)` is
    /// included in the resulting path container (except cyclic paths).
    fn lift_step(self, lbl: LABEL, prefix: Path<LABEL>) -> Self::PathContainer;
}

/// Trait that is auto-implemented for any [ScopeContainer] implementation that yields a valid [PathContainer].
///
/// This trait is implemented for:
/// - `Vec<Scope>`,
/// - [Result] of scope containers, and
/// - [FutureWrapper] of scope containers.
/// ```
/// # use scopegraphs::containers::ScopeContainerWf;
/// # use scopegraphs::future_wrapper::FutureWrapper;
/// # use scopegraphs::Scope;
/// # use std::fmt::Debug;
/// # use std::hash::Hash;
///
/// # trait LBound<'sg>: Copy + Hash + Eq + Debug + 'sg {}
/// # trait DBound<'sg>: Hash + Eq + 'sg {}
///
/// fn test<'sg, 'rslv, LABEL: LBound<'sg>, DATA: DBound<'sg>, DWFO>(
///     cont: impl ScopeContainerWf<'sg, 'rslv, LABEL, DATA, DWFO>
/// ) { }
///
/// # fn scope_vec<'sg, 'rslv, LABEL: LBound<'sg>, DATA: DBound<'sg>>() {
///     let vec: Vec<Scope> = todo!();
///     test::<'_, '_, LABEL, DATA, bool>(vec);
/// # }
///
/// # fn result<'sg, 'rslv, LABEL: LBound<'sg>, DATA: DBound<'sg>, E: Debug + Clone>() {
///     let result: Result<Vec<Scope>, E> = todo!();
///     test::<'_, '_, LABEL, DATA, bool>(result);
///     test::<'_, '_, LABEL, DATA, Result<bool, E>>(result);
/// # }
///
/// # fn future<'sg, 'rslv, LABEL: LBound<'sg>, DATA: DBound<'sg>>() {
///     let future: FutureWrapper<Vec<Scope>> = todo!();
///     test::<'_, '_, LABEL, DATA, bool>(future);
///     test::<'_, '_, LABEL, DATA, FutureWrapper<'_, bool>>(future);
/// # }
/// ```
///
/// ```no_run
/// # use scopegraphs::containers::ScopeContainerWf;
/// # use scopegraphs::Scope;
/// # use std::fmt::Debug;
/// # use std::hash::Hash;
///
///
/// fn test<'sg, 'rslv, LABEL: Clone + Hash + Eq + Debug + 'sg, DATA: Hash + Eq + 'sg, DWFO>(cont: impl ScopeContainerWf<'sg, 'rslv, LABEL, DATA, DWFO>) {
///
/// }
/// ```
///
/// ```no_run
/// # use scopegraphs::containers::ScopeContainerWf;
/// # use scopegraphs::Scope;
/// # use std::fmt::Debug;
/// # use std::hash::Hash;
///
/// test::<'_, '_, (), (), bool>(Result::<_, ()>::Ok(Vec::<Scope>::new()));
/// test::<'_, '_, (), (), Result<bool, ()>>(Result::<_, ()>::Ok(Vec::<Scope>::new()));
///
/// fn test<'sg, 'rslv, LABEL: Clone + Hash + Eq + Debug + 'sg, DATA: Hash + Eq + 'sg, DWFO>(cont: impl ScopeContainerWf<'sg, 'rslv, LABEL, DATA, DWFO>) {
///
/// }
/// ```
///
pub trait ScopeContainerWf<'sg, 'rslv, LABEL, DATA, DWFO>:
    ScopeContainer<'sg, 'rslv, LABEL, DATA, PathContainer = Self::PathContainerWf>
where
    LABEL: Debug + 'sg,
    DATA: 'sg,
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
{
    /// Refinement of `Self::PathContainer`, carrying proof that this scope container resolves to valid path containers.
    type PathContainerWf: PathContainerWf<'sg, 'rslv, LABEL, DATA, DWFO>;
}

impl<'sg, 'rslv, LABEL, DATA, DWFO, T> ScopeContainerWf<'sg, 'rslv, LABEL, DATA, DWFO> for T
where
    LABEL: Debug + 'sg,
    DATA: 'sg,
    T: ScopeContainer<'sg, 'rslv, LABEL, DATA>,
    Self::PathContainer: PathContainerWf<'sg, 'rslv, LABEL, DATA, DWFO>,
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
{
    type PathContainerWf = Self::PathContainer;
}

impl<'sg: 'rslv, 'rslv, LABEL: Debug + Copy + Eq + Hash + 'sg, DATA: Eq + Hash + 'sg>
    ScopeContainer<'sg, 'rslv, LABEL, DATA> for Vec<Scope>
where
    Vec<Path<LABEL>>: PathContainer<'sg, 'rslv, LABEL, DATA>,
{
    type PathContainer = Vec<Path<LABEL>>;

    fn lift_step(self, lbl: LABEL, prefix: Path<LABEL>) -> Self::PathContainer {
        self.into_iter()
            .filter_map(move |s| prefix.step(lbl, s))
            .collect()
    }
}

impl<
        'sg: 'rslv,
        'rslv,
        LABEL: Debug + Copy + Eq + Hash + 'sg,
        DATA: Eq + Hash + 'sg,
        SC: ScopeContainer<'sg, 'rslv, LABEL, DATA>,
        E: Debug,
    > ScopeContainer<'sg, 'rslv, LABEL, DATA> for Result<SC, E>
where
    Result<SC::PathContainer, E>: PathContainer<'sg, 'rslv, LABEL, DATA>,
{
    type PathContainer = Result<SC::PathContainer, E>;

    fn lift_step(self, lbl: LABEL, prefix: Path<LABEL>) -> Self::PathContainer {
        self.map(|sc| sc.lift_step(lbl, prefix))
    }
}

impl<
        'sg: 'rslv,
        'rslv,
        LABEL: Debug + Copy + Eq + Hash + 'sg,
        DATA: Eq + Hash + 'sg,
        SC: ScopeContainer<'sg, 'rslv, LABEL, DATA> + Clone,
    > ScopeContainer<'sg, 'rslv, LABEL, DATA> for FutureWrapper<'rslv, SC>
where
    LABEL: Copy,
    Self: 'rslv,
    LABEL: 'rslv,
    SC::PathContainer: Clone,
{
    type PathContainer = FutureWrapper<'rslv, SC::PathContainer>;

    fn lift_step(self, lbl: LABEL, prefix: Path<LABEL>) -> Self::PathContainer {
        FutureWrapper::new(async move { self.0.await.lift_step(lbl, prefix.clone()) })
    }
}
