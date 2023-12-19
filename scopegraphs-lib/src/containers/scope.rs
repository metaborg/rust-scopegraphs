use std::future::{poll_fn, Future};
use std::pin::pin;
use std::task::Poll;
// use crate::resolution_future::ResolutionFuture;
use crate::future_wrapper::FutureWrapper;
use crate::{resolve::Path, Scope};

/// Interface for scope containers that support the operations required for query resolution.
pub trait ScopeContainer<'a, LABEL> {
    /// The type containing paths obtained after stepping to this scope.
    type PathContainer;

    /// Lift the [`Path::step`] operation into this container.
    ///
    /// Should retain the contract that for all scopes `s` in `self`, `prefix.step(lbl, s)` is
    /// included in the resulting path container (except cyclic paths).
    fn lift_step(&'a mut self, lbl: LABEL, prefix: &Path<LABEL>) -> Self::PathContainer;
}

impl<'a, LABEL: Copy> ScopeContainer<'a, LABEL> for Vec<Scope> {
    type PathContainer = Vec<Path<LABEL>>;

    fn lift_step(&'a mut self, lbl: LABEL, prefix: &Path<LABEL>) -> Self::PathContainer {
        self.into_iter()
            .filter_map(move |s| prefix.step(lbl, *s))
            .collect()
    }
}

impl<'a, LABEL, SC: ScopeContainer<'a, LABEL>, E> ScopeContainer<'a, LABEL> for Result<SC, E> {
    type PathContainer = Result<SC::PathContainer, E>;

    fn lift_step(&'a mut self, lbl: LABEL, prefix: &Path<LABEL>) -> Self::PathContainer {
        self.map(|mut sc| sc.lift_step(lbl, prefix))
    }
}

impl<'fut, LABEL, SC: ScopeContainer<'fut, LABEL> + Clone> ScopeContainer<'fut, LABEL>
    for FutureWrapper<'fut, SC>
where
    LABEL: Copy,
    SC: Unpin,
    SC::PathContainer: Clone + Unpin,
    Self: 'fut,
{
    type PathContainer = FutureWrapper<'fut, SC::PathContainer>;

    fn lift_step(&'fut mut self, lbl: LABEL, prefix: &Path<LABEL>) -> Self::PathContainer {
        let mut p_self = pin!(self);
        let fut = poll_fn(move |cx| match p_self.as_mut().poll(cx) {
            Poll::Ready(mut inner_sc) => Poll::Ready(inner_sc.lift_step(lbl, prefix)),
            Poll::Pending => Poll::Pending,
        });
        FutureWrapper(Box::new(fut))
    }
}
