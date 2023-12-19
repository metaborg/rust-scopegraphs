use crate::completeness::private::Sealed;
use crate::completeness::{Completeness, CriticalEdgeBasedCompleteness, Delay, ExplicitClose};
use crate::label::Label;
use crate::{InnerScopeGraph, Scope};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::future::Future;
use std::hash::Hash;
use std::pin::Pin;
use std::task::{Context, Poll, Waker};

pub enum ResolutionFuture<'fut, T> {
    Ready { value: T },
    PollFn { poll: PollMut<'fut, T> },
}

type PollMut<'fut, T> = Box<dyn FnMut(&Context) -> Poll<T> + 'fut>;

impl<'fut, T> Future for ResolutionFuture<'fut, T>
where
    T: Clone + Unpin,
{
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match Pin::into_inner(self) {
            ResolutionFuture::Ready { value } => Poll::Ready(value.clone()),
            ResolutionFuture::PollFn { poll } => (*poll)(cx),
        }
    }
}

impl<'fut, T> From<T> for ResolutionFuture<'fut, T> {
    fn from(value: T) -> Self {
        ResolutionFuture::Ready { value }
    }
}

impl<'fut, T: Clone> ResolutionFuture<'fut, T> {
    pub(crate) fn flat_map(&'fut mut self, map: &'fut mut (impl FnMut(&T) -> Self + 'fut)) -> Self {
        match self {
            // outer value is ready, return mapping of that value
            ResolutionFuture::Ready { value } => map(value),
            // outer value is not ready, return `Poll` that peeks `self`, and maps it once there is a result.
            ResolutionFuture::PollFn { poll } => {
                // Return new polling future
                ResolutionFuture::PollFn {
                    poll: Box::new(|cx| match (*poll)(cx) {
                        // that when `self` is ready
                        Poll::Ready(value) => {
                            // applies the mapping
                            match map(&value) {
                                // and returns the mapped value when it is ready
                                ResolutionFuture::Ready { value } => Poll::Ready(value.clone()),
                                ResolutionFuture::PollFn { mut poll } => (*poll)(cx),
                            }
                        }
                        // .., or, when `self` is not ready, indicates it is pending
                        Poll::Pending => Poll::Pending,
                    }),
                }
            }
        }
    }
}

pub struct FutureCompleteness<LABEL> {
    explicit_close: ExplicitClose<LABEL>,
    wakers: RefCell<HashMap<Delay<LABEL>, Vec<Waker>>>,
}

impl<LABEL> Sealed for FutureCompleteness<LABEL> {}

impl<LABEL: Hash + Eq + Label + Copy, DATA> Completeness<LABEL, DATA>
    for FutureCompleteness<LABEL>
{
    fn cmpl_new_scope(&self, inner_scope_graph: &mut InnerScopeGraph<LABEL, DATA>, scope: Scope) {
        self.explicit_close.cmpl_new_scope(inner_scope_graph, scope)
    }

    type NewEdgeResult = <ExplicitClose<LABEL> as Completeness<LABEL, DATA>>::NewEdgeResult;

    fn cmpl_new_edge(
        &self,
        inner_scope_graph: &mut InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult {
        self.explicit_close
            .cmpl_new_edge(inner_scope_graph, src, lbl, dst)
    }

    type GetEdgesResult<'fut> = ResolutionFuture<'fut, Vec<Scope>> where DATA: 'fut, LABEL: 'fut, Self: 'fut;

    fn cmpl_get_edges<'fut>(
        &'fut self,
        inner_scope_graph: &'fut InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
    ) -> Self::GetEdgesResult<'fut> {
        match self
            .explicit_close
            .cmpl_get_edges(inner_scope_graph, src, lbl)
        {
            Ok(scopes) => ResolutionFuture::Ready { value: scopes },
            Err(_) => ResolutionFuture::PollFn {
                poll: Box::new(move |cx| {
                    match self
                        .explicit_close
                        .cmpl_get_edges(inner_scope_graph, src, lbl)
                    {
                        Ok(scopes) => Poll::Ready(scopes),
                        Err(delay) => {
                            self.wakers
                                .borrow_mut()
                                .entry(delay)
                                .or_default()
                                .push(cx.waker().clone());
                            Poll::Pending
                        }
                    }
                }),
            },
        }
    }
}

impl<LABEL: Hash + Eq + Copy> FutureCompleteness<LABEL> {
    pub(super) fn close(&mut self, scope: Scope, label: &LABEL) {
        self.explicit_close.close(scope, label);
        for waker in self
            .wakers
            .borrow()
            .get(&Delay {
                scope,
                label: *label,
            })
            .into_iter()
            .flatten()
        {
            waker.wake_by_ref()
        }
    }
}

impl<LABEL: Hash + Eq + Label + Copy, DATA> CriticalEdgeBasedCompleteness<LABEL, DATA>
    for FutureCompleteness<LABEL>
{
    fn init_scope_with(&self, open_edges: HashSet<LABEL>) {
        <ExplicitClose<LABEL> as CriticalEdgeBasedCompleteness<LABEL, DATA>>::init_scope_with(
            &self.explicit_close,
            open_edges,
        );
    }
}
