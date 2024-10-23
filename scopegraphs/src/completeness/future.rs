use crate::completeness::private::Sealed;
use crate::completeness::{Completeness, CriticalEdgeBasedCompleteness, Delay, ExplicitClose};
use crate::future_wrapper::FutureWrapper;
use crate::label::Label;
use crate::scopegraph::{InnerScopeGraph, Scope};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::future::poll_fn;
use std::hash::Hash;
use std::task::{Poll, Waker};

use super::{UserClosed, Witness};

/// A completeness strategy that makes queries return a [`Future`](std::future::Future)
/// in case one of the scopes that the query runs over was not yet closed.
/// The future resolves once all such scopes *are* closed.
///
/// Using [`FutureCompleteness`], you can somewhat delegate the task of scheduling
/// typechecking to one of rust's executors, be it [`tokio`](https://docs.rs/tokio)
/// or [`futures::block_on`](https://docs.rs/futures/latest/futures/executor/fn.block_on.html).
// TODO: add practical lessons from Lace.
///
/// Extends, and contains an instance of, [`ExplicitClose`].
#[derive(Debug)]
pub struct FutureCompleteness<LABEL: Label> {
    explicit_close: ExplicitClose<LABEL>,
    wakers: RefCell<HashMap<Delay<LABEL>, Vec<Waker>>>,
}

impl<LABEL: Label> Default for FutureCompleteness<LABEL> {
    fn default() -> Self {
        Self {
            explicit_close: ExplicitClose::<LABEL>::default(),
            wakers: RefCell::new(HashMap::default()),
        }
    }
}

impl<LABEL: Label> Sealed for FutureCompleteness<LABEL> {}

impl<LABEL: Hash + Label + Copy, DATA> Completeness<LABEL, DATA> for FutureCompleteness<LABEL> {
    fn cmpl_new_scope(&self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope) {
        self.explicit_close.cmpl_new_scope(inner_scope_graph, scope)
    }

    fn cmpl_new_complete_scope(&self, _: &InnerScopeGraph<LABEL, DATA>, _: Scope) {
        <FutureCompleteness<LABEL> as CriticalEdgeBasedCompleteness<LABEL, DATA>>::init_scope_with(
            self,
            HashSet::new(), // init with empty label set to prevent extension
        )
    }

    type NewEdgeResult = <ExplicitClose<LABEL> as Completeness<LABEL, DATA>>::NewEdgeResult;

    fn cmpl_new_edge(
        &self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult {
        self.explicit_close
            .cmpl_new_edge(inner_scope_graph, src, lbl, dst)
    }

    type GetEdgesResult<'rslv>
        = FutureWrapper<'rslv, Vec<Scope>>
    where
        Self: 'rslv,
        LABEL: 'rslv,
        DATA: 'rslv;

    fn cmpl_get_edges<'rslv>(
        &'rslv self,
        inner_scope_graph: &'rslv InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
    ) -> Self::GetEdgesResult<'rslv>
    where
        LABEL: 'rslv,
        DATA: 'rslv,
    {
        FutureWrapper::new(poll_fn(move |cx| {
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
        }))
    }
}

impl<LABEL: Hash + Label + Copy, DATA> CriticalEdgeBasedCompleteness<LABEL, DATA>
    for FutureCompleteness<LABEL>
{
    fn init_scope_with(&self, open_edges: HashSet<LABEL>) {
        <ExplicitClose<LABEL> as CriticalEdgeBasedCompleteness<LABEL, DATA>>::init_scope_with(
            &self.explicit_close,
            open_edges,
        );
    }
}

impl<LABEL: Hash + Label + Copy, DATA> UserClosed<LABEL, DATA> for FutureCompleteness<LABEL> {
    /// Close a scope for a certain label
    /// // TODO: link to "closing" in concepts
    fn close(&self, scope: Scope, label: &LABEL, _witness: Witness) {
        UserClosed::<_, DATA>::close(&self.explicit_close, scope, label, _witness);
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
