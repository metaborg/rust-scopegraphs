use crate::completeness::private::Sealed;
use crate::completeness::{Completeness, CriticalEdgeBasedCompleteness, Delay, ExplicitClose};
use crate::future_wrapper::FutureWrapper;
use crate::label::Label;
use crate::{InnerScopeGraph, Scope};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::future::poll_fn;
use std::hash::Hash;
use std::task::{Poll, Waker};

#[derive(Debug)]
pub struct FutureCompleteness<LABEL> {
    explicit_close: ExplicitClose<LABEL>,
    wakers: RefCell<HashMap<Delay<LABEL>, Vec<Waker>>>,
}

impl<LABEL> Default for FutureCompleteness<LABEL> {
    fn default() -> Self {
        Self {
            explicit_close: ExplicitClose::<LABEL>::default(),
            wakers: RefCell::new(HashMap::default()),
        }
    }
}

impl<LABEL> Sealed for FutureCompleteness<LABEL> {}

impl<LABEL: Hash + Eq + Label + Copy, DATA> Completeness<LABEL, DATA>
    for FutureCompleteness<LABEL>
{
    fn cmpl_new_scope(&self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope) {
        self.explicit_close.cmpl_new_scope(inner_scope_graph, scope)
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

    type GetEdgesResult<'rslv> = FutureWrapper<'rslv, Vec<Scope>> where Self: 'rslv, LABEL: 'rslv, DATA: 'rslv;

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

impl<LABEL: Hash + Eq + Copy> FutureCompleteness<LABEL> {
    pub fn close(&self, scope: Scope, label: &LABEL) {
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
