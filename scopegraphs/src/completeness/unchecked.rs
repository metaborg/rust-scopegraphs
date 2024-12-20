use crate::completeness::private::Sealed;
use crate::completeness::Completeness;
use crate::scopegraph::{InnerScopeGraph, Scope};
use crate::Label;

use super::Implicit;

/// No-Op implementation of [`Completeness`].
#[derive(Debug)]
pub struct UncheckedCompleteness {}

impl Sealed for UncheckedCompleteness {}

impl UncheckedCompleteness {
    /// Constructs a new instance of [`UncheckedCompleteness`].
    ///
    /// # Safety
    ///
    /// Marked as `unsafe`, as it does adhere to its contract (guaranteeing stability).
    ///
    /// Unless you are sure you really need this, consider alternatives
    /// such as [`ImplicitClose`](crate::completeness::ImplicitClose) or [`ExplicitClose`](crate::completeness::ExplicitClose).
    pub unsafe fn new() -> Self {
        Self {}
    }
}

impl<LABEL: Label, DATA> Completeness<LABEL, DATA> for UncheckedCompleteness {
    fn cmpl_new_scope(&self, _: &InnerScopeGraph<LABEL, DATA>, _: Scope) {}

    fn cmpl_new_complete_scope(
        &self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        scope: Scope,
    ) {
        self.cmpl_new_scope(inner_scope_graph, scope)
    }

    type NewEdgeResult = ();

    fn cmpl_new_edge(
        &self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult {
        inner_scope_graph.add_edge(src, lbl, dst)
    }

    type GetEdgesResult<'rslv>
        = Vec<Scope>
    where
        Self: 'rslv,
        LABEL: 'rslv,
        DATA: 'rslv;

    fn cmpl_get_edges<'rslv>(
        &self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
    ) -> Self::GetEdgesResult<'rslv>
    where
        LABEL: 'rslv,
        DATA: 'rslv,
    {
        inner_scope_graph.get_edges(src, lbl)
    }
}

impl<LABEL: Label, DATA> Implicit<LABEL, DATA> for UncheckedCompleteness {}
