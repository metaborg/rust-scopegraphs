use crate::completeness::private::Sealed;
use crate::completeness::Completeness;
use crate::scopegraph::{InnerScopeGraph, Scope};
use std::hash::Hash;

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
    /// such as [`ImplicitClose`] or [`ExplicitClose`].
    pub unsafe fn new() -> Self {
        Self {}
    }
}

impl<LABEL: Hash + Eq, DATA> Completeness<LABEL, DATA> for UncheckedCompleteness {
    fn cmpl_new_scope(&self, _: &InnerScopeGraph<LABEL, DATA>, _: Scope) {}

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

    type GetEdgesResult<'rslv> = Vec<Scope>
        where
            Self: 'rslv, LABEL: 'rslv, DATA: 'rslv;

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
