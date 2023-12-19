use crate::{completeness::private::Sealed, completeness::Completeness, InnerScopeGraph, Scope};
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
    fn cmpl_new_scope(&self, _: &mut InnerScopeGraph<LABEL, DATA>, _: Scope) {}

    type NewEdgeResult = ();

    fn cmpl_new_edge(
        &self,
        inner_scope_graph: &mut InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult {
        inner_scope_graph.add_edge(src, lbl, dst)
    }

    type GetEdgesResult<'a> = Vec<Scope>
        where
            DATA: 'a,
            LABEL: 'a;

    fn cmpl_get_edges<'a>(
        &'a self,
        inner_scope_graph: &'a InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
    ) -> Self::GetEdgesResult<'a> {
        inner_scope_graph.get_edges(src, lbl)
    }
}
