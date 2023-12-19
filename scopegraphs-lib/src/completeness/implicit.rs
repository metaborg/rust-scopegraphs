use crate::{
    completeness::private::Sealed,
    completeness::{Completeness, CriticalEdgeBasedCompleteness, CriticalEdgeSet, EdgeClosedError},
    label::Label,
    InnerScopeGraph, Scope,
};
use std::collections::HashSet;
use std::hash::Hash;

/// Critical-edge based [`Completeness`] implementation.
///
/// Unlike [`ExplicitClose`], this implementation will implicitly close edges once traversed.
/// This does not require special attention from the type checker writer.
///
/// Returns [`EdgeClosedError`] when an edge is added to a scope in which the label is already
/// closed (because `get_edges(s, l, ...)` was called earlier.
///
/// When edges are retrieved (e.g. during query resolution) the `(src, label)` edge is closed.
pub struct ImplicitClose<LABEL> {
    critical_edges: CriticalEdgeSet<LABEL>,
}

impl<LABEL> Default for ImplicitClose<LABEL> {
    fn default() -> Self {
        Self {
            critical_edges: Default::default(),
        }
    }
}

impl<LABEL> Sealed for ImplicitClose<LABEL> {}

impl<LABEL: Hash + Eq + Label, DATA> Completeness<LABEL, DATA> for ImplicitClose<LABEL> {
    fn cmpl_new_scope(&mut self, _: &InnerScopeGraph<LABEL, DATA>, _: Scope) {
        <ImplicitClose<LABEL> as CriticalEdgeBasedCompleteness<LABEL, DATA>>::init_scope_with(
            self,
            HashSet::from_iter(LABEL::iter()),
        )
    }

    fn cmpl_new_complete_scope(&mut self, _: &InnerScopeGraph<LABEL, DATA>, _: Scope) {
        <ImplicitClose<LABEL> as CriticalEdgeBasedCompleteness<LABEL, DATA>>::init_scope_with(
            self,
            HashSet::new(),
        )
    }

    type NewEdgeResult = Result<(), EdgeClosedError<LABEL>>;

    // FIXME: identical to `ExplicitClose` impl.
    fn cmpl_new_edge(
        &mut self,
        inner_scope_graph: &mut InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult {
        if self.critical_edges.is_open(src, &lbl) {
            inner_scope_graph.add_edge(src, lbl, dst);
            Ok(())
        } else {
            // FIXME: provide reason (queries) that made this edge closed?
            Err(EdgeClosedError {
                scope: src,
                label: lbl,
            })
        }
    }

    type GetEdgesResult = Vec<Scope>;

    fn cmpl_get_edges(
        &mut self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
    ) -> Self::GetEdgesResult {
        self.critical_edges.close(src, &lbl);
        inner_scope_graph.get_edges(src, lbl)
    }
}

impl<LABEL: Hash + Eq + Label, DATA> CriticalEdgeBasedCompleteness<LABEL, DATA>
    for ImplicitClose<LABEL>
{
    fn init_scope_with(&mut self, open_labels: HashSet<LABEL>) {
        self.critical_edges.init_scope(open_labels)
    }
}