use crate::{
    completeness::private::Sealed,
    completeness::{
        Completeness, CriticalEdgeBasedCompleteness, CriticalEdgeSet, Delay, EdgeClosedError,
        EdgesOrDelay,
    },
    label::Label,
    InnerScopeGraph, Scope,
};
use std::{collections::HashSet, hash::Hash};

/// Critical-edge based [`Completeness`] implementation.
///
/// Unlike [`ImplicitClose`], this implementation shifts responsibility of closing edges to the
/// _type checker writer_. I.e., they have to insert `sg.close(scope, label)` statements at the
/// appropriate positions in the code.
///
/// Returns [`EdgeClosedError`] when an edge is added to a scope in which the label is already
/// closed (by an explicit close of the type checker writer).
///
/// Returns [`Delay`] when edges are retrieved (e.g. during query resolution) for an edge that is
/// not yet closed.
pub struct ExplicitClose<LABEL> {
    critical_edges: CriticalEdgeSet<LABEL>,
}

impl<LABEL> Default for ExplicitClose<LABEL> {
    fn default() -> Self {
        ExplicitClose {
            critical_edges: CriticalEdgeSet::default(),
        }
    }
}

impl<LABEL> Sealed for ExplicitClose<LABEL> {}

impl<LABEL: Hash + Eq + Label, DATA> Completeness<LABEL, DATA> for ExplicitClose<LABEL> {
    fn cmpl_new_scope(&mut self, _: &InnerScopeGraph<LABEL, DATA>, _: Scope) {
        <ExplicitClose<LABEL> as CriticalEdgeBasedCompleteness<LABEL, DATA>>::init_scope_with(
            self,
            LABEL::iter().collect(), // init with all labels: programmer is responsible for closing edges
        )
    }

    fn cmpl_new_complete_scope(&mut self, _: &InnerScopeGraph<LABEL, DATA>, _: Scope) {
        <ExplicitClose<LABEL> as CriticalEdgeBasedCompleteness<LABEL, DATA>>::init_scope_with(
            self,
            HashSet::new(), // init with empty label set to prevent extension
        )
    }

    type NewEdgeResult = Result<(), EdgeClosedError<LABEL>>;

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
            Err(EdgeClosedError {
                scope: src,
                label: lbl,
            })
        }
    }

    type GetEdgesResult = EdgesOrDelay<Vec<Scope>, LABEL>;

    fn cmpl_get_edges(
        &mut self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
    ) -> Self::GetEdgesResult {
        if self.critical_edges.is_open(src, &lbl) {
            Err(Delay {
                scope: src,
                label: lbl,
            })
        } else {
            Ok(inner_scope_graph.get_edges(src, lbl))
        }
    }
}

impl<LABEL: Hash + Eq + Label, DATA> CriticalEdgeBasedCompleteness<LABEL, DATA>
    for ExplicitClose<LABEL>
{
    fn init_scope_with(&mut self, open_labels: HashSet<LABEL>) {
        self.critical_edges.init_scope(open_labels)
    }
}

impl<LABEL: Hash + Eq> ExplicitClose<LABEL> {
    pub(crate) fn close(&mut self, scope: Scope, label: &LABEL) {
        self.critical_edges.close(scope, label);
    }
}
