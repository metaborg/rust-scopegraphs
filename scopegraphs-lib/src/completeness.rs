use std::{collections::HashSet, hash::Hash};

use crate::label::Label;

use super::{InnerScopeGraph, Scope};

mod private {
    pub trait Sealed {}
}

use private::Sealed;

/*** Completeness trait ***/

pub trait Completeness<LABEL, DATA>: Sealed {
    fn cmpl_new_scope(&mut self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope);

    fn cmpl_new_complete_scope(
        &mut self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        scope: Scope,
    );

    type NewEdgeResult;
    fn cmpl_new_edge(
        &mut self,
        inner_scope_graph: &mut InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult;

    // type GetDataResult;
    // fn get_data(&mut self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope) -> Self::GetDataResult;

    type GetEdgesResult;
    fn cmpl_get_edges(
        &mut self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
    ) -> Self::GetEdgesResult;
}

/*** Unchecked Completeness Implementation ***/

#[derive(Debug, Default)]
pub struct UncheckedCompleteness {}
impl Sealed for UncheckedCompleteness {}

impl<LABEL: Hash + Eq, DATA> Completeness<LABEL, DATA> for UncheckedCompleteness {
    fn cmpl_new_scope(&mut self, _: &InnerScopeGraph<LABEL, DATA>, _: Scope) {}

    fn cmpl_new_complete_scope(&mut self, _: &InnerScopeGraph<LABEL, DATA>, _: Scope) {}

    type NewEdgeResult = ();

    fn cmpl_new_edge<'a>(
        &mut self,
        inner_scope_graph: &mut InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult {
        inner_scope_graph.add_edge(src, lbl, dst)
    }

    type GetEdgesResult = Vec<Scope>;

    fn cmpl_get_edges(
        &mut self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
    ) -> Self::GetEdgesResult {
        inner_scope_graph.get_edges(src, lbl)
    }
}

/*** Utilities for Weakly-Critical-Edge Based Completeness Checking ***/

struct CriticalEdgeSet<LABEL> {
    open_edges: Vec<HashSet<LABEL>>,
}

impl<LABEL> Default for CriticalEdgeSet<LABEL> {
    fn default() -> Self {
        Self {
            open_edges: Default::default(),
        }
    }
}

impl<LABEL> CriticalEdgeSet<LABEL> {
    fn init_scope(&mut self, edges: HashSet<LABEL>) {
        self.open_edges.push(edges)
    }
}

impl<LABEL: Hash + Eq> CriticalEdgeSet<LABEL> {
    pub fn is_open(&self, scope: Scope, lbl: &LABEL) -> bool {
        self.open_edges[scope.0].contains(lbl)
    }

    fn close(&mut self, scope: Scope, lbl: &LABEL) -> bool {
        self.open_edges[scope.0].remove(lbl)
    }
}

pub struct Witness(pub(super) ()); // Prevent abuse of trait function bt requiring argument that can only be constructed locally.

pub trait CriticalEdgeBasedCompleteness<LABEL, DATA>: Completeness<LABEL, DATA> {
    fn init_scope_with(&mut self, open_edges: HashSet<LABEL>, _witness: Witness);
}

#[derive(Debug)]
pub enum EdgeClosedError<LABEL> {
    EdgeClosed { scope: Scope, label: LABEL },
}

#[derive(Debug)]
pub struct Delay<LABEL> {
    pub scope: Scope,
    pub label: LABEL,
}

pub(crate) type EdgesOrDelay<EDGES, LABEL> = Result<EDGES, Delay<LABEL>>;

/*** Weakly-Critical-Edge Based Completeness Checking with Explicit Closing ***/

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
            HashSet::from_iter(LABEL::iter()),
            Witness(()),
        )
    }

    fn cmpl_new_complete_scope(&mut self, _: &InnerScopeGraph<LABEL, DATA>, _: Scope) {
        <ExplicitClose<LABEL> as CriticalEdgeBasedCompleteness<LABEL, DATA>>::init_scope_with(
            self,
            HashSet::new(),
            Witness(()),
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
            Err(EdgeClosedError::EdgeClosed {
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
    fn init_scope_with(&mut self, open_labels: HashSet<LABEL>, _witness: Witness) {
        self.critical_edges.init_scope(open_labels)
    }
}

impl<LABEL: Hash + Eq> ExplicitClose<LABEL> {
    pub fn close(&mut self, scope: Scope, label: &LABEL) {
        self.critical_edges.close(scope, label);
    }
}

/*** Weakly-Critical-Edge Based Completeness Checking with Implicit Closing ***/

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
            Witness(()),
        )
    }

    fn cmpl_new_complete_scope(&mut self, _: &InnerScopeGraph<LABEL, DATA>, _: Scope) {
        <ImplicitClose<LABEL> as CriticalEdgeBasedCompleteness<LABEL, DATA>>::init_scope_with(
            self,
            HashSet::new(),
            Witness(()),
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
            Err(EdgeClosedError::EdgeClosed {
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
    fn init_scope_with(&mut self, open_labels: HashSet<LABEL>, _witness: Witness) {
        self.critical_edges.init_scope(open_labels)
    }
}

// TODO: Asynchronous Completeness can be a wrapper around the ExplicitClose impl

// TODO: Residual-query-based Completeness requires access to query resolution
