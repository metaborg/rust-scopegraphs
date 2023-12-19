use crate::{completeness::Completeness, Scope};
use std::{collections::HashSet, hash::Hash};

pub(super) struct CriticalEdgeSet<LABEL> {
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
    pub(super) fn init_scope(&mut self, edges: HashSet<LABEL>) {
        self.open_edges.push(edges)
    }
}

impl<LABEL: Hash + Eq> CriticalEdgeSet<LABEL> {
    pub fn is_open(&self, scope: Scope, lbl: &LABEL) -> bool {
        self.open_edges[scope.0].contains(lbl)
    }

    pub(super) fn close(&mut self, scope: Scope, lbl: &LABEL) -> bool {
        self.open_edges[scope.0].remove(lbl)
    }
}

/// Sub-trait of [`Completeness`] that uses _critical edges_ (scope-label pairs) to manage completeness.
///
/// Provides utility function to create scopes with particular open edges.
///
/// Should not be called externally, but only from utility function on [`super::ScopeGraph`].
pub trait CriticalEdgeBasedCompleteness<LABEL, DATA>: Completeness<LABEL, DATA> {
    fn init_scope_with(&mut self, open_edges: HashSet<LABEL>);
}

/// Error returned when attempting to add an edge with a label that is already closed in that scope.
#[derive(Debug)]
pub struct EdgeClosedError<LABEL> {
    pub scope: Scope,
    pub label: LABEL,
}

/// Value returned when a query cannot yet be computed because some edge it depends on is still closed.
#[derive(Debug, Copy, Clone)]
pub struct Delay<LABEL> {
    pub scope: Scope,
    pub label: LABEL,
}

pub(crate) type EdgesOrDelay<EDGES, LABEL> = Result<EDGES, Delay<LABEL>>;
