use crate::{completeness::Completeness, Scope, ScopeGraph};
use std::cell::RefCell;
use std::{collections::HashSet, hash::Hash};

#[derive(Debug)]
pub(super) struct CriticalEdgeSet<LABEL> {
    open_edges: RefCell<Vec<HashSet<LABEL>>>,
}

impl<LABEL> Default for CriticalEdgeSet<LABEL> {
    fn default() -> Self {
        Self {
            open_edges: Default::default(),
        }
    }
}

impl<LABEL> CriticalEdgeSet<LABEL> {
    pub(super) fn init_scope(&self, edges: HashSet<LABEL>) {
        self.open_edges.borrow_mut().push(edges)
    }
}

impl<LABEL: Hash + Eq> CriticalEdgeSet<LABEL> {
    pub fn is_open(&self, scope: Scope, lbl: &LABEL) -> bool {
        self.open_edges.borrow()[scope.0].contains(lbl)
    }

    pub(super) fn close(&self, scope: Scope, lbl: &LABEL) -> bool {
        self.open_edges.borrow_mut()[scope.0].remove(lbl)
    }
}

/// Sub-trait of [`Completeness`] that uses _critical edges_ (scope-label pairs) to manage completeness.
///
/// Provides utility function to create scopes with particular open edges.
///
/// Should not be called externally, but only from utility function on [`super::ScopeGraph`].
pub trait CriticalEdgeBasedCompleteness<LABEL, DATA>: Completeness<LABEL, DATA> {
    fn init_scope_with(&self, open_edges: HashSet<LABEL>);
}

/// Error returned when attempting to add an edge with a label that is already closed in that scope.
#[derive(Debug)]
pub struct EdgeClosedError<LABEL> {
    pub scope: Scope,
    pub label: LABEL,
}

/// Value returned when a query cannot yet be computed because some edge it depends on is still closed.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Delay<LABEL> {
    pub scope: Scope,
    pub label: LABEL,
}

pub(crate) type EdgesOrDelay<EDGES, LABEL> = Result<EDGES, Delay<LABEL>>;

impl<'sg, LABEL: Hash + Eq, DATA, CMPL> ScopeGraph<'sg, LABEL, DATA, CMPL>
where
    CMPL: CriticalEdgeBasedCompleteness<LABEL, DATA>,
{
    /// Adds a new scope with some open edges.
    pub fn add_scope_with<I>(&mut self, data: DATA, open_edges: I) -> Scope
    where
        I: IntoIterator<Item = LABEL>,
    {
        let scope = self.inner_scope_graph.add_scope(data);
        self.completeness
            .init_scope_with(open_edges.into_iter().collect());
        scope
    }

    /// Adds a new scope with no open edges.
    pub fn add_scope_closed(&mut self, data: DATA) -> Scope {
        let scope = self.inner_scope_graph.add_scope(data);
        self.completeness.init_scope_with(HashSet::new());
        scope
    }
}

impl<'sg, LABEL: Hash + Eq, DATA, CMPL> ScopeGraph<'sg, LABEL, DATA, CMPL>
where
    DATA: Default,
    CMPL: CriticalEdgeBasedCompleteness<LABEL, DATA>,
{
    /// Adds a new scope with some open edges and default data.
    pub fn add_scope_default_with<I>(&mut self, open_edges: I) -> Scope
    where
        I: IntoIterator<Item = LABEL>,
    {
        self.add_scope_with(DATA::default(), open_edges)
    }

    /// Adds a new scope with no open edges and default data.
    pub fn add_scope_default_closed(&mut self) -> Scope {
        self.add_scope_with(DATA::default(), HashSet::new())
    }
}
