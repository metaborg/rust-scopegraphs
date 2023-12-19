//! This module contains several utilities to guarantee query stability.
//! Query stability means that the result of a query, once resolved, will remain valid after future
//! additions to the scope graph.
//! This allows safe interleaving of name resolution and scope graph construction.
//!
//! The main trait of this module is [`Completeness`]. An instance of this trait should be passed
//! to [`super::ScopeGraph::new`] to obtain a correct scope graph instance.
//!
//! Currently the module contains two safe implementations.
//! [`ImplicitClose`] is the easiest to use, and most likely the preferred choice for simple
//! sequential type checkers.
//! [`ExplicitClose`] requires some manual bookkeeping, but allows more flexible handling of
//! queries. This is the most suitable choice for type checkers that need to do dynamic scheduling.

use std::{collections::HashSet, hash::Hash};

use super::{InnerScopeGraph, Scope};

mod explicit;
pub use explicit::*;
mod implicit;
pub use implicit::*;
mod unchecked;
pub use unchecked::*;

mod private {
    pub trait Sealed {}
}

use private::Sealed;

/*** Completeness trait ***/

/// Types implementing this trait have the responsibility to guarantee query stability.
///
/// This means that the result of a query, once computed, may not be invalidated by later extensions
/// of the scope graph.
///
/// A [`super::ScopeGraph`] will call the handlers of its completeness on each operation.
/// For scope addition and data access, it can update some internal state.
/// For edge addition and retrieval, it can override/augment the default result as well.
///
/// The way query stability is guaranteed is very flexible. Current implementations use
/// - critical edges, explicitly closed by the client ([`ExplicitClose`]).
/// - critical edges, implicitly closed when retrieved ([`ImplicitClose`]).
///
/// Future implementations may also:
/// - delay retrieval of edges until it is closed (by returning a future).
/// - guarantee stability by retaining residual queries, and checking those do not return any
///   results for later additions.
/// - ...
///
/// This trait is sealed to ensure only verified implementations are available.
pub trait Completeness<LABEL, DATA>: Sealed {
    fn cmpl_new_scope(&mut self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope);

    /// Should initialize a scope without possibility to extend it with edges
    fn cmpl_new_complete_scope(
        &mut self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        scope: Scope,
    ) {
        self.cmpl_new_scope(inner_scope_graph, scope)
    }

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

// TODO: Asynchronous Completeness can be a wrapper around the ExplicitClose impl

// TODO: Residual-query-based Completeness requires access to query resolution
