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

use crate::label::Label;

use super::{InnerScopeGraph, Scope};

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

/*** Weakly-Critical-Edge Based Completeness Checking with Explicit Closing ***/

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
            HashSet::from_iter(LABEL::iter()),
        )
    }

    fn cmpl_new_complete_scope(&mut self, _: &InnerScopeGraph<LABEL, DATA>, _: Scope) {
        <ExplicitClose<LABEL> as CriticalEdgeBasedCompleteness<LABEL, DATA>>::init_scope_with(
            self,
            HashSet::new(),
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
    pub(super) fn close(&mut self, scope: Scope, label: &LABEL) {
        self.critical_edges.close(scope, label);
    }
}

/*** Weakly-Critical-Edge Based Completeness Checking with Implicit Closing ***/

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

// TODO: Asynchronous Completeness can be a wrapper around the ExplicitClose impl

// TODO: Residual-query-based Completeness requires access to query resolution
