//! This module contains several utilities to guarantee query stability.
//!
//! Query stability means that the result of a query, once resolved, will remain valid after future
//! additions to the scope graph.
//! This allows safe interleaving of name resolution and scope graph construction.
//!
//! The main trait of this module is [`Completeness`]. An instance of this trait should be passed
//! to [`ScopeGraph::new`](super::ScopeGraph::new) to obtain a scope graph instance.
//!
//! Currently, the module contains three safe implementations.
//! * [`ImplicitClose`] is the easiest to use, and most likely the preferred choice for simple
//! sequential type checkers.
//! * [`ExplicitClose`] requires some manual bookkeeping, but allows more flexible handling of
//! queries. This is the most suitable choice for type checkers that need to do dynamic scheduling.
//! Running queries can return an error, because scopes relevant to the query weren't closed yet.
//! * [`FutureCompleteness`] is like [`ExplicitClose`], except queries can no longer error. Instead,
//! queries return a [`Future`](std::future::Future) that resolves when all scopes related to the query are closed.

mod future;
pub use future::*;

mod critical_edge;
pub use critical_edge::*;
mod explicit;
pub use explicit::*;
mod implicit;
pub use implicit::*;
mod unchecked;
pub use unchecked::*;

mod private {
    pub trait Sealed {}
}

use crate::scopegraph::{InnerScopeGraph, Scope};
use private::Sealed;

/*** Completeness trait ***/

/// Types implementing this trait have the responsibility to guarantee query stability.
///
/// This means that the result of a query, once computed, may not be invalidated by later extensions
/// of the scope graph. See [`crate::concepts::completeness`]
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
/// - delay retrieval of edges until it is closed (by returning a future). TODO: [this is implemented now](`FutureCompleteness`)
/// - guarantee stability by retaining residual queries, and checking those do not return any
///   results for later additions.
/// - ...
///
/// This trait is sealed.
/// You cannot define your own completeness strategies to ensure that only verified implementations are available.
// TODO: @Aron could you document this?
#[allow(missing_docs)]
pub trait Completeness<LABEL, DATA>: Sealed {
    fn cmpl_new_scope(&self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope);

    /// Should initialize a scope without possibility to extend it with edges
    fn cmpl_new_complete_scope(
        &self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        scope: Scope,
    ) {
        self.cmpl_new_scope(inner_scope_graph, scope)
    }

    type NewEdgeResult;
    fn cmpl_new_edge(
        &self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult;

    // type GetDataResult;
    // fn get_data(&mut self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope) -> Self::GetDataResult;

    type GetEdgesResult<'rslv>
    where
        Self: 'rslv,
        LABEL: 'rslv,
        DATA: 'rslv;
    fn cmpl_get_edges<'rslv>(
        &'rslv self,
        inner_scope_graph: &'rslv InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
    ) -> Self::GetEdgesResult<'rslv>
    where
        LABEL: 'rslv,
        DATA: 'rslv;
}

// TODO: Residual-query-based Completeness requires access to query resolution
