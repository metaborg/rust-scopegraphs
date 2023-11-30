use std::{arch::x86_64::__m128, collections::HashSet, future, hash::Hash};

use crate::label::Label;

use super::{InnerScopeGraph, Scope};

mod private {
    pub trait Sealed {}
}

use private::Sealed;

/*** Completeness trait ***/

pub trait Completeness<LABEL, DATA>: Sealed {
    fn new_scope(&mut self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope); // FIXME `scope` needed?

    type NewEdgeResult<'a>
    where
        Self: 'a,
        LABEL: 'a,
        DATA: 'a;
    fn new_edge<'a>(
        &mut self,
        inner_scope_graph: &'a mut InnerScopeGraph<'a, LABEL, DATA>,
        src: Scope,
        lbl: &'a LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult<'a>;

    // type GetDataResult;
    // fn get_data(&mut self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope) -> Self::GetDataResult;

    type GetEdgesResult<'a>
    where
        Self: 'a,
        DATA: 'a,
        LABEL: 'a;
    fn get_edges<'a, 'b: 'a>(
        &mut self,
        inner_scope_graph: &'a InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: &'b LABEL,
    ) -> Self::GetEdgesResult<'a>;
}

/*** Unchecked Completeness Implementation ***/

struct UncheckedCompleteness {}
impl Sealed for UncheckedCompleteness {}

impl<LABEL: Hash + Eq, DATA> Completeness<LABEL, DATA> for UncheckedCompleteness {
    fn new_scope(&mut self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope) {}

    type NewEdgeResult<'a> = () where DATA: 'a, LABEL: 'a;

    fn new_edge<'a>(
        &mut self,
        inner_scope_graph: &'a mut InnerScopeGraph<'a, LABEL, DATA>,
        src: Scope,
        lbl: &'a LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult<'a> {
        inner_scope_graph.add_edge(src, lbl, dst)
    }

    type GetEdgesResult<'a> = Box<dyn Iterator<Item = Scope> + 'a> where DATA: 'a, LABEL: 'a;

    fn get_edges<'a, 'b: 'a>(
        &mut self,
        inner_scope_graph: &'a InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: &'b LABEL,
    ) -> Self::GetEdgesResult<'a> {
        Box::new(inner_scope_graph.get_edges(src, lbl))
    }
}

/*** Utilities for Weakly-Critical-Edge Based Completeness Checking ***/

struct CriticalEdgeSet<LABEL> {
    open_edges: Vec<HashSet<LABEL>>,
}

impl<LABEL> CriticalEdgeSet<LABEL> {
    fn init_scope(&mut self, edges: HashSet<LABEL>) {
        self.open_edges.push(edges)
    }
}

impl<LABEL: Hash + Eq> CriticalEdgeSet<LABEL> {
    fn is_open(&self, scope: Scope, lbl: &LABEL) -> bool {
        self.open_edges[scope.0].contains(lbl)
    }

    fn close(&mut self, scope: Scope, lbl: &LABEL) -> bool {
        self.open_edges[scope.0].remove(lbl)
    }
}

pub enum EdgeClosedError<'a, LABEL> {
    EdgeClosed { scope: Scope, label: &'a LABEL },
}

pub enum EdgesOrDelay<'a, EDGES, LABEL> {
    Edges { edges: EDGES },
    Delay { scope: Scope, label: &'a LABEL },
}

/*** Weakly-Critical-Edge Based Completeness Checking with Explicit Closing ***/

struct ExplicitClose<LABEL> {
    critical_edges: CriticalEdgeSet<LABEL>,
}

impl<LABEL> Sealed for ExplicitClose<LABEL> {}

impl<LABEL: Hash + Eq + for<'a> Label<'a>, DATA> Completeness<LABEL, DATA>
    for ExplicitClose<LABEL>
{
    fn new_scope(&mut self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope) {
        self.critical_edges
            .init_scope(HashSet::from_iter(LABEL::iter()))
    }

    type NewEdgeResult<'a> = Result<(), EdgeClosedError<'a, LABEL>> where LABEL: 'a, DATA: 'a;

    fn new_edge<'a>(
        &mut self,
        inner_scope_graph: &'a mut InnerScopeGraph<'a, LABEL, DATA>,
        src: Scope,
        lbl: &'a LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult<'a> {
        if self.critical_edges.is_open(src, &lbl) {
            Err(EdgeClosedError::EdgeClosed {
                scope: src,
                label: lbl,
            })
        } else {
            inner_scope_graph.add_edge(src, lbl, dst);
            Ok(())
        }
    }

    type GetEdgesResult<'a> = EdgesOrDelay<'a, Box<dyn Iterator<Item = Scope> + 'a>, LABEL> where DATA: 'a, LABEL: 'a;

    fn get_edges<'a, 'b: 'a>(
        &mut self,
        inner_scope_graph: &'a InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: &'b LABEL,
    ) -> Self::GetEdgesResult<'a> {
        if self.critical_edges.is_open(src, &lbl) {
            EdgesOrDelay::Delay {
                scope: src,
                label: lbl,
            }
        } else {
            EdgesOrDelay::Edges {
                edges: Box::new(inner_scope_graph.get_edges(src, lbl)),
            }
        }
    }
}

/*** Weakly-Critical-Edge Based Completeness Checking with Implicit Closing ***/

struct ImplicitClose<LABEL> {
    critical_edges: CriticalEdgeSet<LABEL>,
}

impl<LABEL> Sealed for ImplicitClose<LABEL> {}

impl<LABEL: Hash + Eq + for<'a> Label<'a>, DATA> Completeness<LABEL, DATA>
    for ImplicitClose<LABEL>
{
    fn new_scope(&mut self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope) {
        self.critical_edges
            .init_scope(HashSet::from_iter(LABEL::iter()))
    }

    type NewEdgeResult<'a> = Result<(), EdgeClosedError<'a, LABEL>> where DATA: 'a, LABEL: 'a;

    // FIXME: identical to `ExplicitClose` impl.
    fn new_edge<'a>(
        &mut self,
        inner_scope_graph: &'a mut InnerScopeGraph<'a, LABEL, DATA>,
        src: Scope,
        lbl: &'a LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult<'a> {
        if self.critical_edges.is_open(src, &lbl) {
            // FIXME: provide reason (queries) that made this edge closed?
            Err(EdgeClosedError::EdgeClosed {
                scope: src,
                label: lbl,
            })
        } else {
            inner_scope_graph.add_edge(src, lbl, dst);
            Ok(())
        }
    }

    type GetEdgesResult<'a> = Box<dyn Iterator<Item = Scope> + 'a> where DATA: 'a, LABEL: 'a;

    fn get_edges<'a, 'b: 'a>(
        &mut self,
        inner_scope_graph: &'a InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: &'b LABEL,
    ) -> Self::GetEdgesResult<'a> {
        self.critical_edges.close(src, &lbl);
        Box::new(inner_scope_graph.get_edges(src, lbl))
    }
}

// TODO: Asynchronous Completeness can be a wrapper around the ExplicitClose impl

// TODO: Residual-query-based Completeness requires access to query resolution
