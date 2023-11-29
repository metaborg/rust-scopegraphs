use std::{collections::HashSet, future};

use super::{InnerScopeGraph, Scope};

pub trait Completeness<LABEL, DATA> {
    fn new_scope(&mut self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope); // FIXME `scope` needed?

    type NewEdgeResult;
    fn new_edge(
        &mut self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult;

    // type GetDataResult;
    // fn on_get_data(&mut self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope) -> Self::GetDataResult;

    type GetEdgesResult;
    fn get_edges(
        &mut self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
    ) -> Self::GetEdgesResult;
}

struct ExplicitClose<LABEL> {
    open_edges: Vec<HashSet<LABEL>>,
}

struct ImplicitClose<LABEL> {
    closed_edges: Vec<HashSet<LABEL>>,
}
