use crate::{scopegraph::InnerScopeGraph, Label, Scope, ScopeGraph};

use super::{private::Sealed, Completeness, CriticalEdgeBasedCompleteness, ExplicitClose};

use std::{collections::HashSet, hash::Hash};

/// Completeness implementation that leverages the [Drop] trait to close edges.
#[derive(Debug, Default)]
pub struct DropClose<LABEL> {
    explicit_close: ExplicitClose<LABEL>,
}

impl<LABEL> Sealed for DropClose<LABEL> {}

impl<LABEL: Hash + Eq + Label, DATA> Completeness<LABEL, DATA> for DropClose<LABEL> {
    fn cmpl_new_scope(&self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope) {
        self.explicit_close.cmpl_new_scope(inner_scope_graph, scope)
    }

    fn cmpl_new_complete_scope(
        &self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        scope: Scope,
    ) {
        self.explicit_close
            .cmpl_new_complete_scope(inner_scope_graph, scope)
    }

    type NewEdgeResult = <ExplicitClose<LABEL> as Completeness<LABEL, DATA>>::NewEdgeResult;

    fn cmpl_new_edge(
        &self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult {
        self.explicit_close
            .cmpl_new_edge(inner_scope_graph, src, lbl, dst)
    }

    type GetEdgesResult<'rslv> = <ExplicitClose<LABEL> as Completeness<LABEL, DATA>>::GetEdgesResult<'rslv>
        where Self: 'rslv, LABEL: 'rslv, DATA: 'rslv;

    fn cmpl_get_edges<'rslv>(
        &'rslv self,
        inner_scope_graph: &'rslv InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
    ) -> Self::GetEdgesResult<'rslv>
    where
        LABEL: 'rslv,
        DATA: 'rslv,
    {
        self.explicit_close
            .cmpl_get_edges(inner_scope_graph, src, lbl)
    }
}

impl<LABEL: Hash + Eq + Label, DATA> CriticalEdgeBasedCompleteness<LABEL, DATA>
    for DropClose<LABEL>
{
    fn init_scope_with(&self, open_edges: HashSet<LABEL>) {
        CriticalEdgeBasedCompleteness::<_, DATA>::init_scope_with(&self.explicit_close, open_edges)
    }
}

pub struct ScopeExt<'ext, 'storage, LABEL, DATA, CMPL> {
    scope: Scope,
    label: LABEL,
    sg: &'ext ScopeGraph<'storage, LABEL, DATA, CMPL>,
}
