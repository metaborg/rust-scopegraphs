use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Formatter},
    hash::Hash,
};

use crate::completeness::{Completeness, UncheckedCompleteness};

/// Representation of scopes (nodes in the scope graph).
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scope(pub(crate) usize);

impl Debug for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

// Mutability: RefCell in Scope, not Scope in RefCell
// Concurrency: RW-lock on edges

pub struct InnerScopeGraph<LABEL, DATA> {
    edges: Vec<HashMap<LABEL, HashSet<Scope>>>, // FIXME: BTreeMap? Vectors? Whatever?
    data: Vec<DATA>,
}

impl<LABEL, DATA> InnerScopeGraph<LABEL, DATA> {
    fn new() -> Self {
        Self {
            edges: Vec::new(),
            data: Vec::new(),
        }
    }

    /// Adds a new scope to the graph, with `data` as its associated data.
    /// After this operation, all future calls to [`InnerScopeGraph::get_data`] on this scope will return the associated data.
    pub(super) fn add_scope(&mut self, data: DATA) -> Scope {
        let id = self.data.len();
        self.data.push(data);
        self.edges.push(HashMap::with_capacity(0));
        Scope(id)
    }

    /// Returns the data associated with the `scope` argument.
    fn get_data(&self, scope: Scope) -> &DATA {
        // panics if src.0 is out of bounds
        // the methods that update `ScopeGraphs` retain the invariant that each scope has an appropriate entry in this vector
        // however, panics can still happen when a scope from a different scope graph is used
        &self.data[scope.0]
    }
}

impl<'a, LABEL: Hash + Eq, DATA> InnerScopeGraph<LABEL, DATA> {
    /// Adds a new edge from `src`, to `dst`, with label `lbl` to the scope graph.
    /// After this operation, all future calls to [`InnerScopeGraph::get_edges`] on the source will contain the destination.
    pub(crate) fn add_edge(&mut self, src: Scope, lbl: LABEL, dst: Scope) {
        // panics if src.0 is out of bounds
        // the methods that update `ScopeGraphs` retain the invariant that each scope has an appropriate entry in this vector
        // however, panics can still happen when a scope from a different scope graph is used
        self.edges[src.0].entry(lbl).or_default().insert(dst);
    }

    /// Returns the targets of the outgoing edges of `src` with label `lbl`.
    pub(crate) fn get_edges(&'a self, scope: Scope, lbl: LABEL) -> Vec<Scope> {
        // panics if scope.0 is out of bounds
        // the methods that update `ScopeGraphs` retain the invariant that each scope has an appropriate entry in this vector
        // however, panics can still happen when a scope from a different scope graph is used
        self.edges[scope.0]
            .get(&lbl)
            .into_iter()
            .flatten()
            .copied()
            .collect()
    }
}

/// Scope Graph data structure.
///
/// As a data structure, scope graphs are simple graphs with labeled nodes and labeled, directed edges.
///
/// This trait has three type parameters:
/// - [`LABEL`]: the type of the edge labels.
/// - [`DATA`]: the type of the scope/node labels.
/// - [`CMPL`]: metadata that guarantees query stability (i.e., query results remain valid in the future).
///
/// The data structure has been designed for typical scope graph usage scenario's.
/// For example, there is no support for _removing_ scopes or edges, as this usually does not happen in scope graphs.
/// In addition, there is no data type for edges, as edges should only be traversed, but never leak outside the scope graph structure.
/// Finally, although not made explicit, [`LABEL`] should be a finite, iterable set.
pub struct ScopeGraph<LABEL, DATA, CMPL> {
    pub(super) inner_scope_graph: InnerScopeGraph<LABEL, DATA>,
    pub(super) completeness: CMPL,
}

impl<LABEL, DATA, CMPL> ScopeGraph<LABEL, DATA, CMPL> {
    pub fn new(completeness: CMPL) -> Self {
        ScopeGraph {
            inner_scope_graph: InnerScopeGraph::new(),
            completeness,
        }
    }
}

impl<LABEL, DATA> ScopeGraph<LABEL, DATA, UncheckedCompleteness> {
    /// Creates a new scope graph with [`UncheckedCompleteness`] as its completeness validation.
    ///
    /// # Safety
    ///
    /// Unsafe, because [`UncheckedCompleteness`] does not actually guarantee query stability.
    pub unsafe fn raw() -> Self {
        Self::new(UncheckedCompleteness::new())
    }
}

impl<LABEL, DATA, CMPL> ScopeGraph<LABEL, DATA, CMPL>
where
    CMPL: Completeness<LABEL, DATA>,
{
    /// Add a new scope to the scope graph, with `data` as its label.
    pub fn add_scope(&mut self, data: DATA) -> Scope {
        let scope = self.inner_scope_graph.add_scope(data);
        self.completeness
            .cmpl_new_scope(&mut self.inner_scope_graph, scope);
        scope
    }

    /// Add a new edge in the scope graph.
    ///
    /// Permission for this is checked by `CMPL`.
    pub fn add_edge(&mut self, src: Scope, lbl: LABEL, dst: Scope) -> CMPL::NewEdgeResult {
        self.completeness
            .cmpl_new_edge(&mut self.inner_scope_graph, src, lbl, dst)
    }

    /// Get the data associated with a scope.
    pub fn get_data(&self, scope: Scope) -> &DATA {
        self.inner_scope_graph.get_data(scope)
    }

    /// Get the targets of the outgoing edges of a scope with some label.
    ///
    /// Permission for this operation is checked by `CMPL`.
    pub fn get_edges<'sg>(&'sg self, src: Scope, lbl: LABEL) -> CMPL::GetEdgesResult<'sg> {
        self.completeness
            .cmpl_get_edges(&self.inner_scope_graph, src, lbl)
    }

    /// Utility function to add declarations (i.e., scopes with data, without any outgoing edges).
    ///
    /// It performs (roughly) the following operation:
    ///
    /// ```ignore
    /// fn add_decl(&mut self, src: Scope, lbl: LABEL, data: DATA) -> CMPL::NewEdgeResult {
    ///     let s_data = self.add_scope(data);
    ///     self.add_edge(src, lbl, s_data);
    /// }
    /// ```
    pub fn add_decl(&mut self, src: Scope, lbl: LABEL, data: DATA) -> CMPL::NewEdgeResult {
        // Create scope with no open edges.
        let s_data = self.inner_scope_graph.add_scope(data);
        self.completeness
            .cmpl_new_complete_scope(&mut self.inner_scope_graph, s_data);
        self.add_edge(src, lbl, s_data)
    }
}

impl<LABEL, DATA, CMPL> ScopeGraph<LABEL, DATA, CMPL>
where
    DATA: Default,
    CMPL: Completeness<LABEL, DATA>,
{
    /// Add a new scope to the scope graph, with default data.
    pub fn add_scope_default(&mut self) -> Scope {
        self.add_scope(DATA::default())
    }
}
