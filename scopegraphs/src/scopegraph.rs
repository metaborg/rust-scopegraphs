use std::cell::RefCell;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Formatter},
    hash::Hash,
};

use crate::completeness::{Completeness, Implicit, ScopeExt, UncheckedCompleteness, UserClosed};
use crate::storage::Storage;
use crate::Label;

/// Representation of scopes (nodes in the scope graph).
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scope(pub usize);

impl Debug for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

// Mutability: RefCell in Scope, not Scope in RefCell
// Concurrency: RW-lock on edges

#[derive(Debug)]
pub struct InnerScopeGraph<'sg, LABEL, DATA> {
    pub storage: &'sg Storage,
    #[allow(clippy::type_complexity)]
    pub edges: RefCell<Vec<&'sg RefCell<HashMap<LABEL, HashSet<Scope>>>>>, // FIXME: BTreeMap? Vectors? Whatever?
    pub data: RefCell<Vec<&'sg DATA>>,
}

impl<'sg, LABEL, DATA> InnerScopeGraph<'sg, LABEL, DATA> {
    fn new(storage: &'sg Storage) -> Self {
        Self {
            storage,
            edges: RefCell::new(Vec::new()),
            data: RefCell::new(Vec::new()),
        }
    }

    /// Adds a new scope to the graph, with `data` as its associated data.
    /// After this operation, all future calls to [`InnerScopeGraph::get_data`] on this scope will return the associated data.
    pub fn add_scope(&self, data: DATA) -> Scope {
        let id = self.data.borrow().len();

        self.data.borrow_mut().push(self.storage.0.alloc(data));
        self.edges.borrow_mut().push(
            self.storage
                .0
                .alloc(RefCell::new(HashMap::with_capacity(0))),
        );
        Scope(id)
    }

    /// Returns the data associated with the `scope` argument.
    fn get_data(&self, scope: Scope) -> &'sg DATA {
        // panics if src.0 is out of bounds
        // the methods that update `ScopeGraphs` retain the invariant that each scope has an appropriate entry in this vector
        // however, panics can still happen when a scope from a different scope graph is used
        self.data.borrow()[scope.0]
    }
}

impl<'sg, LABEL: Hash + Eq, DATA> InnerScopeGraph<'sg, LABEL, DATA> {
    /// Adds a new edge from `src`, to `dst`, with label `lbl` to the scope graph.
    /// After this operation, all future calls to [`InnerScopeGraph::get_edges`] on the source will contain the destination.
    pub fn add_edge(&self, src: Scope, lbl: LABEL, dst: Scope) {
        // panics if src.0 is out of bounds
        // the methods that update `ScopeGraphs` retain the invariant that each scope has an appropriate entry in this vector
        // however, panics can still happen when a scope from a different scope graph is used
        self.edges.borrow()[src.0]
            .borrow_mut()
            .entry(lbl)
            .or_default()
            .insert(dst);
    }

    /// Returns the targets of the outgoing edges of `src` with label `lbl`.
    pub fn get_edges(&self, scope: Scope, lbl: LABEL) -> Vec<Scope> {
        // panics if scope.0 is out of bounds
        // the methods that update `ScopeGraphs` retain the invariant that each scope has an appropriate entry in this vector
        // however, panics can still happen when a scope from a different scope graph is used
        self.edges.borrow()[scope.0]
            .borrow()
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
/// - `LABEL`: the type of the edge labels.
/// - `DATA`: the type of the scope/node labels.
/// - `CMPL`: metadata that guarantees query stability (i.e., query results remain valid in the future).
///
/// The data structure has been designed for typical scope graph usage scenario's.
/// For example, there is no support for _removing_ scopes or edges, as this usually does not happen in scope graphs.
/// In addition, there is no data type for edges, as edges should only be traversed, but never leak outside the scope graph structure.
/// Finally, although not made explicit, `LABEL` should be a finite, iterable set.
#[derive(Debug)]
pub struct ScopeGraph<'storage, LABEL, DATA, CMPL> {
    pub(crate) inner_scope_graph: InnerScopeGraph<'storage, LABEL, DATA>,
    pub(crate) completeness: CMPL,
}

impl<'storage, LABEL, DATA, CMPL> ScopeGraph<'storage, LABEL, DATA, CMPL> {
    /// Creates a new, empty, scope graph.
    ///
    /// You must supply a [`Storage`] object, in which the scope graph can allocate memory,
    /// and a [`Completeness`] strategy, that defines how the scope graph should deal with query stability.
    pub fn new(storage: &'storage Storage, completeness: CMPL) -> Self {
        ScopeGraph {
            inner_scope_graph: InnerScopeGraph::new(storage),
            completeness,
        }
    }
}

impl<'sg, LABEL, DATA> ScopeGraph<'sg, LABEL, DATA, UncheckedCompleteness> {
    /// Creates a new scope graph with [`UncheckedCompleteness`] as its completeness validation.
    ///
    /// # Safety
    ///
    /// Unsafe, because [`UncheckedCompleteness`] does not actually guarantee query stability.
    pub unsafe fn raw(storage: &'sg Storage) -> Self {
        Self::new(storage, UncheckedCompleteness::new())
    }
}

impl<'sg, LABEL, DATA, CMPL> ScopeGraph<'sg, LABEL, DATA, CMPL>
where
    CMPL: Completeness<LABEL, DATA>,
{
    /// Add a new scope to the scope graph, with `data` as its label.
    pub fn add_scope(&self, data: DATA) -> Scope {
        let scope = self.inner_scope_graph.add_scope(data);
        self.completeness
            .cmpl_new_scope(&self.inner_scope_graph, scope);
        scope
    }

    // add_edge defined below, based on completeness type

    /// Get the data associated with a scope.
    pub fn get_data(&self, scope: Scope) -> &DATA {
        self.inner_scope_graph.get_data(scope)
    }

    /// Get the targets of the outgoing edges of a scope with some label.
    ///
    /// Permission for this operation is checked by `CMPL`.
    pub fn get_edges(&self, src: Scope, lbl: LABEL) -> CMPL::GetEdgesResult<'_> {
        self.completeness
            .cmpl_get_edges(&self.inner_scope_graph, src, lbl)
    }
}

impl<'sg, LABEL, DATA, CMPL> ScopeGraph<'sg, LABEL, DATA, CMPL>
where
    CMPL: Implicit<LABEL, DATA>,
{
    /// Add a new edge in the scope graph.
    ///
    /// Permission for this is checked by `CMPL`.
    pub fn add_edge(&self, src: Scope, lbl: LABEL, dst: Scope) -> CMPL::NewEdgeResult {
        self.completeness
            .cmpl_new_edge(&self.inner_scope_graph, src, lbl, dst)
    }

    /// Utility function to add declarations (i.e., scopes with data, without any outgoing edges).
    ///
    /// It performs (roughly) the following operation:
    ///
    /// ```ignore
    /// fn add_decl(&self, src: Scope, lbl: LABEL, data: DATA) -> CMPL::NewEdgeResult {
    ///     let s_data = self.add_scope(data);
    ///     self.add_edge(src, lbl, s_data);
    /// }
    /// ```
    pub fn add_decl(&self, src: Scope, lbl: LABEL, data: DATA) -> CMPL::NewEdgeResult {
        // Create scope with no open edges.
        let s_data = self.inner_scope_graph.add_scope(data);
        self.completeness
            .cmpl_new_complete_scope(&self.inner_scope_graph, s_data);
        self.add_edge(src, lbl, s_data)
    }
}

impl<'sg, LABEL: Hash + Eq + Label + Copy + Debug, DATA, CMPL> ScopeGraph<'sg, LABEL, DATA, CMPL>
where
    CMPL: UserClosed<LABEL, DATA>,
{
    /// Add a new edge in the scope graph.
    ///
    /// Permission for this is checked by `CMPL`.
    pub fn ext_edge<'ext>(
        &'ext self,
        ext: &ScopeExt<'ext, LABEL, DATA, CMPL>,
        dst: Scope,
    ) -> CMPL::NewEdgeResult {
        self.completeness
            .cmpl_new_edge(&self.inner_scope_graph, ext.scope, ext.label, dst)
    }

    /// Utility function to add declarations (i.e., scopes with data, without any outgoing edges).
    ///
    /// It performs (roughly) the following operation:
    ///
    /// ```ignore
    /// fn ext_decl(&self, src: Scope, lbl: LABEL, data: DATA) -> CMPL::NewEdgeResult {
    ///     let s_data = self.add_scope(data);
    ///     self.ext_edge(src, lbl, s_data);
    /// }
    /// ```
    pub fn ext_decl<'ext>(
        &'ext self,
        ext: &ScopeExt<'ext, LABEL, DATA, CMPL>,
        data: DATA,
    ) -> CMPL::NewEdgeResult {
        // Create scope with no open edges.
        let s_data = self.inner_scope_graph.add_scope(data);
        self.completeness
            .cmpl_new_complete_scope(&self.inner_scope_graph, s_data);
        self.ext_edge(ext, s_data)
    }
}

impl<'sg, LABEL, DATA, CMPL> ScopeGraph<'sg, LABEL, DATA, CMPL>
where
    DATA: Default,
    CMPL: Completeness<LABEL, DATA>,
{
    /// Add a new scope to the scope graph, with default data.
    pub fn add_scope_default(&self) -> Scope {
        self.add_scope(DATA::default())
    }
}
