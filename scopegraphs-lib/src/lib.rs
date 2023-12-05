pub mod label;
pub mod resolve;

pub mod completeness;

use std::fmt::{Debug, Display, Formatter};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
};

use completeness::Completeness;
use completeness::{CriticalEdgeBasedCompleteness, ExplicitClose, Witness};

use self::completeness::UncheckedCompleteness;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scope(usize);

impl Debug for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

// Mutability: RefCell in Scope, not Scope in RefCell
// Concurrency: RW-lock on edges

/// Scope Graph operations.
///
/// This trait describes scope graphs, and the primitive operations that can be applied on them.
/// As a data structure, scope graphs are simple graphs with labeled nodes and labeled, directed edges.
///
/// This trait has three associated types:
/// - [`InnerScopeGraph::Scope`]: the type of the nodes.
/// - [`InnerScopeGraph::Label`]: the type of the edge labels.
/// - [`InnerScopeGraph::Data`]: the type of the scope/node labels.
///
/// The data structure has been designed for typical scope graph usage scenario's.
/// For example, there is no support for _removing_ scopes or edges, as this usually does not happen in scope graphs.
/// In addition, there is no data type for edges, as edges should only be traversed, but never leak outside the scope graph structure.
/// Finally, although not made explicit, [`InnerScopeGraph::Label`] should be a finite, iterable set.
///
/// Typical scope graph implementations will take ownership of the nodes, but only store references to the data that is associated with them.
///
/// Type parameters:
/// - [`SCOPE`] Type of nodes.
/// - [`LABEL`] Type of edge labels.
/// - [`DATA`] Type of data associated with nodes.
#[derive(Default)]
pub struct InnerScopeGraph<LABEL, DATA /*, META */> {
    id_counter: u64,
    edges: Vec<HashMap<LABEL, HashSet<Scope>>>, // FIXME: BTreeMap? Vectors? Whatever?
    data: Vec<DATA>,
}

impl<LABEL, DATA> InnerScopeGraph<LABEL, DATA> {
    pub fn new() -> Self {
        Self {
            id_counter: 0,
            edges: Vec::new(),
            data: Vec::new(),
        }
    }

    /// Adds a new scope to the graph, with `data` as its associated data.
    /// After this operation, all future calls to [`InnerScopeGraph::get_data`] on this scope will return the associated data.
    ///
    /// Example:
    /// ```ignore
    /// # use InnerScopeGraphs::InnerScopeGraph::InnerScopeGraph;
    /// let mut sg : InnerScopeGraph<i32, i32, i32> = InnerScopeGraph::new();
    /// let data = 42;
    ///
    /// let scope = sg.add_scope(&data);
    ///
    /// let newData = sg.get_data(scope);
    /// assert_eq!(data, *newData);
    /// ```
    pub fn add_scope(&mut self, data: DATA) -> Scope {
        let id = self.data.len();
        self.data.push(data);
        self.edges.push(HashMap::with_capacity(0));
        Scope(id)
    }
}

impl<'a, LABEL: Hash + Eq, DATA> InnerScopeGraph<LABEL, DATA> {
    /// Adds a new edge from `src`, to `dst`, with label `lbl` to the scope graph.
    /// After this operation, all future calls to [`InnerScopeGraph::get_edges`] on the source will contain the destination.
    ///
    /// Example:
    /// ```ignore
    /// # use InnerScopeGraphs::{InnerScopeGraph::InnerScopeGraph, Label};
    ///
    /// #[derive(Label, Eq, PartialEq, Copy, Clone)]
    /// enum Label { LEX }
    /// let mut sg : InnerScopeGraph<i32, Label, i32> = InnerScopeGraph::new();
    /// let data = 42;
    ///
    /// let src = sg.add_scope(&data);
    /// let dst = sg.add_scope(&data);
    /// sg.add_edge(src, &Label::LEX, dst);
    ///
    /// let mut  dst_iter = sg.get_edges(src, &Label::LEX);
    /// assert!(dst_iter.any(|d| d == *dst));
    /// ```
    ///
    pub fn add_edge(&mut self, src: Scope, lbl: LABEL, dst: Scope) {
        self.edges[src.0].entry(lbl).or_default().insert(dst);
    }

    /// Returns the data associated with the `scope` argument.
    fn get_data(&self, scope: Scope) -> &DATA {
        &self.data[scope.0]
    }

    /// Returns the targets of the outgoing edges of `src` with label `lbl`.
    fn get_edges(&'a self, scope: Scope, lbl: LABEL) -> Vec<Scope> {
        self.edges[scope.0]
            .get(&lbl)
            .into_iter()
            .flatten()
            .copied()
            .collect()
    }
}

pub struct ScopeGraph<LABEL, DATA, CMPL> {
    inner_scope_graph: InnerScopeGraph<LABEL, DATA>,
    completeness: RefCell<CMPL>,
}

impl<LABEL, DATA, CMPL> ScopeGraph<LABEL, DATA, CMPL> {
    pub fn new(completeness: CMPL) -> Self {
        ScopeGraph {
            inner_scope_graph: InnerScopeGraph::new(),
            completeness: RefCell::new(completeness),
        }
    }
}
impl<LABEL, DATA> ScopeGraph<LABEL, DATA, UncheckedCompleteness> {
    pub fn raw() -> Self {
        Self::new(UncheckedCompleteness::default())
    }
}

impl<LABEL, DATA, CMPL> ScopeGraph<LABEL, DATA, CMPL>
where
    CMPL: Completeness<LABEL, DATA>,
{
    pub fn new_scope(&mut self, data: DATA) -> Scope {
        let scope = self.inner_scope_graph.add_scope(data);
        self.completeness
            .borrow_mut()
            .cmpl_new_scope(&self.inner_scope_graph, scope);
        scope
    }

    pub fn new_edge(&mut self, src: Scope, lbl: LABEL, dst: Scope) -> CMPL::NewEdgeResult {
        self.completeness
            .borrow_mut()
            .cmpl_new_edge(&mut self.inner_scope_graph, src, lbl, dst)
    }

    pub fn get_data(&self, scope: Scope) -> &DATA {
        &self.inner_scope_graph.data[scope.0]
    }

    pub fn get_edges(&self, src: Scope, lbl: LABEL) -> CMPL::GetEdgesResult {
        self.completeness
            .borrow_mut()
            .cmpl_get_edges(&self.inner_scope_graph, src, lbl)
    }

    pub fn new_decl(&mut self, src: Scope, lbl: LABEL, data: DATA) -> CMPL::NewEdgeResult {
        // Create scope with no open edges.
        let s_data = self.inner_scope_graph.add_scope(data);
        self.completeness
            .borrow_mut()
            .cmpl_new_complete_scope(&self.inner_scope_graph, s_data);
        self.new_edge(src, lbl, s_data)
    }
}

impl<LABEL: Hash + Eq, DATA, CMPL> ScopeGraph<LABEL, DATA, CMPL>
where
    CMPL: CriticalEdgeBasedCompleteness<LABEL, DATA>,
{
    pub fn new_scope_with<I>(&mut self, data: DATA, open_edges: I) -> Scope
    where
        I: IntoIterator<Item = LABEL>,
    {
        let scope = self.inner_scope_graph.add_scope(data);
        self.completeness
            .borrow_mut()
            .init_scope_with(HashSet::from_iter(open_edges.into_iter()), Witness(()));
        scope
    }
}

impl<LABEL: Hash + Eq, DATA> ScopeGraph<LABEL, DATA, ExplicitClose<LABEL>> {
    pub fn close(&self, scope: Scope, label: &LABEL) {
        self.completeness.borrow_mut().close(scope, label)
    }
}

#[cfg(test)]
mod test {
    use super::{completeness::UncheckedCompleteness, Scope, ScopeGraph};

    #[test]
    fn test_create_scope() {
        let mut sg: ScopeGraph<usize, usize, _> = ScopeGraph::raw();
        let scope = sg.new_scope(42);
        assert_eq!(42, *sg.get_data(scope));
    }

    #[test]
    fn test_create_two_scopes() {
        let mut sg: ScopeGraph<usize, usize, _> = ScopeGraph::raw();

        let s1 = sg.new_scope(1);
        let s2 = sg.new_scope(2);

        assert_eq!(1, *sg.get_data(s1));
        assert_eq!(2, *sg.get_data(s2));
    }

    #[test]
    fn test_create_edge() {
        let mut sg: ScopeGraph<usize, usize, _> = ScopeGraph::raw();

        let s1 = sg.new_scope(1);
        let s2 = sg.new_scope(2);

        sg.new_edge(s1, 1, s2);

        assert_eq!(vec![s2], sg.get_edges(s1, 1));
        assert_eq!(Vec::<Scope>::new(), sg.get_edges(s1, 2));
    }

    #[test]
    fn test_create_edges() {
        let mut sg: ScopeGraph<usize, usize, _> = ScopeGraph::raw();

        let s1 = sg.new_scope(1);
        let s2 = sg.new_scope(2);
        let s3 = sg.new_scope(3);

        sg.new_edge(s1, 1, s2);
        sg.new_edge(s1, 1, s3);

        assert!(sg.get_edges(s1, 1).contains(&s2));
        assert!(sg.get_edges(s1, 1).contains(&s3));

        assert_eq!(Vec::<Scope>::new(), sg.get_edges(s1, 2));
    }
}
