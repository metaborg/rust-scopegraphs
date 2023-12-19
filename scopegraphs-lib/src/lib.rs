pub mod completeness;
pub mod label;
pub mod resolve;

use std::fmt::{Debug, Formatter};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
};

use completeness::Completeness;
use completeness::{CriticalEdgeBasedCompleteness, ExplicitClose};

use self::completeness::UncheckedCompleteness;

/// Representation of scopes (nodes in the scope graph).
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scope(usize);

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
    fn add_scope(&mut self, data: DATA) -> Scope {
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
    fn add_edge(&mut self, src: Scope, lbl: LABEL, dst: Scope) {
        // panics if src.0 is out of bounds
        // the methods that update `ScopeGraphs` retain the invariant that each scope has an appropriate entry in this vector
        // however, panics can still happen when a scope from a different scope graph is used
        self.edges[src.0].entry(lbl).or_default().insert(dst);
    }

    /// Returns the targets of the outgoing edges of `src` with label `lbl`.
    fn get_edges(&'a self, scope: Scope, lbl: LABEL) -> Vec<Scope> {
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
            .borrow_mut()
            .cmpl_new_scope(&self.inner_scope_graph, scope);
        scope
    }

    /// Add a new edge in the scope graph.
    ///
    /// Permission for this is checked by `CMPL`.
    pub fn add_edge(&mut self, src: Scope, lbl: LABEL, dst: Scope) -> CMPL::NewEdgeResult {
        self.completeness
            .borrow_mut()
            .cmpl_new_edge(&mut self.inner_scope_graph, src, lbl, dst)
    }

    /// Get the data associated with a scope.
    pub fn get_data(&self, scope: Scope) -> &DATA {
        self.inner_scope_graph.get_data(scope)
    }

    /// Get the targets of the outgoing edges of a scope with some label.
    ///
    /// Permission for this operation is checked by `CMPL`.
    pub fn get_edges(&self, src: Scope, lbl: LABEL) -> CMPL::GetEdgesResult {
        self.completeness
            .borrow_mut()
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
            .borrow_mut()
            .cmpl_new_complete_scope(&self.inner_scope_graph, s_data);
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

impl<LABEL: Hash + Eq, DATA, CMPL> ScopeGraph<LABEL, DATA, CMPL>
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
            .borrow_mut()
            .init_scope_with(HashSet::from_iter(open_edges.into_iter()));
        scope
    }

    /// Adds a new scope with no open edges.
    pub fn add_scope_closed(&mut self, data: DATA) -> Scope {
        let scope = self.inner_scope_graph.add_scope(data);
        self.completeness
            .borrow_mut()
            .init_scope_with(HashSet::new());
        scope
    }
}

impl<LABEL: Hash + Eq, DATA, CMPL> ScopeGraph<LABEL, DATA, CMPL>
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

impl<LABEL: Hash + Eq, DATA> ScopeGraph<LABEL, DATA, ExplicitClose<LABEL>> {
    /// Closes an edge, (i.e., prohibit future new
    ///
    /// For example, the following program will return an error.
    /// ```
    /// # use scopegraphs_lib::completeness::ExplicitClose;
    /// # use scopegraphs_lib::ScopeGraph;
    /// # use scopegraphs_macros::Label;
    /// # #[derive(Eq, Hash, PartialEq, Label)] enum Lbl { Def }
    /// # use Lbl::*;
    /// let mut sg = ScopeGraph::<Lbl, usize, _>::new(ExplicitClose::default());
    ///
    /// let s1 = sg.add_scope_with(0, [Def]);
    /// let s2 = sg.add_scope_closed(42);
    ///
    /// sg.close(s1, &Def);
    /// sg.add_edge(s1, Def, s2).expect_err("cannot add edge after closing edge");
    /// ```
    ///
    /// Closing is required to permit queries to traverse these edges:
    /// ```
    ///
    /// # use scopegraphs_lib::completeness::ExplicitClose;
    /// # use scopegraphs_lib::ScopeGraph;
    /// # use scopegraphs_lib::resolve::{DefaultDataEquiv, DefaultLabelOrder, EdgeOrData, Resolve};
    /// # use scopegraphs_macros::{compile_regex, Label};
    /// #
    /// # #[derive(Eq, Hash, PartialEq, Label, Debug, Copy, Clone)]
    /// # enum Lbl { Def }
    /// # use Lbl::*;
    /// # type LblD = EdgeOrData<Lbl>;
    /// #
    /// # compile_regex!(type Regex<Lbl> = Def);
    /// let mut sg = ScopeGraph::<Lbl, usize, _>::new(ExplicitClose::default());
    ///
    /// let s1 = sg.add_scope_with(0, [Def]);
    /// let s2 = sg.add_scope_closed(42);
    ///
    /// // Note: not calling `sg.close(s1, &Def)`
    ///
    /// let query_result = sg.query()
    ///     .with_path_wellformedness(Regex::new()) // regex: `Def`
    ///     .with_data_wellformedness(|x: &usize| *x == 42) // match `42`
    ///     .resolve(s1);
    ///
    /// query_result.expect_err("require s1/Def to be closed");
    /// ```
    ///
    /// Closing allows queries to resolve:
    /// ```
    ///
    /// # use scopegraphs_lib::completeness::ExplicitClose;
    /// # use scopegraphs_lib::ScopeGraph;
    /// # use scopegraphs_lib::resolve::{DefaultDataEquiv, DefaultLabelOrder, EdgeOrData, Resolve};
    /// # use scopegraphs_macros::{compile_regex, Label};
    /// #
    /// # #[derive(Eq, Hash, PartialEq, Label, Debug, Copy, Clone)]
    /// # enum Lbl { Def }
    /// # use Lbl::*;
    /// # type LblD = EdgeOrData<Lbl>;
    /// #
    /// # compile_regex!(type Regex<Lbl> = Def);
    /// let mut sg = ScopeGraph::<Lbl, usize, _>::new(ExplicitClose::default());
    ///
    /// let s1 = sg.add_scope_with(0, [Def]);
    /// let s2 = sg.add_scope_closed(42);
    ///
    /// // Note: closing the edge *after* creating all edges, *before* doing the query
    /// sg.close(s1, &Def);
    ///
    /// let query_result = sg.query()
    ///     .with_path_wellformedness(Regex::new()) // regex: `Def`
    ///     .with_data_wellformedness(|x: &usize| *x == 42) // match `42`
    ///     .resolve(s1);
    ///
    /// query_result.expect("query should return result");
    /// ```
    pub fn close(&self, scope: Scope, label: &LABEL) {
        self.completeness.borrow_mut().close(scope, label)
    }
}

#[cfg(test)]
mod test {
    use super::{Scope, ScopeGraph};

    #[test]
    fn test_create_scope() {
        let mut sg: ScopeGraph<usize, usize, _> = unsafe { ScopeGraph::raw() };
        let scope = sg.add_scope(42);
        assert_eq!(42, *sg.get_data(scope));
    }

    #[test]
    fn test_create_two_scopes() {
        let mut sg: ScopeGraph<usize, usize, _> = unsafe { ScopeGraph::raw() };

        let s1 = sg.add_scope(1);
        let s2 = sg.add_scope(2);

        assert_eq!(1, *sg.get_data(s1));
        assert_eq!(2, *sg.get_data(s2));
    }

    #[test]
    fn test_create_edge() {
        let mut sg: ScopeGraph<usize, usize, _> = unsafe { ScopeGraph::raw() };

        let s1 = sg.add_scope(1);
        let s2 = sg.add_scope(2);

        sg.add_edge(s1, 1, s2);

        assert_eq!(vec![s2], sg.get_edges(s1, 1));
        assert_eq!(Vec::<Scope>::new(), sg.get_edges(s1, 2));
    }

    #[test]
    fn test_create_edges() {
        let mut sg: ScopeGraph<usize, usize, _> = unsafe { ScopeGraph::raw() };

        let s1 = sg.add_scope(1);
        let s2 = sg.add_scope(2);
        let s3 = sg.add_scope(3);

        sg.add_edge(s1, 1, s2);
        sg.add_edge(s1, 1, s3);

        assert!(sg.get_edges(s1, 1).contains(&s2));
        assert!(sg.get_edges(s1, 1).contains(&s3));

        assert_eq!(Vec::<Scope>::new(), sg.get_edges(s1, 2));
    }
}
