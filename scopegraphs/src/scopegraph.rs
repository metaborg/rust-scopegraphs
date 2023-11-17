use std::marker::PhantomData;

/// Scope Graph operations.
///
/// This trait describes scope graphs, and the primitive operations that can be applied on them.
/// As a data structure, scope graphs are simple graphs with labeled nodes and labeled, directed edges.
///
/// This trait has three associated types:
/// - [`ScopeGraph::Scope`]: the type of the nodes.
/// - [`ScopeGraph::Label`]: the type of the edge labels.
/// - [`ScopeGraph::Data`]: the type of the scope/node labels.
///
/// The data structure has been designed for typical scope graph usage scenario's.
/// For example, there is no support for _removing_ scopes or edges, as this usually does not happen in scope graphs.
/// In addition, there is no data type for edges, as edges should only be traversed, but never leak outside the scope graph structure.
/// Finally, although not made explicit, [`ScopeGraph::Label`] should be a finite, iterable set.
///
/// Typical scope graph implementations will take ownership of the nodes, but only store references to the data that is associated with them.
///
/// Type parameters:
/// - [`SCOPE`] Type of nodes.
/// - [`LABEL`] Type of edge labels.
/// - [`DATA`] Type of data associated with nodes.
pub struct ScopeGraph<SCOPE, LABEL, DATA> {
    phantom: PhantomData<(SCOPE, LABEL, DATA)>,
}

impl<SCOPE, LABEL, DATA> Default for ScopeGraph<SCOPE, LABEL, DATA> {
    fn default() -> Self {
        Self::new()
    }
}

impl<SCOPE, LABEL, DATA> ScopeGraph<SCOPE, LABEL, DATA> {
    pub fn new() -> Self {
        Self {
            phantom: PhantomData,
        }
    }

    /// Adds a new scope to the graph, with `data` as its associated data.
    /// After this operation, all future calls to [`ScopeGraph::get_data`] on this scope will return the associated data.
    ///
    /// Example:
    /// ```no_run
    /// # use scopegraphs::ScopeGraph;
    /// let mut sg : &dyn ScopeGraph<Scope = i32, Label = i32, Data = i32> = todo!();
    /// let data = 42;
    ///
    /// let scope = sg.add_scope(&data);
    ///
    /// let newData = sg.get_data(scope);
    /// assert_eq!(data, *newData);
    /// ```
    pub fn add_scope(&mut self, _data: &DATA) -> &SCOPE {
        todo!()
    }

    /// Adds a new edge from `src`, to `dst`, with label `lbl` to the scope graph.
    /// After this operation, all future calls to [`ScopeGraph::get_edges`] on the source will contain the destination.
    ///
    /// Example:
    /// ```no_run
    /// # use scopegraphs::ScopeGraph;
    /// enum Label { LEX }
    /// let mut sg : &dyn ScopeGraph<Scope = i32, Label = Label, Data = i32> = todo!();
    /// let data = 42;
    ///
    /// let src = sg.add_scope(&data);
    /// let dst = sg.add_scope(&data);
    /// sg.add_edge(src, &Label::LEX, dst);
    ///
    /// let mut  dst_iter = sg.get_edges(src, &Label::LEX);
    /// assert!(dst_iter.any(|&d| d == *dst));
    /// ```
    ///
    pub fn add_edge(&mut self, _src: &SCOPE, _lbl: &LABEL, _dst: &SCOPE) {
        todo!()
    }

    /// Returns the data associated with the `scope` argument.
    pub fn get_data(&self, _scope: &SCOPE) -> &DATA {
        todo!()
    }

    /// Returns the targets of the outgoing edges of `src` with label `lbl`.
    pub fn get_edges(&self, _scope: &SCOPE, _lbl: &LABEL) -> impl Iterator<Item = SCOPE> {
        std::iter::empty()
    }
}
