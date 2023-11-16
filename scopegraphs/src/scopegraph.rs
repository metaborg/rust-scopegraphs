use std::slice::Iter;

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
pub trait ScopeGraph {
    /// Type of nodes.
    type Scope;
    /// Type of edge labels.
    type Label;
    /// Type of data associated with nodes.
    type Data;

    /// Adds a new scope to the graph, with `data` as its associated data.
    /// After this operation, all future calls to [`ScopeGraph::get_data`] on this scope will return the associated data.
    ///
    /// Example:
    /// ```no_run
    /// # use scopegraphs::scopegraphs::ScopeGraph;
    /// let mut sg : &dyn ScopeGraph<Scope = i32, Label = i32, Data = i32> = todo!();
    /// let data = 42;
    ///
    /// let scope = sg.add_scope(&data);
    ///
    /// let newData = sg.get_data(scope);
    /// assert_eq!(data, *newData);
    /// ```
    fn add_scope(&mut self, data: &Self::Data) -> &Self::Scope;

    /// Adds a new edge from `src`, to `dst`, with label `lbl` to the scope graph.
    /// After this operation, all future calls to [`ScopeGraph::get_edges`] on the source will contain the destination.
    ///
    /// Examplek:
    /// ```no_run
    /// # use scopegraphs::scopegraphs::ScopeGraph;
    /// enum Label { LEX }
    /// let mut sg : &dyn ScopeGraph<Scope = i32, Label = Label, Data = i32> = todo!();
    /// let data = 42;
    ///
    /// let src = sg.add_scope(&data);
    /// let dst = sg.add_scope(&data);
    /// sg.add_edge(src, &Label::LEX, dst);
    ///
    /// let dst_iter = sg.get_edges(src, &Label::LEX);
    /// assert!(dst_iter.any(|&d| d == *dst));
    /// ```
    ///
    fn add_edge(&mut self, src: &Self::Scope, lbl: &Self::Label, dst: &Self::Scope);

    /// Returns the data associated with the `scope` argument.
    fn get_data(&self, scope: &Self::Scope) -> &Self::Data;

    /// Returns the targets of the outgoing edges of `src` with label `lbl`.
    fn get_edges(&self, scope: &Self::Scope, lbl: &Self::Label) -> Iter<'_, Self::Scope>;
}
