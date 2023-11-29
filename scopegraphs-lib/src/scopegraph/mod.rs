#![allow(unused)]

mod completeness;

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    iter,
    marker::PhantomData,
};

use bumpalo::Bump;

use self::{completeness::Completeness, private::Sealed};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scope(usize);

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
}

impl<LABEL: Hash + Eq, DATA> InnerScopeGraph<LABEL, DATA> {
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
        // TODO: Metadata updaten
        Scope(id)
    }

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
        // FIXME: Update metadata
    }

    /// Returns the data associated with the `scope` argument.
    fn get_data(&self, scope: Scope) -> &DATA {
        &self.data[scope.0]
    }

    /// Returns the targets of the outgoing edges of `src` with label `lbl`.
    fn get_edges<'a>(&'a self, scope: Scope, lbl: &LABEL) -> impl Iterator<Item = Scope> + 'a {
        self.edges[scope.0].get(lbl).into_iter().flatten().copied()
        // FIXME: update metadata
    }
}

mod private {
    pub trait Sealed {}
}

trait Accessor<'sg, 'lbl, LABEL, DATA>: Sealed {
    type DataResult;
    // type EdgeResult;

    fn get_data(&'sg mut self, scope: Scope) -> Self::DataResult;
}

pub struct ScopeGraph<LABEL, DATA, CMPL> {
    inner_scope_graph: InnerScopeGraph<LABEL, DATA>,
    completeness: CMPL,
}

impl<LABEL, DATA, CMPL> ScopeGraph<LABEL, DATA, CMPL>
where
    CMPL: Completeness<LABEL, DATA>,
{
    fn get_edges(&mut self, src: Scope, lbl: LABEL) -> CMPL::GetEdgesResult {
        self.completeness
            .get_edges(&self.inner_scope_graph, src, lbl)
    }
}

// Example implementations:

#[cfg(any())]
struct RawAccessor<'sg, 'lbl, LABEL, DATA> {
    phantom: &'sg &'lbl PhantomData<(LABEL, DATA)>,
}

#[cfg(any())]
impl<'sg, 'lbl, LABEL, DATA> Sealed for RawAccessor<'sg, 'lbl, LABEL, DATA> {}

#[cfg(any())]
impl<'sg, 'lbl: 'sg, LABEL: 'lbl, DATA: 'sg> Accessor<'sg, 'lbl, LABEL, DATA>
    for RawAccessor<'sg, 'lbl, LABEL, DATA>
{
    type DataResult = Option<&'sg DATA>;

    fn get_data(&mut self, scope: Scope) -> Self::DataResult {
        scope.data
    }
}

#[cfg(any())]
mod api_examples {

    use std::collections::{HashMap, HashSet};
    use std::future::{self, Future, Pending};
    use std::hash::Hash;
    use std::rc::Rc;

    use crate::resolve::topdown::EdgeOrData;

    use super::{Accessor, ScopeRef};

    struct WeaklyCriticalEdgeAccessor<'sg, 'lbl, LABEL, DATA> {
        open_edges: HashMap<Scope, HashSet<EdgeOrData<'lbl, LABEL>>>,
    }

    enum WCEData<'sg, DATA> {
        NoData,
        Open,
        Data(&'sg DATA),
    }

    impl<'sg, 'lbl: 'sg, LABEL: 'lbl, DATA: 'sg> Accessor<'sg, 'lbl, LABEL, DATA>
        for WeaklyCriticalEdgeAccessor<'sg, 'lbl, LABEL, DATA>
    where
        LABEL: Hash + Eq,
    {
        type DataResult = WCEData<'sg, DATA>;

        fn get_data(&mut self, scope: Scope) -> Self::DataResult {
            // Check if data is closed already or not yet.
            if self
                .open_edges
                .get(scope)
                .map(|o| o.contains(&EdgeOrData::Data))
                .unwrap_or(false)
            {
                WCEData::Open
            } else {
                scope
                    .data
                    .map(|d| WCEData::Data(d))
                    .unwrap_or(WCEData::NoData)
            }
        }
    }

    struct AsyncAccessor<'sg, 'lbl, LABEL, DATA> {
        open_edges: HashMap<Scope, HashSet<EdgeOrData<'lbl, LABEL>>>,
        futures: HashMap<(Scope, EdgeOrData<'lbl, LABEL>), Vec<Rc<Pending<Option<&'sg DATA>>>>>,
    }

    impl<'sg, 'lbl: 'sg, LABEL: 'lbl, DATA: 'sg> Accessor<'sg, 'lbl, LABEL, DATA>
        for AsyncAccessor<'sg, 'lbl, LABEL, DATA>
    where
        Scope: Hash + Eq,
        EdgeOrData<'lbl, LABEL>: Hash + Eq,
    {
        type DataResult = Rc<dyn Future<Output = Option<&'sg DATA>>>;

        fn get_data(&'sg mut self, scope: Scope) -> Self::DataResult {
            // Check if data is closed already or not yet.
            if self
                .open_edges
                .get(scope)
                .map(|o| o.contains(&EdgeOrData::Data))
                .unwrap_or(false)
            {
                let future: Rc<Pending<Option<&'sg DATA>>> = Rc::new(future::pending());
                match self.futures.get_mut(&(scope, EdgeOrData::Data)) {
                    Some(futures) => {
                        futures.push(future.clone());
                    }
                    None => {
                        let futures = vec![future.clone()];
                        self.futures.insert((scope, EdgeOrData::Data), futures);
                    }
                }
                return future;
            } else {
                return Rc::new(future::ready(scope.data));
            }
        }

        // when setting data, complete all open futures
    }
}
