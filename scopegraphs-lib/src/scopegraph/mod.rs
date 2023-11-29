#![allow(unused)]

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    iter,
    marker::PhantomData,
};

use bumpalo::Bump;

#[derive(Debug)]
pub struct Scope<'sg, 'lbl, LABEL, DATA> {
    id: u64, // TODO: by_address?
    data: Option<&'sg DATA>,
    edges: HashMap<&'lbl LABEL, HashSet<&'sg Scope<'sg, 'lbl, LABEL, DATA>>>, // FIXME: Vec?
}

pub type ScopeRef<'sg, 'lbl, LABEL, DATA> = &'sg Scope<'sg, 'lbl, LABEL, DATA>;

impl<LABEL, DATA> PartialEq for Scope<'_, '_, LABEL, DATA> {
    fn eq(&self, other: &Self) -> bool {
        // id should uniquely define scope identity
        self.id == other.id
    }
}

impl<LABEL, DATA> Eq for Scope<'_, '_, LABEL, DATA> {}

impl<'sg, 'lbl, LABEL, DATA> Hash for Scope<'sg, 'lbl, LABEL, DATA> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<'sg, LABEL, DATA> Scope<'sg, '_, LABEL, DATA> {
    fn new(id: u64) -> Self {
        Self {
            id,
            data: None,
            edges: HashMap::new(),
        }
    }

    // FIXME: Scope always has data immediately
    fn set_data(&mut self, data: &'sg DATA) -> Result<(), ScopeGraphDataError> {
        if self.data.is_some() {
            Err(ScopeGraphDataError::OverrideData)
        } else {
            self.data = Some(data);
            Ok(())
        }
    }
}

impl<'sg, 'lbl, LABEL, DATA> Scope<'sg, 'lbl, LABEL, DATA>
where
    LABEL: Hash + Eq,
    DATA: Hash + Eq,
{
    fn add_edge(&mut self, label: &'lbl LABEL, target: ScopeRef<'sg, 'lbl, LABEL, DATA>) {
        self.edges.entry(label).or_default().insert(target);
    }

    fn get_edges(
        &'sg self,
        label: &'lbl LABEL,
    ) -> impl Iterator<Item = &ScopeRef<'sg, 'lbl, LABEL, DATA>> {
        self.edges.get(label).into_iter().flatten()
    }
}

/// Overriding scope graph data is not allowed,
pub enum ScopeGraphDataError {
    OverrideData,
}

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
pub struct ScopeGraph<'sg, LABEL, DATA> {
    id_counter: u64,
    allocator: &'sg Bump,
    phantom: PhantomData<(LABEL, DATA)>,
}

impl<'sg, LABEL, DATA> ScopeGraph<'sg, LABEL, DATA> {
    pub fn new(allocator: &'sg Bump) -> Self {
        Self {
            id_counter: 0,
            allocator,
            phantom: PhantomData,
        }
    }

    /// Adds a new scope to the graph, with `data` as its associated data.
    /// After this operation, all future calls to [`ScopeGraph::get_data`] on this scope will return the associated data.
    ///
    /// Example:
    /// ```ignore
    /// # use scopegraphs::scopegraph::ScopeGraph;
    /// let mut sg : ScopeGraph<i32, i32, i32> = ScopeGraph::new();
    /// let data = 42;
    ///
    /// let scope = sg.add_scope(&data);
    ///
    /// let newData = sg.get_data(scope);
    /// assert_eq!(data, *newData);
    /// ```
    fn add_scope(&mut self) -> &mut Scope<'_, '_, LABEL, DATA> {
        let id = self.id_counter;
        self.id_counter += 1;
        self.allocator.alloc(Scope::new(id))
    }

    /// Adds a new edge from `src`, to `dst`, with label `lbl` to the scope graph.
    /// After this operation, all future calls to [`ScopeGraph::get_edges`] on the source will contain the destination.
    ///
    /// Example:
    /// ```ignore
    /// # use scopegraphs::{scopegraph::ScopeGraph, Label};
    ///
    /// #[derive(Label, Eq, PartialEq, Copy, Clone)]
    /// enum Label { LEX }
    /// let mut sg : ScopeGraph<i32, Label, i32> = ScopeGraph::new();
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
    pub fn add_edge(
        &mut self,
        _src: ScopeRef<'_, '_, LABEL, DATA>,
        _lbl: &LABEL,
        _dst: &ScopeRef<'_, '_, LABEL, DATA>,
    ) {
    }

    /// Returns the data associated with the `scope` argument.
    pub fn get_data(&self, _scope: ScopeRef<'_, '_, LABEL, DATA>) -> Option<&DATA> {
        todo!()
    }

    /// Returns the targets of the outgoing edges of `src` with label `lbl`.
    pub fn get_edges(
        &self,
        _scope: ScopeRef<'_, '_, LABEL, DATA>,
        _lbl: &LABEL,
    ) -> impl Iterator<Item = ScopeRef<'_, '_, LABEL, DATA>> {
        std::iter::empty()
    }
}

trait Accessor<'sg, 'lbl, LABEL, DATA> {
    type DataResult;
    // type EdgeResult;

    fn get_data(&'sg mut self, scope: ScopeRef<'sg, 'lbl, LABEL, DATA>) -> Self::DataResult;
}

// Example implementations:

struct RawAccessor<'sg, 'lbl, LABEL, DATA> {
    phantom: &'sg &'lbl PhantomData<(LABEL, DATA)>,
}

impl<'sg, 'lbl: 'sg, LABEL: 'lbl, DATA: 'sg> Accessor<'sg, 'lbl, LABEL, DATA>
    for RawAccessor<'sg, 'lbl, LABEL, DATA>
{
    type DataResult = Option<&'sg DATA>;

    fn get_data(&mut self, scope: &Scope<'sg, 'lbl, LABEL, DATA>) -> Self::DataResult {
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
        open_edges: HashMap<ScopeRef<'sg, 'lbl, LABEL, DATA>, HashSet<EdgeOrData<'lbl, LABEL>>>,
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

        fn get_data(&mut self, scope: ScopeRef<'sg, 'lbl, LABEL, DATA>) -> Self::DataResult {
            // Check if data is closed already or not yet.
            if self
                .open_edges
                .get(scope)
                .map(|o| o.contains(&EdgeOrData::Data))
                .unwrap_or(false)
            {
                WCEData::NoData
            } else {
                scope
                    .data
                    .map(|d| WCEData::Data(d))
                    .unwrap_or(WCEData::NoData)
            }
        }
    }

    struct AsyncAccessor<'sg, 'lbl, LABEL, DATA> {
        open_edges: HashMap<ScopeRef<'sg, 'lbl, LABEL, DATA>, HashSet<EdgeOrData<'lbl, LABEL>>>,
        futures: HashMap<
            (ScopeRef<'sg, 'lbl, LABEL, DATA>, EdgeOrData<'lbl, LABEL>),
            Vec<Rc<Pending<Option<&'sg DATA>>>>,
        >,
    }

    impl<'sg, 'lbl: 'sg, LABEL: 'lbl, DATA: 'sg> Accessor<'sg, 'lbl, LABEL, DATA>
        for AsyncAccessor<'sg, 'lbl, LABEL, DATA>
    where
        ScopeRef<'sg, 'lbl, LABEL, DATA>: Hash + Eq,
        EdgeOrData<'lbl, LABEL>: Hash + Eq,
    {
        type DataResult = Rc<dyn Future<Output = Option<&'sg DATA>>>;

        fn get_data(&'sg mut self, scope: ScopeRef<'sg, 'lbl, LABEL, DATA>) -> Self::DataResult {
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
