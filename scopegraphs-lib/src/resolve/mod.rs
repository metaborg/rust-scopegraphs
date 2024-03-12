use prust_lib::hashmap::HashSet as TrieSet;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::marker::PhantomData;
use std::sync::Arc;

use super::{Scope, ScopeGraph};

mod params;
pub use params::*;
pub mod lookup;

/// Representation of either a labeled edge or the special 'data' label.
///
/// Used to implement label orders. The `Data` label is there to support expressing preference
/// between traversing an edge or resolving to the current node.
#[derive(Hash, PartialEq, Eq)]
pub enum EdgeOrData<LABEL> {
    Data,
    Edge(LABEL),
}

impl<LABEL: Debug> Debug for EdgeOrData<LABEL> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EdgeOrData::Data => write!(f, "$"),
            EdgeOrData::Edge(lbl) => write!(f, "@{:?}", lbl),
        }
    }
}

// custom implementation not to impose LABEL: Copy
impl<LABEL: Copy> Clone for EdgeOrData<LABEL> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<LABEL: Copy> Copy for EdgeOrData<LABEL> {}

#[derive(Hash, PartialEq, Eq, Debug)]
enum InnerPath<LABEL> {
    Start {
        source: Scope,
    },
    Step {
        prefix: Path<LABEL>,
        label: LABEL,
        target: Scope,
    },
}

/// Path (alternating sequence of scopes and labels) in a scope graph.
#[derive(Clone)]
pub struct Path<LABEL> {
    inner_path: Arc<InnerPath<LABEL>>,
    /// Set of all scopes in this path.
    ///
    /// Paths are alternating sequences of scopes and labels.
    /// In scope graphs, paths may not be cyclic.
    /// To check whether a possible _path extension_ is cyclic, we maintain a separate set of all scopes in a path.
    /// The [`Path::step`] function will check whether the new scope is in this set, and only return an extended oath if this is not the case.
    /// This is cheaper than traversing the [`Path::inner_path`], at the cost of some more memory usage.
    ///
    /// In order to make paths cheap to extend multiple times, we use a persistent data structure.
    scopes: Arc<TrieSet<Scope>>,
    // FIXME: put fields in same Arc
}

impl<LABEL> PartialEq for Path<LABEL>
where
    Scope: PartialEq,
    LABEL: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        // `self.scopes` is determined by the `inner_path`, so no need to check for equality there.
        self.inner_path == other.inner_path
    }
}

impl<LABEL> Eq for Path<LABEL>
where
    Scope: Eq,
    LABEL: Eq,
{
}

impl<LABEL> Hash for Path<LABEL>
where
    Scope: Hash,
    LABEL: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // `self.scopes` is determined by the `inner_path`, so no need to separately hash it.
        self.inner_path.hash(state);
    }
}

impl<LABEL> Debug for Path<LABEL>
where
    LABEL: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Path")
            // `self.scopes` is determined by the `inner_path`, so no need to include it in the debug representation.
            .field("inner_path", &self.inner_path)
            .finish()
    }
}

/// A path in the scope graph, including the data of the target of the path.
#[derive(Hash, PartialEq, Eq, Debug)]
pub struct ResolvedPath<'sg, LABEL, DATA> {
    path: Path<LABEL>,
    data: &'sg DATA,
}

impl<'sg, LABEL: Clone, DATA> Clone for ResolvedPath<'sg, LABEL, DATA> {
    fn clone(&self) -> Self {
        Self {
            path: self.path.clone(),
            data: self.data,
        }
    }
}

impl<'sg, LABEL, DATA> ResolvedPath<'sg, LABEL, DATA> {
    pub fn path(&self) -> &Path<LABEL> {
        &self.path
    }

    pub fn data(&self) -> &DATA {
        self.data
    }
}

impl<LABEL> Path<LABEL> {
    /// Creates a new path that contains of a single scope.
    pub fn new(source: Scope) -> Self {
        Self {
            inner_path: Arc::new(InnerPath::Start { source }),
            scopes: Arc::new(TrieSet::new().insert(source)),
        }
    }

    /// Returns the last scope in the path.
    pub fn target(&self) -> Scope {
        match self.inner_path.as_ref() {
            InnerPath::Start { source } => *source,
            InnerPath::Step { target, .. } => *target,
        }
    }

    /// Extends the path with a new edge.
    ///
    /// Returns `None` if the resulting path would contain a cycle.
    pub fn step(&self, label: LABEL, target: Scope) -> Option<Self> {
        if self.scopes.search(&target) {
            None
        } else {
            Some(Self {
                inner_path: Arc::new(InnerPath::Step {
                    prefix: Self {
                        inner_path: self.inner_path.clone(),
                        scopes: self.scopes.clone(),
                    },
                    label,
                    target,
                }),
                scopes: Arc::new(self.scopes.insert(target)),
            })
        }
    }

    /// Creates a resolved path from this path.
    pub fn resolve<DATA>(self, data: &DATA) -> ResolvedPath<'_, LABEL, DATA> {
        ResolvedPath { path: self, data }
    }
}

/// Representation of an environment (i.e., a collection of resolved paths).
// For now, we stick with hashmaps because they are easy.
// We might however want to change that in the future, because:
// - we currently create a lot of new hashmaps, which is not really efficient
// - efficiency might be dependent on the name resolution (shadowing) strategy
// - we (not always necessarily) clone hashmaps often
// Perhaps we will resort to fibbonacy heaps/pairing heaps, and/or make resolution parametric in the environment type.
#[derive(Debug)]
pub struct Env<'sg, LABEL: 'sg, DATA>(HashSet<ResolvedPath<'sg, LABEL, DATA>>);

impl<'sg, LABEL, DATA> IntoIterator for Env<'sg, LABEL, DATA> {
    type Item = ResolvedPath<'sg, LABEL, DATA>;

    type IntoIter = <HashSet<ResolvedPath<'sg, LABEL, DATA>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<LABEL, DATA> Default for Env<'_, LABEL, DATA> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'sg, LABEL, DATA> Env<'sg, LABEL, DATA> {
    /// Creates an empty environemnt.
    pub fn new() -> Self {
        Self(HashSet::new())
    }

    /// Create an iterator over all paths in the environment.
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a ResolvedPath<'sg, LABEL, DATA>> + 'a {
        self.0.iter()
    }

    /// Returns `true` is the environment is empty, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<'sg, LABEL, DATA> Env<'sg, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash,
{
    /// Create an environment with a single path.
    pub fn single(path: ResolvedPath<'sg, LABEL, DATA>) -> Self {
        let mut env = Env::new();
        env.insert(path);
        env
    }

    /// Insert a path in the environment.
    pub fn insert(&mut self, path: ResolvedPath<'sg, LABEL, DATA>) {
        self.0.insert(path);
    }

    /// Add all paths in `other` to the current environment.
    pub fn merge(&mut self, other: Self) {
        self.0.extend(other.0)
    }
}

impl<'sg, LABEL: 'sg, DATA: Hash> FromIterator<ResolvedPath<'sg, LABEL, DATA>>
    for Env<'sg, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash,
{
    fn from_iter<T: IntoIterator<Item = ResolvedPath<'sg, LABEL, DATA>>>(iter: T) -> Self {
        Env(HashSet::from_iter(iter))
    }
}

impl<'sg, LABEL: 'sg, DATA: Hash> FromIterator<Env<'sg, LABEL, DATA>> for Env<'sg, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash,
{
    fn from_iter<T: IntoIterator<Item = Env<'sg, LABEL, DATA>>>(iter: T) -> Self {
        iter.into_iter().flatten().collect()
    }
}

impl<'sg, LABEL: 'sg + Clone, DATA> Clone for Env<'sg, LABEL, DATA> {
    fn clone(&self) -> Self {
        Env(self.0.clone())
    }
}

pub trait Resolve<'sg, 'rslv> {
    type EnvContainer<'env>
    where
        'sg: 'env,
        'env: 'rslv,
        Self: 'rslv,
        'env: 'sg;

    fn resolve<'env: 'rslv>(&'rslv self, scope: Scope) -> Self::EnvContainer<'env>
    where
        'env: 'sg;
}

pub struct Query<'sg, 'rslv, LABEL, DATA, CMPL, PWF, DWF, LO, DEq> {
    _phantom: PhantomData<&'rslv ()>,
    scope_graph: &'sg ScopeGraph<LABEL, DATA, CMPL>,
    path_wellformedness: PWF,
    data_wellformedness: DWF,
    label_order: LO,
    data_equivalence: DEq,
}

impl<'sg, 'rslv, LABEL, DATA, CMPL, PWF, DWF, LO, DEq>
    Query<'sg, 'rslv, LABEL, DATA, CMPL, PWF, DWF, LO, DEq>
{
    pub fn with_path_wellformedness<NPWF>(
        self,
        new_path_wellformedness: NPWF,
    ) -> Query<'sg, 'rslv, LABEL, DATA, CMPL, NPWF, DWF, LO, DEq> {
        Query {
            _phantom: PhantomData,
            scope_graph: self.scope_graph,
            path_wellformedness: new_path_wellformedness,
            data_wellformedness: self.data_wellformedness,
            label_order: self.label_order,
            data_equivalence: self.data_equivalence,
        }
    }

    pub fn with_data_wellformedness<NDWF>(
        self,
        new_data_wellformedness: NDWF,
    ) -> Query<'sg, 'rslv, LABEL, DATA, CMPL, PWF, NDWF, LO, DEq> {
        Query {
            _phantom: PhantomData,
            scope_graph: self.scope_graph,
            path_wellformedness: self.path_wellformedness,
            data_wellformedness: new_data_wellformedness,
            label_order: self.label_order,
            data_equivalence: self.data_equivalence,
        }
    }

    pub fn with_label_order<NLO>(
        self,
        new_label_order: NLO,
    ) -> Query<'sg, 'rslv, LABEL, DATA, CMPL, PWF, DWF, NLO, DEq> {
        Query {
            _phantom: PhantomData,
            scope_graph: self.scope_graph,
            path_wellformedness: self.path_wellformedness,
            data_wellformedness: self.data_wellformedness,
            label_order: new_label_order,
            data_equivalence: self.data_equivalence,
        }
    }

    pub fn with_data_equivalence<NDEq>(
        self,
        new_data_equivalence: NDEq,
    ) -> Query<'sg, 'rslv, LABEL, DATA, CMPL, PWF, DWF, LO, NDEq> {
        Query {
            _phantom: PhantomData,
            scope_graph: self.scope_graph,
            path_wellformedness: self.path_wellformedness,
            data_wellformedness: self.data_wellformedness,
            label_order: self.label_order,
            data_equivalence: new_data_equivalence,
        }
    }
}

impl<LABEL, DATA, CMPL> ScopeGraph<LABEL, DATA, CMPL> {
    pub fn query(
        &self,
    ) -> Query<LABEL, DATA, CMPL, (), DefaultDataWellformedness, DefaultLabelOrder, DefaultDataEquiv>
    {
        Query {
            _phantom: PhantomData,
            scope_graph: self,
            path_wellformedness: (),
            data_wellformedness: DefaultDataWellformedness::default(),
            label_order: DefaultLabelOrder::default(),
            data_equivalence: DefaultDataEquiv::default(),
        }
    }
}
