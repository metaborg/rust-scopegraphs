//! This module contains code to query scope graphs.

use scopegraphs_prust_lib::hashmap::HashSet as TrieSet;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::marker::PhantomData;
use std::rc::Rc;

mod params;
use crate::{Label, Scope, ScopeGraph};
pub use params::*;
use scopegraphs_regular_expressions::RegexMatcher;

pub mod lookup;

/// Representation of either a labeled edge or the special 'data' label.
///
/// Used to implement label orders. The `Data` label is there to support expressing preference
/// between traversing an edge or resolving to the current node.
// TODO: document this well, also with a `concepts` page about $ and labels
//       @Aron could you do this?
#[allow(missing_docs)]
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
    inner_path: Rc<InnerPath<LABEL>>,
    /// Set of all scopes in this path.
    ///
    /// Paths are alternating sequences of scopes and labels.
    /// In scope graphs, paths may not be cyclic.
    /// To check whether a possible _path extension_ is cyclic, we maintain a separate set of all scopes in a path.
    /// The [`Path::step`] function will check whether the new scope is in this set, and only return an extended oath if this is not the case.
    /// This is cheaper than traversing the [`Path::inner_path`], at the cost of some more memory usage.
    ///
    /// In order to make paths cheap to extend multiple times, we use a persistent data structure.
    scopes: Rc<TrieSet<Scope>>,
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
    pub(crate) data: &'sg DATA,
}

impl<LABEL: Clone, DATA> Clone for ResolvedPath<'_, LABEL, DATA> {
    fn clone(&self) -> Self {
        Self {
            path: self.path.clone(),
            data: self.data,
        }
    }
}

impl<LABEL, DATA> ResolvedPath<'_, LABEL, DATA> {
    /// Get the full path of scopes leading to this target scope
    pub fn path(&self) -> &Path<LABEL> {
        &self.path
    }

    /// Get the data on this target scope.
    pub fn data(&self) -> &DATA {
        self.data
    }
}

impl<LABEL> Path<LABEL> {
    /// Creates a new path that contains of a single scope.
    pub fn new(source: Scope) -> Self {
        Self {
            inner_path: Rc::new(InnerPath::Start { source }),
            scopes: Rc::new(TrieSet::new().insert(source)),
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
                inner_path: Rc::new(InnerPath::Step {
                    prefix: Self {
                        inner_path: self.inner_path.clone(),
                        scopes: self.scopes.clone(),
                    },
                    label,
                    target,
                }),
                scopes: Rc::new(self.scopes.insert(target)),
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

impl<'a, 'sg, LABEL, DATA> IntoIterator for &'a Env<'sg, LABEL, DATA> {
    type Item = &'a ResolvedPath<'sg, LABEL, DATA>;

    type IntoIter = <&'a HashSet<ResolvedPath<'sg, LABEL, DATA>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

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

/// Error emitted by [Env::get_only_item] when the environment argument did not contain exactly one argument.
pub enum OnlyElementError<'a, 'sg, DATA, LABEL, I>
where
    I: Iterator<Item = &'a ResolvedPath<'sg, DATA, LABEL>>,
{
    /// Environment was empty
    Empty,
    /// Environment contained multiple items
    Multiple {
        /// the first element that the iterator returned
        first: &'a ResolvedPath<'sg, DATA, LABEL>,
        /// the second element the iterator returned (witnessing the environment is not a singleton environment)
        second: &'a ResolvedPath<'sg, DATA, LABEL>,
        /// the iterator (can be used to access the remaining elements)
        rest: I,
    },
}

impl<'a, 'sg, DATA, LABEL, I> Debug for OnlyElementError<'a, 'sg, DATA, LABEL, I>
where
    I: Iterator<Item = &'a ResolvedPath<'sg, DATA, LABEL>>,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OnlyElementError::Empty => write!(f, "OnlyElementError::Empty"),
            OnlyElementError::Multiple { .. } => {
                write!(f, "OnlyElementError::Multiple {{..}}")
            }
        }
    }
}

impl<'a, 'sg, DATA, LABEL, I> IntoIterator for OnlyElementError<'a, 'sg, DATA, LABEL, I>
where
    I: Iterator<Item = &'a ResolvedPath<'sg, DATA, LABEL>>,
{
    type Item = &'a ResolvedPath<'sg, DATA, LABEL>;
    type IntoIter = OnlyElementErrorIter<'a, 'sg, DATA, LABEL, I>;

    fn into_iter(self) -> Self::IntoIter {
        OnlyElementErrorIter { e: self, offset: 0 }
    }
}

/// Iterator over an [`OnlyElementError`], to easily access its elements.
pub struct OnlyElementErrorIter<'a, 'sg, DATA, LABEL, I>
where
    I: Iterator<Item = &'a ResolvedPath<'sg, DATA, LABEL>>,
{
    e: OnlyElementError<'a, 'sg, DATA, LABEL, I>,
    offset: usize,
}

impl<'a, 'sg, DATA, LABEL, I> Iterator for OnlyElementErrorIter<'a, 'sg, DATA, LABEL, I>
where
    I: Iterator<Item = &'a ResolvedPath<'sg, DATA, LABEL>>,
{
    type Item = &'a ResolvedPath<'sg, DATA, LABEL>;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.e {
            OnlyElementError::Empty => None,
            OnlyElementError::Multiple {
                first,
                second,
                rest,
            } => match self.offset {
                0 => {
                    self.offset += 1;
                    Some(first)
                }
                1 => {
                    self.offset += 1;
                    Some(second)
                }
                _ => rest.next(),
            },
        }
    }
}

impl<'sg, LABEL, DATA> Env<'sg, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash + Clone,
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
    pub fn merge(&mut self, other: &Self) {
        self.0.extend(other.0.iter().cloned())
    }

    /// Returns `Ok(value)` if the environment only has a single resolved path, or an error otherwise.
    #[allow(clippy::type_complexity)]
    pub fn get_only_item<'a>(
        &'a self,
    ) -> Result<
        ResolvedPath<'sg, LABEL, DATA>,
        OnlyElementError<
            'a,
            'sg,
            LABEL,
            DATA,
            impl Iterator<Item = &'a ResolvedPath<'sg, LABEL, DATA>> + 'a,
        >,
    > {
        let mut iter = self.iter();
        iter.next().map_or(Err(OnlyElementError::Empty), |value| {
            iter.next().map_or_else(
                || Ok(value.clone()),
                |second| {
                    Err(OnlyElementError::Multiple {
                        first: value,
                        second,
                        rest: iter,
                    })
                },
            )
        })
    }
}

impl<'sg, LABEL: 'sg, DATA> FromIterator<ResolvedPath<'sg, LABEL, DATA>> for Env<'sg, LABEL, DATA>
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

/// A trait that represents a specific name resolution algorithm.
///
/// Currently has one implementor: [`Query`].
pub trait Resolve<'sg, 'rslv> {
    /// The type of result that this query gives.
    /// This depends on the [completeness strategy](crate::completeness::Completeness) used.
    ///
    /// * Using [`ImplicitClose`](crate::completeness::ImplicitClose) this is a simply a `Vec<Scope>`.
    ///   Querying using this completeness strategy cannot fail.
    /// * Using [`ExplicitClose`](crate::completeness::ExplicitClose) this is a [`EdgesOrDelay<Scope, LABEL>`](crate::completeness::EdgesOrDelay).
    ///
    /// Querying can fail, because a scope this query traverses wasn't closed yet.
    ///
    /// Using [`FutureCompleteness`](crate::completeness::FutureCompleteness), this is a [`Future`](std::future::Future).
    ///
    /// Querying can pend, because a scope this query traverses wasn't closed yet.
    type EnvContainer
    where
        'sg: 'rslv,
        Self: 'rslv;

    /// actually run this query
    fn resolve(&'rslv self, scope: Scope) -> Self::EnvContainer;
}

/// A query over a scope graph. Read more [here](crate::concepts)
pub struct Query<'storage, 'sg, 'rslv, LABEL: Label, DATA, CMPL, PWF, DWF, LO, DEq> {
    _phantom: PhantomData<&'rslv ()>,
    scope_graph: &'sg ScopeGraph<'storage, LABEL, DATA, CMPL>,
    path_wellformedness: PWF,
    data_wellformedness: DWF,
    label_order: LO,
    data_equivalence: DEq,
}

impl<'sg, 'storage, 'rslv, LABEL: Label, DATA, CMPL, PWF, DWF, LO, DEq>
    Query<'sg, 'storage, 'rslv, LABEL, DATA, CMPL, PWF, DWF, LO, DEq>
{
    /// Add a [path well-formedness](crate::concepts::path_wellformedness) to this query.
    ///
    /// A path well-formedness can be specified using a regular expression.
    /// Often you want to use [`query_regex!`](crate::query_regex) here.
    ///
    /// ```rust
    /// # use scopegraphs::{query_regex, ScopeGraph, Storage};
    /// # use scopegraphs::completeness::UncheckedCompleteness;
    /// # use scopegraphs::Label;
    /// # let storage = Storage::new();
    /// # let scopegraph = ScopeGraph::<Lbl, (), _>::new(&storage, unsafe{UncheckedCompleteness::new()});
    ///
    /// #[derive(Label, Eq, PartialEq, Copy, Clone)]
    /// pub enum Lbl {
    ///     Lexical,
    ///     Definition
    /// }
    /// use Lbl::*;
    ///
    /// scopegraph.query()
    ///     .with_path_wellformedness(query_regex!(Lbl: Lexical* Definition));
    ///     
    /// ```
    ///
    pub fn with_path_wellformedness<NPWF>(
        self,
        new_path_wellformedness: NPWF,
    ) -> Query<'sg, 'storage, 'rslv, LABEL, DATA, CMPL, NPWF, DWF, LO, DEq>
    where
        NPWF: for<'a> RegexMatcher<&'a LABEL> + 'rslv,
    {
        Query {
            _phantom: PhantomData,
            scope_graph: self.scope_graph,
            path_wellformedness: new_path_wellformedness,
            data_wellformedness: self.data_wellformedness,
            label_order: self.label_order,
            data_equivalence: self.data_equivalence,
        }
    }

    /// Add a [data well-formedness](crate::concepts::data_wellformedness) to this query.
    /// A data well-formedness must implement [`DataWellformedness`].
    ///
    /// Defaults to [`DefaultDataWellformedness`], considering all data to be well-formed.
    ///
    /// With a data well-formedness you can specify what data a scope must have to be a valid
    /// target for this query.
    ///
    /// ```rust
    /// # use scopegraphs::{query_regex, ScopeGraph, Storage};
    /// # use scopegraphs::completeness::UncheckedCompleteness;
    /// # use scopegraphs::Label;
    /// # let storage = Storage::new();
    /// # let scopegraph = ScopeGraph::<(), MyData, _>::new(&storage, unsafe{UncheckedCompleteness::new()});
    /// use scopegraphs::resolve::DataWellformedness;
    ///
    /// struct MyData {
    ///     is_good: bool
    /// }
    /// struct MyDataWellformedness;
    /// impl<'sg> DataWellformedness<'sg, MyData> for MyDataWellformedness {
    ///     type Output = bool;
    ///     fn data_wf(&self, data: &'sg MyData) -> bool {
    ///         data.is_good
    ///     }
    /// }
    ///
    /// scopegraph.query()
    ///     .with_data_wellformedness(MyDataWellformedness);
    ///
    /// ```
    ///
    /// A data-wellformedness can be a lambda that takes a reference to `DATA` and returns a boolean.
    ///
    /// ```rust
    /// # use scopegraphs::{query_regex, ScopeGraph, Storage};
    /// # use scopegraphs::completeness::UncheckedCompleteness;
    /// # use scopegraphs::Label;
    /// # let storage = Storage::new();
    /// # let scopegraph = ScopeGraph::<(), MyData, _>::new(&storage, unsafe{UncheckedCompleteness::new()});
    /// use scopegraphs::resolve::DataWellformedness;
    ///
    /// struct MyData {
    ///     is_good: bool
    /// }
    /// scopegraph.query()
    ///     .with_data_wellformedness(|data: &MyData| data.is_good);
    ///
    /// ```
    pub fn with_data_wellformedness<NDWF>(
        self,
        new_data_wellformedness: NDWF,
    ) -> Query<'sg, 'storage, 'rslv, LABEL, DATA, CMPL, PWF, NDWF, LO, DEq>
    where
        NDWF: DataWellformedness<'sg, DATA> + 'rslv,
    {
        Query {
            _phantom: PhantomData,
            scope_graph: self.scope_graph,
            path_wellformedness: self.path_wellformedness,
            data_wellformedness: new_data_wellformedness,
            label_order: self.label_order,
            data_equivalence: self.data_equivalence,
        }
    }

    /// Add a [label order](crate::concepts::label_ordering) to this query.
    /// A label order must implement [`LabelOrder`].
    ///
    /// Defaults to [`DefaultLabelOrder`], considering all labels of equal importance.
    ///
    /// With a label order, you can specify which labels are "more important" in a query.
    /// Specify label orders using the [`label_order!`](crate::label_order) macro.
    /// TODO: lower is better? from Lace.
    ///
    /// ```rust
    /// # use scopegraphs::{query_regex, ScopeGraph, Storage};
    /// # use scopegraphs::completeness::UncheckedCompleteness;
    /// # use scopegraphs::Label;
    /// # let storage = Storage::new();
    /// # let scopegraph = ScopeGraph::<Lbl, (), _>::new(&storage, unsafe{UncheckedCompleteness::new()});
    /// use scopegraphs_macros::label_order;
    ///
    /// #[derive(Label, Copy, Clone, PartialEq, Eq)]
    /// pub enum Lbl {
    ///     Lexical,
    ///     Definition
    /// }
    /// use Lbl::*;
    ///
    /// scopegraph.query()
    ///     .with_label_order(label_order!(Lbl: Lexical < Definition));
    /// ```
    pub fn with_label_order<NLO>(
        self,
        new_label_order: NLO,
    ) -> Query<'sg, 'storage, 'rslv, LABEL, DATA, CMPL, PWF, DWF, NLO, DEq>
    where
        NLO: LabelOrder<LABEL> + 'rslv,
    {
        Query {
            _phantom: PhantomData,
            scope_graph: self.scope_graph,
            path_wellformedness: self.path_wellformedness,
            data_wellformedness: self.data_wellformedness,
            label_order: new_label_order,
            data_equivalence: self.data_equivalence,
        }
    }

    /// Add a [data equivalence](crate::concepts::data_equivalence) to this query.
    /// A data equivalence must implement [`DataEquivalence`].
    ///
    /// TODO: example (@aron?)
    pub fn with_data_equivalence<NDEq>(
        self,
        new_data_equivalence: NDEq,
    ) -> Query<'sg, 'storage, 'rslv, LABEL, DATA, CMPL, PWF, DWF, LO, NDEq>
    where
        NDEq: DataEquivalence<'sg, DATA> + 'rslv,
    {
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

impl<'storage, LABEL: Label, DATA, CMPL> ScopeGraph<'storage, LABEL, DATA, CMPL> {
    /// Build a query over the scope graph.
    pub fn query<'rslv>(
        &'rslv self,
    ) -> Query<
        'storage,
        'rslv,
        'rslv, // TODO: remove(???)
        LABEL,
        DATA,
        CMPL,
        (),
        DefaultDataWellformedness,
        DefaultLabelOrder,
        DefaultDataEquivalence,
    >
    where
        'storage: 'rslv,
    {
        Query {
            _phantom: PhantomData,
            scope_graph: self,
            path_wellformedness: (),
            data_wellformedness: DefaultDataWellformedness::default(),
            label_order: DefaultLabelOrder::default(),
            data_equivalence: DefaultDataEquivalence::default(),
        }
    }
}
