use prust_lib::hashmap::HashSet as TrieSet;
use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use crate::scopegraph::Scope;

pub mod generic_resolution;
pub mod topdown;

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
    scopes: TrieSet<Scope>,
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

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub struct ResolvedPath<'sg, LABEL, DATA> {
    path: Path<LABEL>,
    data: &'sg DATA,
}

impl<LABEL> Path<LABEL> {
    pub fn new(source: Scope) -> Self {
        Self {
            inner_path: Arc::new(InnerPath::Start { source }),
            scopes: TrieSet::new().insert(source),
        }
    }

    pub fn target(&self) -> Scope {
        match self.inner_path.as_ref() {
            InnerPath::Start { source } => *source,
            InnerPath::Step { target, .. } => *target,
        }
    }

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
                scopes: self.scopes.insert(target),
            })
        }
    }

    pub fn resolve<DATA>(self, data: &DATA) -> ResolvedPath<'_, LABEL, DATA> {
        ResolvedPath { path: self, data }
    }
}

// For now, we stick with hashmaps because they are easy.
// We might however want to change that in the future, because:
// - we currently create a lot of new hashmaps, which is not really efficient
// - efficiency might be dependent on the name resolution (shadowing) strategy
// Perhaps we will resort to fibbonacy heaps/pairing heaps, and/or make resolution parametric in the environment type.
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
    pub fn new() -> Self {
        Self(HashSet::new())
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a ResolvedPath<'sg, LABEL, DATA>> + 'a {
        self.0.iter()
    }
}

impl<'sg, LABEL, DATA> Env<'sg, LABEL, DATA>
where
    ResolvedPath<'sg, LABEL, DATA>: Eq + Hash,
{
    pub fn single(path: ResolvedPath<'sg, LABEL, DATA>) -> Self {
        let mut env = Env::new();
        env.insert(path);
        env
    }

    pub fn insert(&mut self, path: ResolvedPath<'sg, LABEL, DATA>) {
        self.0.insert(path);
    }

    pub fn merge(&mut self, other: Self) {
        self.0.extend(other.0)
    }
}
