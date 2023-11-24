pub mod topdown;

use std::{collections::HashSet, fmt::Debug, hash::Hash, sync::Arc};

use prust_lib::hashmap::{empty, HashSet as TrieSet};

#[derive(Hash, PartialEq, Eq, Debug)]
enum InnerPath<'sg, SCOPE, LABEL>
where
    SCOPE: PartialEq,
{
    Start {
        source: &'sg SCOPE,
    },
    Step {
        prefix: Path<'sg, SCOPE, LABEL>,
        label: LABEL,
        target: &'sg SCOPE,
    },
}

#[derive(Clone)]
pub struct Path<'sg, SCOPE, LABEL>
where
    SCOPE: PartialEq,
{
    inner_path: Arc<InnerPath<'sg, SCOPE, LABEL>>,
    scopes: TrieSet<&'sg SCOPE>,
}

impl<'sg, SCOPE, LABEL> PartialEq for Path<'sg, SCOPE, LABEL>
where
    SCOPE: PartialEq,
    LABEL: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.inner_path == other.inner_path
    }
}

impl<'sg, SCOPE, LABEL> Eq for Path<'sg, SCOPE, LABEL>
where
    SCOPE: Eq,
    LABEL: Eq,
{
}

impl<'sg, SCOPE, LABEL> Hash for Path<'sg, SCOPE, LABEL>
where
    SCOPE: PartialEq + Hash,
    LABEL: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner_path.hash(state);
    }
}

impl<'sg, SCOPE, LABEL> Debug for Path<'sg, SCOPE, LABEL>
where
    SCOPE: PartialEq + Debug,
    LABEL: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Path")
            .field("inner_path", &self.inner_path)
            .finish()
    }
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub struct ResolvedPath<'sg, SCOPE, LABEL, DATA>
where
    SCOPE: PartialEq,
{
    path: Path<'sg, SCOPE, LABEL>,
    data: &'sg DATA,
}

impl<'sg, SCOPE, LABEL> Path<'sg, SCOPE, LABEL>
where
    SCOPE: Eq + Hash,
    LABEL: Eq,
{
    pub fn new(source: &'sg SCOPE) -> Self {
        Self {
            inner_path: Arc::new(InnerPath::Start { source }),
            scopes: empty().insert(source),
        }
    }

    pub fn target(&self) -> &SCOPE {
        match self.inner_path.as_ref() {
            InnerPath::Start { source } => source,
            InnerPath::Step { target, .. } => target,
        }
    }

    pub fn step(&self, label: LABEL, target: &'sg SCOPE) -> Option<Self> {
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

    pub fn resolve<DATA>(self, data: &'sg DATA) -> ResolvedPath<SCOPE, LABEL, DATA> {
        ResolvedPath { path: self, data }
    }
}

// For now, we stick with hashmaps because they are easy.
// We might however want to change that in the future, because:
// - we currently create a lot of new hashmaps, which is not really efficient
// - efficiency might be dependent on the name resolution (shadowing) strategy
// Perhaps we will resort to fibbonacy heaps/pairing heaps, and/or make resolution parametric in the environment type.
pub struct Env<'sg, SCOPE: PartialEq, LABEL, DATA>(HashSet<ResolvedPath<'sg, SCOPE, LABEL, DATA>>);

impl<'sg, SCOPE: PartialEq, LABEL, DATA> IntoIterator for Env<'sg, SCOPE, LABEL, DATA> {
    type Item = ResolvedPath<'sg, SCOPE, LABEL, DATA>;

    type IntoIter = std::collections::hash_set::IntoIter<ResolvedPath<'sg, SCOPE, LABEL, DATA>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'sg, SCOPE, LABEL, DATA> Default for Env<'sg, SCOPE, LABEL, DATA>
where
    SCOPE: Hash + Eq,
    LABEL: Hash + Eq,
    DATA: Hash + Eq,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<'sg, SCOPE, LABEL, DATA> Env<'sg, SCOPE, LABEL, DATA>
where
    SCOPE: Hash + Eq,
    LABEL: Hash + Eq,
    DATA: Hash + Eq,
{
    pub fn new() -> Self {
        Self(HashSet::new())
    }

    pub fn single(path: ResolvedPath<'sg, SCOPE, LABEL, DATA>) -> Self {
        let mut env = Env::new();
        env.insert(path);
        env
    }

    pub fn insert(&mut self, path: ResolvedPath<'sg, SCOPE, LABEL, DATA>) {
        self.0.insert(path);
    }

    pub fn merge(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    pub fn iter<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a ResolvedPath<'sg, SCOPE, LABEL, DATA>> + 'a {
        self.0.iter()
    }
}
