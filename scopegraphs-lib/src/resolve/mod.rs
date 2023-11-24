use prust_lib::hashmap::HashSet as TrieSet;
use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

pub mod topdown;

#[derive(Hash, PartialEq, Eq, Debug)]
enum InnerPath<'sg, 'lbl, SCOPE, LABEL: 'lbl> {
    Start {
        source: &'sg SCOPE,
    },
    Step {
        prefix: Path<'sg, 'lbl, SCOPE, LABEL>,
        label: &'lbl LABEL,
        target: &'sg SCOPE,
    },
}

#[derive(Clone)]
pub struct Path<'sg, 'lbl, SCOPE, LABEL> {
    inner_path: Arc<InnerPath<'sg, 'lbl, SCOPE, LABEL>>,
    scopes: TrieSet<&'sg SCOPE>,
}

impl<SCOPE, LABEL> PartialEq for Path<'_, '_, SCOPE, LABEL>
where
    SCOPE: PartialEq,
    LABEL: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.inner_path == other.inner_path
    }
}

impl<SCOPE, LABEL> Eq for Path<'_, '_, SCOPE, LABEL>
where
    SCOPE: Eq,
    LABEL: Eq,
{
}

impl<SCOPE, LABEL> Hash for Path<'_, '_, SCOPE, LABEL>
where
    SCOPE: Hash,
    LABEL: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner_path.hash(state);
    }
}

impl<SCOPE, LABEL> Debug for Path<'_, '_, SCOPE, LABEL>
where
    SCOPE: Debug,
    LABEL: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Path")
            .field("inner_path", &self.inner_path)
            .finish()
    }
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub struct ResolvedPath<'sg, 'lbl, SCOPE, LABEL: 'lbl, DATA> {
    path: Path<'sg, 'lbl, SCOPE, LABEL>,
    data: &'sg DATA,
}

impl<'sg, 'lbl, SCOPE, LABEL> Path<'sg, 'lbl, SCOPE, LABEL>
where
    SCOPE: Eq + Hash,
{
    pub fn new(source: &'sg SCOPE) -> Self {
        Self {
            inner_path: Arc::new(InnerPath::Start { source }),
            scopes: TrieSet::new().insert(source),
        }
    }

    pub fn target(&self) -> &SCOPE {
        match self.inner_path.as_ref() {
            InnerPath::Start { source } => source,
            InnerPath::Step { target, .. } => target,
        }
    }

    pub fn step(&self, label: &'lbl LABEL, target: &'sg SCOPE) -> Option<Self> {
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

    pub fn resolve<DATA>(self, data: &'sg DATA) -> ResolvedPath<'sg, 'lbl, SCOPE, LABEL, DATA> {
        ResolvedPath { path: self, data }
    }
}

// For now, we stick with hashmaps because they are easy.
// We might however want to change that in the future, because:
// - we currently create a lot of new hashmaps, which is not really efficient
// - efficiency might be dependent on the name resolution (shadowing) strategy
// Perhaps we will resort to fibbonacy heaps/pairing heaps, and/or make resolution parametric in the environment type.
pub struct Env<'sg, 'lbl, SCOPE, LABEL: 'lbl, DATA>(
    HashSet<ResolvedPath<'sg, 'lbl, SCOPE, LABEL, DATA>>,
);

impl<'sg, 'lbl, SCOPE, LABEL, DATA> IntoIterator for Env<'sg, 'lbl, SCOPE, LABEL, DATA> {
    type Item = ResolvedPath<'sg, 'lbl, SCOPE, LABEL, DATA>;

    type IntoIter =
        <HashSet<ResolvedPath<'sg, 'lbl, SCOPE, LABEL, DATA>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<SCOPE, LABEL, DATA> Default for Env<'_, '_, SCOPE, LABEL, DATA> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'sg, 'lbl, SCOPE, LABEL, DATA> Env<'sg, 'lbl, SCOPE, LABEL, DATA> {
    pub fn new() -> Self {
        Self(HashSet::new())
    }

    pub fn iter<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a ResolvedPath<'sg, 'lbl, SCOPE, LABEL, DATA>> + 'a {
        self.0.iter()
    }
}

impl<'sg, 'lbl, SCOPE, LABEL, DATA> Env<'sg, 'lbl, SCOPE, LABEL, DATA>
where
    ResolvedPath<'sg, 'lbl, SCOPE, LABEL, DATA>: Eq + Hash,
{
    pub fn single(path: ResolvedPath<'sg, 'lbl, SCOPE, LABEL, DATA>) -> Self {
        let mut env = Env::new();
        env.insert(path);
        env
    }

    pub fn insert(&mut self, path: ResolvedPath<'sg, 'lbl, SCOPE, LABEL, DATA>) {
        self.0.insert(path);
    }

    pub fn merge(&mut self, other: Self) {
        self.0.extend(other.0)
    }
}
