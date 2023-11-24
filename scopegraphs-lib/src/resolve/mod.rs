pub mod topdown;

use std::{collections::HashSet, hash::Hash, sync::Arc};

#[derive(Hash, PartialEq, Eq, Debug)]
enum InnerPath<'sg, SCOPE, LABEL> {
    Start {
        source: &'sg SCOPE,
    },
    Step {
        prefix: Path<'sg, SCOPE, LABEL>,
        label: LABEL,
        target: &'sg SCOPE,
    },
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub struct Path<'sg, SCOPE, LABEL>(Arc<InnerPath<'sg, SCOPE, LABEL>>);

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub struct ResolvedPath<'sg, SCOPE, LABEL, DATA> {
    path: Path<'sg, SCOPE, LABEL>,
    data: &'sg DATA,
}

impl<'sg, SCOPE, LABEL> Path<'sg, SCOPE, LABEL> {
    pub fn new(source: &'sg SCOPE) -> Self {
        Self(Arc::new(InnerPath::Start { source }))
    }

    pub fn target(&self) -> &SCOPE {
        match self.0.as_ref() {
            InnerPath::Start { source } => source,
            InnerPath::Step { target, .. } => target,
        }
    }

    pub fn step(&self, label: LABEL, target: &'sg SCOPE) -> Self {
        Self(Arc::new(InnerPath::Step {
            prefix: Self(self.0.clone()),
            label,
            target,
        }))
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
pub struct Env<'sg, SCOPE, LABEL, DATA>(HashSet<ResolvedPath<'sg, SCOPE, LABEL, DATA>>);

impl<'sg, SCOPE, LABEL, DATA> IntoIterator for Env<'sg, SCOPE, LABEL, DATA> {
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
