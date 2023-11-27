use std::hash::Hash;
use std::iter;

use crate::label::Label;
use scopegraphs_regular_expressions::RegexMatcher;

use super::{Env, Path, ResolvedPath};
use crate::scopegraph::ScopeGraph;

pub trait DataWellformedness<DATA>: for<'sg> Fn(&'sg DATA) -> bool {}
impl<DATA, T> DataWellformedness<DATA> for T where for<'sg> T: Fn(&'sg DATA) -> bool {}

pub trait LabelOrder<LABEL>: Fn(&EdgeOrData<LABEL>, &EdgeOrData<LABEL>) -> bool {}
impl<LABEL, T> LabelOrder<LABEL> for T where T: Fn(&EdgeOrData<LABEL>, &EdgeOrData<LABEL>) -> bool {}

pub trait DataOrder<DATA>: for<'sg> Fn(&'sg DATA, &'sg DATA) -> bool {}
impl<DATA, T> DataOrder<DATA> for T where for<'sg> T: Fn(&'sg DATA, &'sg DATA) -> bool {}

pub enum EdgeOrData<'lbl, LABEL> {
    Data,
    Edge(&'lbl LABEL),
}

// custom implementation not to impose LABEL: Copy
impl<LABEL> Clone for EdgeOrData<'_, LABEL> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<LABEL> Copy for EdgeOrData<'_, LABEL> {}

pub fn resolve<'sg, 'lbl, SCOPE, LABEL: 'lbl, DATA>(
    sg: &'sg ScopeGraph<SCOPE, LABEL, DATA>,
    path_wellformedness: &mut impl RegexMatcher<&'lbl LABEL>,
    data_wellformedness: &impl DataWellformedness<DATA>,
    label_order: &impl LabelOrder<LABEL>,
    data_order: &impl DataOrder<DATA>,
    source: &'sg SCOPE,
) -> Env<'sg, 'lbl, SCOPE, LABEL, DATA>
where
    LABEL: Label<'lbl>,
    SCOPE: Hash + Eq,
    ResolvedPath<'sg, 'lbl, SCOPE, LABEL, DATA>: Hash + Eq,
    Path<'sg, 'lbl, SCOPE, LABEL>: Clone,
{
    let all_edges: Vec<EdgeOrData<LABEL>> = LABEL::iter_ref()
        .map(EdgeOrData::Edge)
        .chain(iter::once(EdgeOrData::Data))
        .collect();

    let context = ResolutionContext {
        all_edges,
        sg,
        data_wellformedness,
        label_order,
        data_order,
    };

    context.resolve_all(path_wellformedness, &Path::new(source))
}

struct ResolutionContext<'sg, 'lbl, 'query, SCOPE, LABEL, DATA, DWF, LO, DO> {
    all_edges: Vec<EdgeOrData<'lbl, LABEL>>,
    sg: &'sg ScopeGraph<SCOPE, LABEL, DATA>,
    data_wellformedness: &'query DWF,
    label_order: &'query LO,
    data_order: &'query DO,
}

impl<'sg, 'lbl, 'query, SCOPE, LABEL: 'lbl, DATA, DWF, LO, DO>
    ResolutionContext<'sg, 'lbl, 'query, SCOPE, LABEL, DATA, DWF, LO, DO>
where
    SCOPE: Eq + Hash,
    ResolvedPath<'sg, 'lbl, SCOPE, LABEL, DATA>: Hash + Eq,
    DO: DataOrder<DATA>,
    DWF: DataWellformedness<DATA>,
    LO: LabelOrder<LABEL>,
    Path<'sg, 'lbl, SCOPE, LABEL>: Clone,
{
    fn resolve_all(
        &self,
        path_wellformedness: &mut impl RegexMatcher<&'lbl LABEL>,
        path: &Path<'sg, 'lbl, SCOPE, LABEL>,
    ) -> Env<'sg, 'lbl, SCOPE, LABEL, DATA> {
        let edges: Vec<_> = self
            .all_edges
            .iter()
            .copied()
            .filter(|e| match *e {
                EdgeOrData::Data => path_wellformedness.is_accepting(),
                EdgeOrData::Edge(label) => path_wellformedness.accepts([label]),
            })
            .collect();

        self.resolve_edges(path_wellformedness, &edges, path)
    }

    fn resolve_edges(
        &self,
        path_wellformedness: &mut impl RegexMatcher<&'lbl LABEL>,
        edges: &[EdgeOrData<'lbl, LABEL>],
        path: &Path<'sg, 'lbl, SCOPE, LABEL>,
    ) -> Env<'sg, 'lbl, SCOPE, LABEL, DATA> {
        let max = self.max(edges);
        let mut env: Env<SCOPE, LABEL, DATA> = Env::new();

        for edge in max {
            let smaller = self.smaller(edge, edges);
            env.merge(self.resolve_shadow(path_wellformedness, edge, &smaller, path))
        }

        env
    }

    fn resolve_shadow(
        &self,
        path_wellformedness: &mut impl RegexMatcher<&'lbl LABEL>,
        edge: EdgeOrData<'lbl, LABEL>,
        edges: &[EdgeOrData<'lbl, LABEL>],
        path: &Path<'sg, 'lbl, SCOPE, LABEL>,
    ) -> Env<'sg, 'lbl, SCOPE, LABEL, DATA> {
        let mut env = Env::new();
        env.merge(self.resolve_edges(path_wellformedness, edges, path));
        let new = self
            .resolve_edge(path_wellformedness, edge, path)
            .into_iter()
            .filter(|p1| !env.iter().any(|p2| (self.data_order)(p1.data, p2.data)))
            .collect::<Vec<_>>();
        for path in new {
            env.insert(path)
        }

        env
    }

    fn resolve_edge(
        &self,
        path_wellformedness: &mut impl RegexMatcher<&'lbl LABEL>,
        edge: EdgeOrData<'lbl, LABEL>,
        path: &Path<'sg, 'lbl, SCOPE, LABEL>,
    ) -> Env<'sg, 'lbl, SCOPE, LABEL, DATA> {
        match edge {
            EdgeOrData::Edge(label) => self.resolve_label(path_wellformedness, label, path),
            EdgeOrData::Data => self.resolve_data(path),
        }
    }

    fn resolve_label(
        &self,
        path_wellformedness: &mut impl RegexMatcher<&'lbl LABEL>,
        label: &'lbl LABEL,
        path: &Path<'sg, 'lbl, SCOPE, LABEL>,
    ) -> Env<'sg, 'lbl, SCOPE, LABEL, DATA> {
        path_wellformedness.step(label);
        let mut env = Env::new();
        for tgt in self.sg.get_edges(path.target(), label) {
            if let Some(p) = path.step(label, tgt) {
                env.merge(self.resolve_all(path_wellformedness, &p))
            }
        }

        env
    }

    fn resolve_data(
        &self,
        path: &Path<'sg, 'lbl, SCOPE, LABEL>,
    ) -> Env<'sg, 'lbl, SCOPE, LABEL, DATA> {
        let data = self.sg.get_data(path.target());
        if (self.data_wellformedness)(data) {
            Env::single(path.clone().resolve(data))
        } else {
            Env::new()
        }
    }

    fn max(&self, edges: &[EdgeOrData<'lbl, LABEL>]) -> Vec<EdgeOrData<'lbl, LABEL>> {
        edges
            .iter()
            .filter(|l| !edges.iter().any(|ll| (self.label_order)(l, ll)))
            .copied()
            .collect()
    }

    fn smaller(
        &self,
        edge: EdgeOrData<'lbl, LABEL>,
        edges: &[EdgeOrData<'lbl, LABEL>],
    ) -> Vec<EdgeOrData<'lbl, LABEL>> {
        edges
            .iter()
            .filter(|l| (self.label_order)(l, &edge))
            .copied()
            .collect()
    }
}
