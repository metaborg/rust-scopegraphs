use std::{hash::Hash, iter::once};

use scopegraphs_regular_expressions::RegexMatcher;

use super::{Env, Path};
use crate::{label::Label, scopegraph::ScopeGraph};

#[derive(Clone, Copy)]
pub enum EdgeOrData<LABEL> {
    Data,
    Edge(LABEL),
}

pub fn resolve<'sg, SCOPE, LABEL, DATA>(
    sg: &'sg ScopeGraph<SCOPE, LABEL, DATA>,
    path_wellformedness: &mut impl for<'a> RegexMatcher<&'a LABEL>,
    data_wellformedness: &impl for<'a> Fn(&'a DATA) -> bool,
    label_order: &impl Fn(&EdgeOrData<LABEL>, &EdgeOrData<LABEL>) -> bool, // FIXME: LabelOrder trait
    data_order: &impl for<'a> Fn(&'a DATA, &'a DATA) -> bool,
    source: &'sg SCOPE,
) -> Env<'sg, SCOPE, LABEL, DATA>
where
    SCOPE: Hash + Eq + Clone,
    LABEL: Hash + Eq + Label + Copy,
    DATA: Hash + Eq + Clone,
{
    // Ugh: how can I make thos borrowed properly?
    let all_edges: Vec<EdgeOrData<LABEL>> = LABEL::iter()
        .map(EdgeOrData::Edge)
        .chain(once(EdgeOrData::Data))
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

struct ResolutionContext<'sg, 'query, SCOPE, LABEL, DATA, DWF, LO, DO>
where
    DATA: 'query,
    DWF: for<'a> Fn(&'a DATA) -> bool,
    LO: Fn(&EdgeOrData<LABEL>, &EdgeOrData<LABEL>) -> bool,
    DO: for<'a> Fn(&'a DATA, &'a DATA) -> bool,
{
    all_edges: Vec<EdgeOrData<LABEL>>,
    sg: &'sg ScopeGraph<SCOPE, LABEL, DATA>,
    data_wellformedness: &'query DWF,
    label_order: &'query LO, // FIXME: LabelOrder trait
    data_order: &'query DO,
}

impl<'sg, 'query, SCOPE, LABEL, DATA, DWF, LO, DO>
    ResolutionContext<'sg, 'query, SCOPE, LABEL, DATA, DWF, LO, DO>
where
    SCOPE: Hash + Eq + Clone,
    LABEL: Label + Hash + Copy,
    DATA: Hash + Eq + Clone + 'query,
    DWF: for<'a> Fn(&'a DATA) -> bool,
    LO: Fn(&EdgeOrData<LABEL>, &EdgeOrData<LABEL>) -> bool,
    DO: for<'a> Fn(&'a DATA, &'a DATA) -> bool,
{
    fn resolve_all(
        &self,
        path_wellformedness: &mut impl for<'a> RegexMatcher<&'a LABEL>,
        path: &Path<'sg, SCOPE, LABEL>,
    ) -> Env<'sg, SCOPE, LABEL, DATA> {
        let edges: Vec<_> = self
            .all_edges
            .iter()
            .filter(|e| match e {
                EdgeOrData::Data => path_wellformedness.is_accepting(),
                EdgeOrData::Edge(label) => path_wellformedness.accepts([label]),
            })
            .copied()
            .collect();

        self.resolve_edges(path_wellformedness, &edges, path)
    }

    fn resolve_edges(
        &self,
        path_wellformedness: &mut impl for<'a> RegexMatcher<&'a LABEL>,
        edges: &[EdgeOrData<LABEL>],
        path: &Path<'sg, SCOPE, LABEL>,
    ) -> Env<'sg, SCOPE, LABEL, DATA> {
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
        path_wellformedness: &mut impl for<'a> RegexMatcher<&'a LABEL>,
        edge: EdgeOrData<LABEL>,
        edges: &[EdgeOrData<LABEL>],
        path: &Path<'sg, SCOPE, LABEL>,
    ) -> Env<'sg, SCOPE, LABEL, DATA> {
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
        path_wellformedness: &mut impl for<'a> RegexMatcher<&'a LABEL>,
        edge: EdgeOrData<LABEL>,
        path: &Path<'sg, SCOPE, LABEL>,
    ) -> Env<'sg, SCOPE, LABEL, DATA> {
        match edge {
            EdgeOrData::Edge(label) => self.resolve_label(path_wellformedness, label, path),
            EdgeOrData::Data => self.resolve_data(path),
        }
    }

    fn resolve_label(
        &self,
        path_wellformedness: &mut impl for<'a> RegexMatcher<&'a LABEL>,
        label: LABEL,
        path: &Path<'sg, SCOPE, LABEL>,
    ) -> Env<'sg, SCOPE, LABEL, DATA> {
        path_wellformedness.step(&label);
        let mut env = Env::new();
        for tgt in self.sg.get_edges(path.target(), &label) {
            path.step(label, tgt)
                .inspect(|p| env.merge(self.resolve_all(path_wellformedness, p)));
        }

        env
    }

    fn resolve_data(&self, path: &Path<'sg, SCOPE, LABEL>) -> Env<'sg, SCOPE, LABEL, DATA> {
        let data = self.sg.get_data(path.target());
        if (self.data_wellformedness)(data) {
            Env::single(path.clone().resolve(data))
        } else {
            Env::new()
        }
    }

    fn max(&self, edges: &[EdgeOrData<LABEL>]) -> Vec<EdgeOrData<LABEL>> {
        edges
            .iter()
            .filter(|l| !edges.iter().any(|ll| (self.label_order)(l, ll)))
            .copied()
            .collect()
    }

    fn smaller(
        &self,
        edge: EdgeOrData<LABEL>,
        edges: &[EdgeOrData<LABEL>],
    ) -> Vec<EdgeOrData<LABEL>> {
        edges
            .iter()
            .filter(|l| (self.label_order)(l, &edge))
            .copied()
            .collect()
    }
}
