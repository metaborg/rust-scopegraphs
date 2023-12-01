use std::hash::Hash;
use std::iter;

use crate::{
    label::Label,
    scopegraph::{completeness::Completeness, Scope},
};
use scopegraphs_regular_expressions::RegexMatcher;

use super::{Env, Path, ResolvedPath};
use crate::scopegraph::ScopeGraph;

pub trait DataWellformedness<DATA>: for<'sg> Fn(&'sg DATA) -> bool {}
impl<DATA, T> DataWellformedness<DATA> for T where for<'sg> T: Fn(&'sg DATA) -> bool {}

pub trait LabelOrder<LABEL>: Fn(&EdgeOrData<LABEL>, &EdgeOrData<LABEL>) -> bool {}
impl<LABEL, T> LabelOrder<LABEL> for T where T: Fn(&EdgeOrData<LABEL>, &EdgeOrData<LABEL>) -> bool {}

pub trait DataOrder<DATA>: for<'sg> Fn(&'sg DATA, &'sg DATA) -> bool {}
impl<DATA, T> DataOrder<DATA> for T where for<'sg> T: Fn(&'sg DATA, &'sg DATA) -> bool {}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum EdgeOrData<LABEL> {
    Data,
    Edge(LABEL),
}

// custom implementation not to impose LABEL: Copy
impl<LABEL: Copy> Clone for EdgeOrData<LABEL> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<LABEL: Copy> Copy for EdgeOrData<LABEL> {}

pub fn resolve<'sg: 'query, 'query, LABEL, DATA, CMPL>(
    sg: &'sg mut ScopeGraph<LABEL, DATA, CMPL>,
    path_wellformedness: &'query mut impl for<'a> RegexMatcher<&'a LABEL>,
    data_wellformedness: &'query impl DataWellformedness<DATA>,
    label_order: &'query impl LabelOrder<LABEL>,
    data_order: &'query impl DataOrder<DATA>,
    source: Scope,
) -> Env<'sg, LABEL, DATA>
where
    LABEL: Label + Copy,
    CMPL: Completeness<LABEL, DATA>,
    for<'a> CMPL::GetEdgesResult<'a>: Iterator<Item = Scope>,
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
    Path<LABEL>: Clone,
{
    let all_edges: Vec<EdgeOrData<LABEL>> = LABEL::iter()
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

struct ResolutionContext<'sg: 'query, 'query, LABEL, DATA, CMPL, DWF, LO, DO> {
    all_edges: Vec<EdgeOrData<LABEL>>,
    sg: &'sg ScopeGraph<LABEL, DATA, CMPL>,
    data_wellformedness: &'query DWF,
    label_order: &'query LO,
    data_order: &'query DO,
}

impl<'sg, 'query, LABEL, DATA, CMPL, DWF, LO, DO>
    ResolutionContext<'sg, 'query, LABEL, DATA, CMPL, DWF, LO, DO>
where
    LABEL: Copy,
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
    CMPL: Completeness<LABEL, DATA>,
    for<'a> CMPL::GetEdgesResult<'a>: Iterator<Item = Scope>,
    DO: DataOrder<DATA>,
    DWF: DataWellformedness<DATA>,
    LO: LabelOrder<LABEL>,
    Path<LABEL>: Clone,
{
    fn resolve_all(
        &self,
        path_wellformedness: &mut impl for<'a> RegexMatcher<&'a LABEL>,
        path: &Path<LABEL>,
    ) -> Env<'sg, LABEL, DATA> {
        let edges: Vec<_> = self
            .all_edges
            .iter()
            .copied()
            .filter(|e| match *e {
                EdgeOrData::Data => path_wellformedness.is_accepting(),
                EdgeOrData::Edge(label) => path_wellformedness.accepts([&label]),
            })
            .collect();

        self.resolve_edges(path_wellformedness, &edges, path)
    }

    fn resolve_edges(
        &self,
        path_wellformedness: &mut impl for<'a> RegexMatcher<&'a LABEL>,
        edges: &[EdgeOrData<LABEL>],
        path: &Path<LABEL>,
    ) -> Env<'sg, LABEL, DATA> {
        let max = self.max(edges);
        let mut env: Env<LABEL, DATA> = Env::new();

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
        path: &Path<LABEL>,
    ) -> Env<'sg, LABEL, DATA> {
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
        path: &Path<LABEL>,
    ) -> Env<'sg, LABEL, DATA> {
        match edge {
            EdgeOrData::Edge(label) => self.resolve_label(path_wellformedness, label, path),
            EdgeOrData::Data => self.resolve_data(path),
        }
    }

    fn resolve_label(
        &self,
        path_wellformedness: &mut impl for<'a> RegexMatcher<&'a LABEL>,
        label: LABEL,
        path: &Path<LABEL>,
    ) -> Env<'sg, LABEL, DATA> {
        path_wellformedness.step(&label);
        let mut env = Env::new();
        let targets = self.sg.get_edges(path.target(), &label);
        for tgt in targets {
            if let Some(p) = path.step(label, tgt) {
                let sub_env = self.resolve_all(path_wellformedness, &p);
                env.merge(sub_env)
            }
        }

        env
    }

    fn resolve_data(&self, path: &Path<LABEL>) -> Env<'sg, LABEL, DATA> {
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

#[cfg(test)]
mod tests {
    use scopegraphs_macros::Label;

    use crate::scopegraph::{completeness::ImplicitClose, ScopeGraph};

    #[test]
    fn test_traverse_single() {
        #[derive(Label, Hash, PartialEq, Eq)]
        enum Lbl {
            Lex,
            Def,
        }

        let mut scope_graph: ScopeGraph<Lbl, usize, ImplicitClose<Lbl>> =
            ScopeGraph::new(ImplicitClose::default());

        let s1 = scope_graph.new_scope(42);
    }
}
