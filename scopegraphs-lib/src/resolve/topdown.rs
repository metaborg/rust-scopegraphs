use std::iter;
use std::{cell::RefCell, hash::Hash};

use crate::{
    label::Label,
    scopegraph::{Completeness, Scope},
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
pub enum EdgeOrData<'sg, LABEL> {
    Data,
    Edge(&'sg LABEL),
}

// custom implementation not to impose LABEL: Copy
impl<LABEL> Clone for EdgeOrData<'_, LABEL> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<LABEL> Copy for EdgeOrData<'_, LABEL> {}

pub fn resolve<'sg: 'query, 'query, LABEL: 'sg, DATA, CMPL>(
    sg: &'sg mut ScopeGraph<LABEL, DATA, CMPL>,
    path_wellformedness: &'query mut impl RegexMatcher<&'query LABEL>,
    data_wellformedness: &'query impl DataWellformedness<DATA>,
    label_order: &'query impl LabelOrder<LABEL>,
    data_order: &'query impl DataOrder<DATA>,
    source: Scope,
) -> Env<'sg, LABEL, DATA>
where
    LABEL: Label<'sg>,
    CMPL: Completeness<LABEL, DATA>,
    for<'a> CMPL::GetEdgesResult<'a>: Iterator<Item = Scope>,
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
    Path<'sg, LABEL>: Clone,
{
    let all_edges: Vec<EdgeOrData<LABEL>> = LABEL::iter_ref()
        .map(EdgeOrData::Edge)
        .chain(iter::once(EdgeOrData::Data))
        .collect();

    let context = ResolutionContext {
        all_edges,
        sg: RefCell::new(sg),
        data_wellformedness,
        label_order,
        data_order,
    };

    context.resolve_all(path_wellformedness, &Path::new(source))
}

struct ResolutionContext<'sg: 'query, 'query, LABEL, DATA, CMPL, DWF, LO, DO> {
    all_edges: Vec<EdgeOrData<'query, LABEL>>,
    sg: RefCell<&'sg mut ScopeGraph<LABEL, DATA, CMPL>>,
    data_wellformedness: &'query DWF,
    label_order: &'query LO,
    data_order: &'query DO,
}

impl<'sg, 'query, LABEL: 'sg, DATA, CMPL, DWF, LO, DO>
    ResolutionContext<'sg, 'query, LABEL, DATA, CMPL, DWF, LO, DO>
where
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
    CMPL: Completeness<LABEL, DATA>,
    for<'a> CMPL::GetEdgesResult<'a>: Iterator<Item = Scope>,
    DO: DataOrder<DATA>,
    DWF: DataWellformedness<DATA>,
    LO: LabelOrder<LABEL>,
    Path<'sg, LABEL>: Clone,
{
    fn resolve_all(
        &self,
        path_wellformedness: &mut impl RegexMatcher<&'query LABEL>,
        path: &Path<'sg, LABEL>,
    ) -> Env<'sg, LABEL, DATA> {
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
        path_wellformedness: &mut impl RegexMatcher<&'query LABEL>,
        edges: &[EdgeOrData<'query, LABEL>],
        path: &Path<'sg, LABEL>,
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
        path_wellformedness: &mut impl RegexMatcher<&'query LABEL>,
        edge: EdgeOrData<'query, LABEL>,
        edges: &[EdgeOrData<'query, LABEL>],
        path: &Path<'sg, LABEL>,
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
        path_wellformedness: &mut impl RegexMatcher<&'query LABEL>,
        edge: EdgeOrData<'query, LABEL>,
        path: &Path<'sg, LABEL>,
    ) -> Env<'sg, LABEL, DATA> {
        match edge {
            EdgeOrData::Edge(label) => self.resolve_label(path_wellformedness, label, path),
            EdgeOrData::Data => self.resolve_data(path),
        }
    }

    fn resolve_label(
        &self,
        path_wellformedness: &mut impl RegexMatcher<&'sg LABEL>,
        label: &'sg LABEL,
        path: &Path<'sg, LABEL>,
    ) -> Env<'sg, LABEL, DATA> {
        path_wellformedness.step(label);
        let mut env = Env::new();
        let mut sg = self.sg.borrow_mut();
        let targets = sg.get_edges(path.target(), label);
        for tgt in targets {
            if let Some(p) = path.step(label, tgt) {
                env.merge(self.resolve_all(path_wellformedness, &p))
            }
        }

        env
    }

    fn resolve_data(&self, path: &Path<'sg, LABEL>) -> Env<'sg, LABEL, DATA> {
        let data = self.sg.borrow().get_data(path.target());
        if (self.data_wellformedness)(data) {
            Env::single(path.clone().resolve(data))
        } else {
            Env::new()
        }
    }

    fn max(&self, edges: &[EdgeOrData<'query, LABEL>]) -> Vec<EdgeOrData<'query, LABEL>> {
        edges
            .iter()
            .filter(|l| !edges.iter().any(|ll| (self.label_order)(l, ll)))
            .copied()
            .collect()
    }

    fn smaller(
        &self,
        edge: EdgeOrData<'query, LABEL>,
        edges: &[EdgeOrData<'query, LABEL>],
    ) -> Vec<EdgeOrData<'query, LABEL>> {
        edges
            .iter()
            .filter(|l| (self.label_order)(l, &edge))
            .copied()
            .collect()
    }
}
