use std::fmt::{Debug, Formatter};
use std::iter;
use std::{borrow::Borrow, hash::Hash};

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

pub fn resolve<'sg: 'query, 'query, LABEL, DATA, CMPL>(
    sg: &'sg ScopeGraph<LABEL, DATA, CMPL>,
    path_wellformedness: &'query impl for<'a> RegexMatcher<&'a LABEL>,
    data_wellformedness: &'query impl DataWellformedness<DATA>,
    label_order: &'query impl LabelOrder<LABEL>,
    data_order: &'query impl DataOrder<DATA>,
    source: Scope,
) -> Env<'sg, LABEL, DATA>
where
    LABEL: Label + Copy + Debug,
    DATA: Debug,
    CMPL: Completeness<LABEL, DATA>,
    CMPL::GetEdgesResult: Borrow<[Scope]>,
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
    LABEL: Copy + Debug,
    DATA: Debug,
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
    CMPL: Completeness<LABEL, DATA>,
    CMPL::GetEdgesResult: Borrow<[Scope]>,
    DO: DataOrder<DATA>,
    DWF: DataWellformedness<DATA>,
    LO: LabelOrder<LABEL>,
    Path<LABEL>: Clone,
{
    fn resolve_all(
        &self,
        path_wellformedness: &impl for<'a> RegexMatcher<&'a LABEL>,
        path: &Path<LABEL>,
    ) -> Env<'sg, LABEL, DATA> {
        let edges: Vec<_> = self
            .all_edges
            .iter()
            .copied()
            .filter(|e| match *e {
                EdgeOrData::Data => path_wellformedness.is_accepting(),
                EdgeOrData::Edge(label) => path_wellformedness.accepts_prefix([&label]),
            })
            .collect();

        log::info!("Resolving edges {:?} in {:?}", edges, path.target());
        self.resolve_edges(path_wellformedness, &edges, path)
    }

    fn resolve_edges(
        &self,
        path_wellformedness: &impl for<'a> RegexMatcher<&'a LABEL>,
        edges: &[EdgeOrData<LABEL>],
        path: &Path<LABEL>,
    ) -> Env<'sg, LABEL, DATA> {
        let tgt = path.target();
        let max = self.max(edges);
        let mut env: Env<LABEL, DATA> = Env::new();

        log::info!("Resolving max-edges {:?} in {:?}", max, tgt);
        for edge in max {
            let smaller = self.smaller(edge, edges);
            log::info!(
                "Resolving(shadowed) {:?} < {:?} in {:?}",
                smaller,
                edge,
                tgt
            );
            env.merge(self.resolve_shadow(path_wellformedness, edge, &smaller, path))
        }

        log::info!("{:?}-result: {:?}", edges, env);
        env
    }

    fn resolve_shadow(
        &self,
        path_wellformedness: &impl for<'a> RegexMatcher<&'a LABEL>,
        edge: EdgeOrData<LABEL>,
        edges: &[EdgeOrData<LABEL>],
        path: &Path<LABEL>,
    ) -> Env<'sg, LABEL, DATA> {
        let mut env = Env::new();
        env.merge(self.resolve_edges(path_wellformedness, edges, path));
        let new = self
            .resolve_edge(path_wellformedness, edge, path)
            .into_iter()
            .filter(|p1| {
                if let Some(p2) = env.iter().find(|p2| (self.data_order)(p1.data, p2.data)) {
                    log::info!(
                        "Discarding {:?} in {:?}; shadowed by {:?} in {:?}",
                        p1.data,
                        p1.path.target(),
                        p2.data,
                        p2.path.target()
                    );
                    false
                } else {
                    true
                }
            })
            .collect::<Vec<_>>();
        log::info!(
            "Merging {:?}-result [{:?}] in {:?}-result [{:?}]",
            edge,
            new,
            edges,
            env
        );
        for path in new {
            env.insert(path)
        }

        env
    }

    fn resolve_edge(
        &self,
        path_wellformedness: &impl for<'a> RegexMatcher<&'a LABEL>,
        edge: EdgeOrData<LABEL>,
        path: &Path<LABEL>,
    ) -> Env<'sg, LABEL, DATA> {
        match edge {
            EdgeOrData::Edge(label) => {
                self.resolve_label(&mut path_wellformedness.clone(), label, path)
            }
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
        let source = path.target();
        let mut env = Env::new();
        let targets = self.sg.get_edges(source, label);
        // targets.cloned()
        for tgt in targets.borrow() {
            if let Some(p) = path.step(label, *tgt) {
                log::info!("Traversing {:?} -{:?}-> {:?}.", source, label, tgt);
                let sub_env = self.resolve_all(path_wellformedness, &p);
                env.merge(sub_env)
            }
        }

        env
    }

    fn resolve_data(&self, path: &Path<LABEL>) -> Env<'sg, LABEL, DATA> {
        let data = self.sg.get_data(path.target());
        if (self.data_wellformedness)(data) {
            log::info!("{:?} matched: return singleton env.", data);
            Env::single(path.clone().resolve(data))
        } else {
            log::info!("{:?} not matched: return empty env.", data);
            Env::new()
        }
    }

    fn max(&self, edges: &[EdgeOrData<LABEL>]) -> Vec<EdgeOrData<LABEL>> {
        let max = edges
            .iter()
            .filter(|l| !edges.iter().any(|ll| (self.label_order)(l, ll)))
            .copied()
            .collect();

        log::info!("max({:?}) = {:?}", edges, max);
        max
    }

    fn smaller(
        &self,
        edge: EdgeOrData<LABEL>,
        edges: &[EdgeOrData<LABEL>],
    ) -> Vec<EdgeOrData<LABEL>> {
        let smaller = edges
            .iter()
            .filter(|l| (self.label_order)(l, &edge))
            .copied()
            .collect();

        log::info!("smaller({:?}, {:?}) = {:?}", edge, edges, smaller);
        smaller
    }
}

#[cfg(test)]
mod tests {
    use scopegraphs_macros::{compile_regex, Label};

    use scopegraphs::{
        resolve::{
            topdown::{resolve, EdgeOrData},
            ResolvedPath,
        },
        scopegraph::{completeness::UncheckedCompleteness, ScopeGraph},
    };

    #[derive(Label, Hash, PartialEq, Eq, Debug, Clone, Copy)]
    enum Lbl {
        Lex,
        Imp,
        Def,
    }
    use Lbl::*;

    #[derive(Hash, PartialEq, Eq, Debug)]
    enum TData<'a, T> {
        NoData,
        Data { name: &'a str, data: T },
    }
    use TData::*;

    impl<'a, T> TData<'a, T> {
        fn matches(&self, n: &str) -> bool {
            match self {
                NoData => false,
                Data { name, .. } => *name == n,
            }
        }
    }
    impl<'a, T: Default> TData<'a, T> {
        fn from_default(name: &'a str) -> Self {
            Data {
                name,
                data: T::default(),
            }
        }
    }

    #[ctor::ctor]
    fn init() {
        env_logger::init();
    }

    #[test]
    fn test_match_data() {
        let mut scope_graph: ScopeGraph<Lbl, TData<()>, UncheckedCompleteness> = ScopeGraph::raw();

        let s0 = scope_graph.new_scope(NoData);
        scope_graph.new_decl(s0, Def, TData::from_default("x"));

        compile_regex!(type Machine<Lbl> = Def);

        let env = resolve(
            &scope_graph,
            &Machine::new(),
            &|x| x.matches("x"),
            &|_, _| false,
            &|_, _| true,
            s0,
        );

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(1, env_vec.len());

        let path: &ResolvedPath<Lbl, TData<()>> = &env_vec[0];
        assert!(path.data().matches("x"))
    }

    #[test]
    fn test_no_match_other_data() {
        let mut scope_graph: ScopeGraph<Lbl, TData<()>, UncheckedCompleteness> = ScopeGraph::raw();

        let s0 = scope_graph.new_scope(NoData);
        scope_graph.new_decl(s0, Def, TData::from_default("x"));

        compile_regex!(type Machine<Lbl> = Def);

        let env = resolve(
            &scope_graph,
            &Machine::new(),
            &|x| x.matches("y"),
            &|_, _| false,
            &|_, _| true,
            s0,
        );

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(0, env_vec.len());
    }

    #[test]
    fn test_regex_enforces_step() {
        let mut scope_graph: ScopeGraph<Lbl, TData<usize>, UncheckedCompleteness> =
            ScopeGraph::raw();

        let s0 = scope_graph.new_scope(NoData);
        let s1 = scope_graph.new_scope(NoData);
        scope_graph.new_edge(s0, Lex, s1);
        scope_graph.new_decl(s0, Def, Data { name: "x", data: 0 });
        scope_graph.new_decl(s1, Def, Data { name: "x", data: 1 });

        compile_regex!(type Machine<Lbl> = Lex Def);

        let env = resolve(
            &scope_graph,
            &Machine::new(),
            &|x| x.matches("x"),
            &|_, _| false,
            &|_, _| true,
            s0,
        );

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(1, env_vec.len());

        let path: &ResolvedPath<Lbl, TData<usize>> = &env_vec[0];
        assert!(matches!(path.data(), &Data { name: "x", data: 1 }))
    }
    #[test]
    fn test_regex_filter() {
        let mut scope_graph: ScopeGraph<Lbl, TData<usize>, UncheckedCompleteness> =
            ScopeGraph::raw();

        let s0 = scope_graph.new_scope(NoData);
        let s1 = scope_graph.new_scope(NoData);
        scope_graph.new_edge(s0, Lex, s1);
        scope_graph.new_decl(s0, Def, Data { name: "x", data: 0 });

        compile_regex!(type Machine<Lbl> = Lex Def);

        let env = resolve(
            &scope_graph,
            &Machine::new(),
            &|x| x.matches("x"),
            &|_, _| false,
            &|_, _| true,
            s0,
        );

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(0, env_vec.len());
    }

    #[test]
    fn test_shadow_applied() {
        let mut scope_graph: ScopeGraph<Lbl, TData<usize>, UncheckedCompleteness> =
            ScopeGraph::raw();

        let s0 = scope_graph.new_scope(NoData);
        let s1 = scope_graph.new_scope(NoData);
        let s2 = scope_graph.new_scope(NoData);
        scope_graph.new_edge(s0, Imp, s1);
        scope_graph.new_edge(s0, Lex, s2);
        scope_graph.new_decl(s1, Def, Data { name: "x", data: 0 });
        scope_graph.new_decl(s2, Def, Data { name: "x", data: 1 });

        compile_regex!(type Machine<Lbl> = Lex Def);

        let env = resolve(
            &scope_graph,
            &Machine::new(),
            &|x| x.matches("x"),
            &|l1, l2| matches!((l1, l2), (EdgeOrData::Edge(Lex), EdgeOrData::Edge(Imp))),
            &|_, _| true,
            s0,
        );

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(1, env_vec.len());

        let path: &ResolvedPath<Lbl, TData<usize>> = &env_vec[0];
        assert!(matches!(path.data(), &Data { name: "x", data: 1 }))
    }

    #[test]
    fn test_label_order_complex() {
        let mut scope_graph: ScopeGraph<Lbl, TData<usize>, UncheckedCompleteness> =
            ScopeGraph::raw();

        let s0 = scope_graph.new_scope(NoData);
        let s_with = scope_graph.new_scope(NoData);
        let s_rec = scope_graph.new_scope(NoData);
        let s_let = scope_graph.new_scope(NoData);

        scope_graph.new_edge(s_with, Lex, s0);
        scope_graph.new_edge(s_with, Imp, s_rec);
        scope_graph.new_edge(s_let, Lex, s_with);

        scope_graph.new_decl(s_rec, Def, Data { name: "x", data: 1 });
        scope_graph.new_decl(s_let, Def, Data { name: "x", data: 2 });

        compile_regex!(type Machine<Lbl> = Lex* Imp? Def);

        let env = resolve(
            &scope_graph,
            &Machine::new(),
            &|x| x.matches("x"),
            &|l1, l2| {
                matches!(
                    (l1, l2),
                    (EdgeOrData::Edge(Imp), EdgeOrData::Edge(Lex))
                        | (EdgeOrData::Edge(Def), EdgeOrData::Edge(Imp))
                        | (EdgeOrData::Edge(Def), EdgeOrData::Edge(Lex))
                )
            },
            &|_, _| true,
            s_let,
        );

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(1, env_vec.len());

        let path: &ResolvedPath<Lbl, TData<usize>> = &env_vec[0];
        assert!(matches!(path.data(), &Data { name: "x", data: 2 }))
    }
}
