use std::fmt::Debug;
use std::hash::Hash;
use std::iter;

use crate::{
    label::Label,
    scopegraph::ScopeGraph,
    scopegraph::{completeness::Completeness, Scope},
};
use scopegraphs_regular_expressions::RegexMatcher;

use super::{
    generic_resolution::*, DataOrder, DataWellformedness, EdgeOrData, Env, LabelOrder, Path,
    ResolvedPath,
};

pub fn resolve<'sg: 'query, 'query, LABEL, DATA, CMPL, ENVC>(
    sg: &'sg ScopeGraph<LABEL, DATA, CMPL>,
    path_wellformedness: &'query impl for<'a> RegexMatcher<&'a LABEL>,
    data_wellformedness: &'query impl DataWellformedness<DATA>,
    label_order: &'query impl LabelOrder<LABEL>,
    data_order: &'query impl DataOrder<DATA>,
    source: Scope,
) -> ENVC
where
    LABEL: Label + Copy + Debug,
    DATA: Debug,
    CMPL: Completeness<LABEL, DATA>,
    CMPL::GetEdgesResult: ScopeContainer<LABEL>,
    <CMPL::GetEdgesResult as ScopeContainer<LABEL>>::PathContainer:
        PathContainer<LABEL, DATA, EnvContainer<'sg> = ENVC>,
    ENVC: EnvContainer<'sg, LABEL, DATA> + Debug,
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

impl<'sg, 'query, LABEL, DATA, CMPL, DWF, LO, DO, ENVC>
    ResolutionContext<'sg, 'query, LABEL, DATA, CMPL, DWF, LO, DO>
where
    LABEL: Copy + Debug,
    DATA: Debug,
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
    CMPL: Completeness<LABEL, DATA>,
    CMPL::GetEdgesResult: ScopeContainer<LABEL>,
    <CMPL::GetEdgesResult as ScopeContainer<LABEL>>::PathContainer:
        PathContainer<LABEL, DATA, EnvContainer<'sg> = ENVC>,
    ENVC: EnvContainer<'sg, LABEL, DATA> + Debug,
    DO: DataOrder<DATA>,
    DWF: DataWellformedness<DATA>,
    LO: LabelOrder<LABEL>,
    Path<LABEL>: Clone,
{
    fn resolve_all(
        &self,
        path_wellformedness: &impl for<'a> RegexMatcher<&'a LABEL>,
        path: &Path<LABEL>,
    ) -> ENVC {
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
    ) -> ENVC {
        let tgt = path.target();
        let max = self.max(edges);
        let mut env = ENVC::empty();

        log::info!("Resolving max-edges {:?} in {:?}", max, tgt);
        for edge in max {
            let smaller = self.smaller(edge, edges);
            log::info!(
                "Resolving(shadowed) {:?} < {:?} in {:?}",
                smaller,
                edge,
                tgt
            );
            env = env.lift_merge(self.resolve_shadow(path_wellformedness, edge, &smaller, path))
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
    ) -> ENVC {
        let env: ENVC = self.resolve_edges(path_wellformedness, edges, path);
        let sub_env = self.resolve_edge(path_wellformedness, edge, path);
        env.lift_shadow(sub_env, self.data_order)
    }

    fn resolve_edge(
        &self,
        path_wellformedness: &impl for<'a> RegexMatcher<&'a LABEL>,
        edge: EdgeOrData<LABEL>,
        path: &Path<LABEL>,
    ) -> ENVC {
        match edge {
            EdgeOrData::Edge(label) => {
                let mut new_path_wellformedness = path_wellformedness.clone();
                new_path_wellformedness.step(&label);
                self.resolve_label(&new_path_wellformedness, label, path)
            }
            EdgeOrData::Data => self.resolve_data(path),
        }
    }

    fn resolve_label(
        &self,
        path_wellformedness: &impl for<'a> RegexMatcher<&'a LABEL>,
        label: LABEL,
        path: &Path<LABEL>,
    ) -> ENVC {
        let source = path.target();
        let targets = self.sg.get_edges(source, label);
        let path_set = targets.lift_step(label, path);
        let env: ENVC = path_set.map_into_env::<_>(|p| self.resolve_all(path_wellformedness, &p));
        env
    }

    fn resolve_data(&self, path: &Path<LABEL>) -> ENVC {
        let data = self.sg.get_data(path.target());
        if (self.data_wellformedness)(data) {
            log::info!("{:?} matched: return singleton env.", data);
            Env::single(path.clone().resolve(data)).into()
        } else {
            log::info!("{:?} not matched: return empty env.", data);
            Env::new().into()
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
        resolve::{topdown::resolve, EdgeOrData, ResolvedPath},
        scopegraph::{
            completeness::{Completeness, ExplicitClose, ImplicitClose, UncheckedCompleteness},
            Scope, ScopeGraph,
        },
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
    fn test_label_order_complex_raw() {
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

        resolve_complex_unchecked(&mut scope_graph, s_let)
    }

    #[test]
    fn test_label_order_complex_implicit_close() {
        let mut scope_graph: ScopeGraph<Lbl, TData<usize>, ImplicitClose<Lbl>> =
            ScopeGraph::new(ImplicitClose::default());

        let s0 = scope_graph.new_scope(NoData);
        let s_with = scope_graph.new_scope(NoData);
        let s_rec = scope_graph.new_scope(NoData);
        let s_let = scope_graph.new_scope(NoData);

        scope_graph
            .new_edge(s_with, Lex, s0)
            .expect("edge closed unexpectedly");
        scope_graph
            .new_edge(s_with, Imp, s_rec)
            .expect("edge closed unexpectedly");
        scope_graph
            .new_edge(s_let, Lex, s_with)
            .expect("edge closed unexpectedly");

        scope_graph
            .new_decl(s_rec, Def, Data { name: "x", data: 1 })
            .expect("edge closed unexpectedly");
        scope_graph
            .new_decl(s_let, Def, Data { name: "x", data: 2 })
            .expect("edge closed unexpectedly");

        resolve_complex_unchecked(&mut scope_graph, s_let);

        // todo!("assert the correct edges are closed!")
    }

    #[test]
    fn test_label_order_complex_explicit_close() {
        let mut scope_graph: ScopeGraph<Lbl, TData<usize>, ExplicitClose<Lbl>> =
            ScopeGraph::new(ExplicitClose::default());

        let s0 = scope_graph.new_scope_with::<[Lbl; 0]>(NoData, []);
        let s_with = scope_graph.new_scope_with(NoData, [Lex, Imp]);
        let s_rec = scope_graph.new_scope_with(NoData, [Def]);
        let s_let = scope_graph.new_scope_with(NoData, [Lex, Def]);

        scope_graph
            .new_edge(s_with, Lex, s0)
            .expect("edge closed unexpectedly");
        scope_graph
            .new_edge(s_with, Imp, s_rec)
            .expect("edge closed unexpectedly");
        scope_graph
            .new_edge(s_let, Lex, s_with)
            .expect("edge closed unexpectedly");

        scope_graph
            .new_decl(s_rec, Def, Data { name: "x", data: 1 })
            .expect("edge closed unexpectedly");
        scope_graph
            .new_decl(s_let, Def, Data { name: "x", data: 2 })
            .expect("edge closed unexpectedly");

        scope_graph.close(s_with, &Lex);
        scope_graph.close(s_with, &Imp);
        scope_graph.close(s_rec, &Def);
        scope_graph.close(s_let, &Lex);
        scope_graph.close(s_let, &Def);

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
        )
        .unwrap();

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(1, env_vec.len());

        let path: &ResolvedPath<Lbl, TData<usize>> = &env_vec[0];
        assert!(matches!(path.data(), &Data { name: "x", data: 2 }));

        // todo!("assert the correct edges are closed!")
    }

    fn resolve_complex_unchecked<CMPL>(
        scope_graph: &mut ScopeGraph<Lbl, TData<usize>, CMPL>,
        s_let: Scope,
    ) where
        CMPL: for<'a> Completeness<Lbl, TData<'a, usize>, GetEdgesResult = Vec<Scope>>,
    {
        compile_regex!(type Machine<Lbl> = Lex* Imp? Def);

        let env = resolve(
            scope_graph,
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
        assert!(matches!(path.data(), &Data { name: "x", data: 2 }));
    }
}
