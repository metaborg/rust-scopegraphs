//! This module contains the lookup-based name resolution algorithm.
//!
//! Starting from a reference in a scope, it will traverse the scope graph to find matching declarations.
//! The versatile set of parameters guides the search to ensure the resulting environment matches
//! the intended semantics of the reference.

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::iter;
use std::rc::Rc;
use std::sync::Arc;

use crate::completeness::Completeness;
use crate::containers::{EnvContainer, PathContainer, PathContainerWf, ScopeContainer, ScopeContainerWf};
use crate::resolve::{
    DataEquivalence, DataWellformedness, EdgeOrData, Env, LabelOrder, Path, Query, Resolve,
    ResolvedPath,
};
use crate::{Label, Scope, ScopeGraph};
use scopegraphs_regular_expressions::RegexMatcher;

impl<'sg: 'rslv, 'storage, 'rslv, LABEL, DATA, CMPL, PWF, DWF, LO, DEq> Resolve<'sg, 'rslv>
    for Query<'storage, 'sg, 'rslv, LABEL, DATA, CMPL, PWF, DWF, LO, DEq>
where
    'storage: 'sg,
    LABEL: Label + Copy + Debug + Hash,
    DATA: Debug,
    CMPL: Completeness<LABEL, DATA>,
    CMPL::GetEdgesResult<'rslv> : ScopeContainerWf<'sg, 'rslv, LABEL, DATA, DWF::Output>,
    PWF: for<'a> RegexMatcher<&'a LABEL> + 'rslv,
    DWF: DataWellformedness<'sg, DATA> + 'rslv,

    // DWF : DataWellFormedNess<DATA, Completeness::DWF>
    LO: LabelOrder<LABEL> + 'rslv,
    DEq: DataEquivalence<DATA> + 'rslv,
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
    Path<LABEL>: Clone,
{
    type EnvContainer
        = EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output>
    where
        'sg: 'rslv,
        Self: 'rslv;

    /// Entry point of lookup-based query resolution. Performs a traversal of the scope graph that
    /// results in an environment containing all declarations matching a reference.
    ///
    /// Type parameters:
    /// - `LABEL`: labels in the scope graph.
    /// - `DATA`: Data in the scope graph.
    /// - `CMPL`: the completeness approach (determined by the scope graph). Should be an instance of
    ///     [`Completeness`]. This guarantees that the query resolution result will remain valid, even
    ///     in the presence of future additions to the scope graph.
    /// - `ENVC`: The [`EnvContainer`] (determined by `CMPL`) used to process environments throughout
    ///     the resolution process..
    /// - `PWF`: regular expression matcher that indicates which paths are valid.
    ///     The labels of all paths in the environment will match the regular expression that this
    ///     matcher is derived from.
    /// - `DWF`: Unary predicate over data that selects valid declarations.
    /// - `LO`: Label order: the order on labels that indicates which declarations are preferred over others.
    /// - `DEq`: Equivalence relation on data that determines which declarations can shadow each other.
    ///
    /// Parameters:
    /// - `scope`: the scope graph in which to start name resolution
    fn resolve(&'rslv self, scope: Scope) -> Self::EnvContainer {
        let all_edges: Vec<EdgeOrData<LABEL>> = LABEL::iter()
            .map(EdgeOrData::Edge)
            .chain(iter::once(EdgeOrData::Data))
            .collect();

        let context = Arc::new(ResolutionContext {
            all_edges,
            sg: self.scope_graph,
            data_wellformedness: &self.data_wellformedness,
            label_order: &self.label_order,
            data_equiv: &self.data_equivalence,
        });

        // AZ: two observations made in the mean time
        // - context is captured in closures, so there is no distinction between 'ctx and 'rslv
        //   solution: capture context in an Rc?
        // - making 'env: 'sg makes the code type-check
        //   solution: do away with 'sg entirely?
        context.resolve_all(self.path_wellformedness.clone(), Path::new(scope))
    }
}

struct ResolutionContext<'storage, 'sg: 'rslv, 'rslv, LABEL: Label, DATA, CMPL, DWF, LO, DEq> {
    all_edges: Vec<EdgeOrData<LABEL>>,
    sg: &'sg ScopeGraph<'storage, LABEL, DATA, CMPL>,
    data_wellformedness: &'rslv DWF,
    label_order: &'rslv LO,
    data_equiv: &'rslv DEq,
}

type EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWFO> = <<<CMPL as Completeness<LABEL, DATA>>::GetEdgesResult<
    'rslv,
> as ScopeContainerWf<'sg, 'rslv, LABEL, DATA, DWFO>>::PathContainerWf as PathContainerWf<'sg, 'rslv, LABEL, DATA, DWFO>>::EnvContainerWf;

type EnvCache<LABEL, ENVC> = RefCell<HashMap<EdgeOrData<LABEL>, Rc<ENVC>>>;

impl<'sg: 'rslv, 'rslv, LABEL, DATA, CMPL, DWF, LO, DEq>
    ResolutionContext<'_, 'sg, 'rslv, LABEL, DATA, CMPL, DWF, LO, DEq>
where
    LABEL: Label + Debug + Hash,
    DATA: Debug,
    ResolvedPath<'sg, LABEL, DATA>: Hash + Eq,
    CMPL: Completeness<LABEL, DATA>,
    CMPL::GetEdgesResult<'rslv> : ScopeContainerWf<'sg, 'rslv, LABEL, DATA, DWF::Output>,
    DEq: DataEquivalence<DATA>,
    DWF: DataWellformedness<'sg, DATA>,
    LO: LabelOrder<LABEL>,
    Path<LABEL>: Clone,
{
    /// Finds all paths of which:
    /// - `path` is a prefix
    /// - the extension matches `path_wellformedness`
    /// - the other conditions in `self` are obeyed.
    fn resolve_all(
        self: &Arc<Self>,
        path_wellformedness: impl for<'a> RegexMatcher<&'a LABEL> + 'rslv,
        path: Path<LABEL>,
    ) -> EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output> {
        let edges: Vec<_> = self
            .all_edges
            .iter()
            .copied()
            .filter(|e| match *e {
                // match current scope only if the regular expression is accepting
                EdgeOrData::Data => path_wellformedness.is_accepting(),
                // traverse `label` if the `path_wellformedness` accepts words that start with it.
                EdgeOrData::Edge(label) => path_wellformedness.accepts_prefix([&label]),
            })
            .collect();

        log::info!("Resolving edges {:?} in {:?}", edges, path.target());
        let cache = HashMap::new();
        self.resolve_edges(path_wellformedness, &edges, path, &RefCell::new(cache))
    }

    /// Computes a sub environment for `edges`, for which shadowing is internally applied.
    fn resolve_edges<'env>(
        self: &Arc<Self>,
        path_wellformedness: impl for<'a> RegexMatcher<&'a LABEL> + 'rslv,
        edges: &[EdgeOrData<LABEL>],
        path: Path<LABEL>,
        cache: &EnvCache<LABEL, EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output>>,
    ) -> EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output> {
        // set of labels that are to be resolved last
        let max = self.max(edges);
        let tgt = path.target();
        log::info!("Resolving max-edges {:?} in {:?}", max, tgt);

        max.into_iter()
            .map::<Rc<EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output>>, _>(|edge| {
                if let Some(env) = cache.borrow().get(&edge) {
                    log::info!("Reuse {:?}-environment from cache", edge);
                    return env.clone();
                }
                // sub-environment that has higher priority that the `max`-environment
                let smaller = self.smaller(edge, edges);
                log::info!(
                    "Resolving(shadowed) {:?} < {:?} in {:?}",
                    smaller,
                    edge,
                    tgt
                );
                let new_env = self.resolve_shadow(
                    path_wellformedness.clone(),
                    edge,
                    &smaller,
                    path.clone(),
                    cache,
                );
                cache.borrow_mut().insert(edge, new_env.clone());
                new_env
            })
            .fold(Self::empty_env_container(), |env1, env2| {
                // env1 and env2 are Rc<ENVC> clones not owned by a cache
                env1.flat_map(move |agg_env| {
                    let mut merged_env = agg_env.clone();
                    env2.flat_map(move |new_env| {
                        merged_env.merge(new_env);
                        <EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output> as From<Env<'sg, LABEL, DATA>>>::from(
                            merged_env,
                        )
                    })
                })
            })
    }

    /// Computes shadowed environment with following steps:
    /// - Compute the environment for `edges` (the _base environment_)
    /// - Compute the environment for `edge` (the _sub-environment_).
    /// - Insert the paths in the sub-environment in the base environment
    ///   iff it is not shadowed by some path in the base environment.
    fn resolve_shadow(
        self: &Arc<Self>,
        path_wellformedness: impl for<'a> RegexMatcher<&'a LABEL> + 'rslv,
        edge: EdgeOrData<LABEL>,     // current max label
        edges: &[EdgeOrData<LABEL>], // smaller set of `edge`
        path: Path<LABEL>,
        cache: &EnvCache<LABEL, EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output>>,
    ) -> Rc<EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output>> {
        // base environment
        let base_env: EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output> =
            self.resolve_edges(path_wellformedness.clone(), edges, path.clone(), cache);
        let local_self = Arc::clone(self);
        // environment of current (max) label, which might be shadowed by the base environment
        Rc::new(base_env.flat_map(move |base_env| {
            if !base_env.is_empty() && local_self.data_equiv.always_equivalent() {
                <EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output> as From<Env<'sg, LABEL, DATA>>>::from(
                    base_env.clone(),
                )
            } else {
                let sub_env = local_self.resolve_edge(path_wellformedness.clone(), edge, path);
                let local_self = Arc::clone(&local_self);
                let base_env = base_env.clone();

                sub_env.flat_map(move |sub_env| {
                    let filtered_env = sub_env
                        .iter()
                        .filter(|p1| {
                            if let Some(p2) = base_env
                                .iter()
                                .find(|p2| local_self.data_equiv.data_equiv(p1.data, p2.data))
                            {
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

                    let mut new_env = base_env;
                    for path in filtered_env {
                        new_env.insert(path.clone())
                    }
                    <EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output> as From<Env<'sg, LABEL, DATA>>>::from(
                        new_env,
                    )
                })
            }
        }))
    }

    /// Compute environment for single edge
    fn resolve_edge<'env>(
        self: &Arc<Self>,
        path_wellformedness: impl for<'a> RegexMatcher<&'a LABEL> + 'rslv,
        edge: EdgeOrData<LABEL>,
        path: Path<LABEL>,
    ) -> EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output> {
        match edge {
            EdgeOrData::Edge(label) => {
                let mut new_path_wellformedness = path_wellformedness.clone();
                new_path_wellformedness.step(&label);
                self.resolve_label(new_path_wellformedness, label, path.clone())
            }
            EdgeOrData::Data => self.resolve_data(path.clone()),
        }
    }

    /// Compute environment for single label.
    ///
    /// Traverses all outgoing edges from `path.target()` with label `label`,
    /// and recursively queries from there.
    fn resolve_label<'env>(
        self: &Arc<Self>,
        path_wellformedness: impl for<'a> RegexMatcher<&'a LABEL> + 'rslv,
        label: LABEL,
        path: Path<LABEL>,
    ) -> EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output> {
        let source = path.target();
        let targets = self.sg.get_edges(source, label);
        let path_set = targets.lift_step(label, path.clone());
        let local_ctx = Arc::clone(self);
        let env: EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output> = path_set.map_into_env::<_>(move |p| {
            local_ctx.resolve_all(path_wellformedness.clone(), p.clone())
        });
        env
    }

    /// Creates single-path environment if the data `path.target()` is matching, or an empty environment otherwise.
    fn resolve_data(&self, path: Path<LABEL>) -> EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output> {
        let data = self.sg.get_data(path.target());
        let path = path.clone().resolve(data);
        let data_ok = self.data_wellformedness.data_wf(data);

        <EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output> as EnvContainer<
            'sg,
            'rslv,
            LABEL,
            DATA,
            <DWF as DataWellformedness<DATA>>::Output,
        >>::inject_if(data_ok, path)
    }

    /// Computes the edges in `edges` for which no 'greater' edge exists.
    fn max(&self, edges: &[EdgeOrData<LABEL>]) -> Vec<EdgeOrData<LABEL>> {
        let max = edges
            .iter()
            .filter(|l| !edges.iter().any(|ll| self.label_order.less_than(l, ll)))
            .copied()
            .collect();

        log::info!("max({:?}) = {:?}", edges, max);
        max
    }

    /// Returns all edges in `edges` that are smaller than `edge`.
    fn smaller(
        &self,
        edge: EdgeOrData<LABEL>,
        edges: &[EdgeOrData<LABEL>],
    ) -> Vec<EdgeOrData<LABEL>> {
        let smaller = edges
            .iter()
            .filter(|l| self.label_order.less_than(l, &edge))
            .copied()
            .collect();

        log::info!("smaller({:?}, {:?}) = {:?}", edge, edges, smaller);
        smaller
    }

    fn empty_env_container() -> EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output> {
        <EnvC<'sg, 'rslv, CMPL, LABEL, DATA, DWF::Output> as EnvContainer<
            'sg,
            'rslv,
            LABEL,
            DATA,
            <DWF as DataWellformedness<DATA>>::Output,
        >>::empty()
    }
}

#[cfg(test)]
mod tests {

    use scopegraphs_macros::label_order;

    use crate::{
        add_scope,
        completeness::{
            Delay, ExplicitClose, FutureCompleteness, ImplicitClose, UncheckedCompleteness,
        },
        future_wrapper::FutureWrapper,
        query_regex,
        resolve::{DataWellformedness, Resolve, ResolvedPath},
        storage::Storage,
        Label, ScopeGraph,
    };

    #[derive(Label, Hash, PartialEq, Eq, Debug, Clone, Copy)]
    enum Lbl {
        Lex,
        Imp,
        Def,
    }
    use Lbl::*;

    pub mod scopegraphs {
        pub use crate::*;
    }

    #[derive(Hash, PartialEq, Eq, Debug, Default)]
    enum TData<'a> {
        #[default]
        NoData,
        Data {
            name: &'a str,
            data: usize,
        },
    }

    use TData::*;

    impl<'a> TData<'a> {
        fn matches(&self, n: &str) -> bool {
            match self {
                NoData => false,
                Data { name, .. } => *name == n,
            }
        }

        fn matcher(n: &'a str) -> impl (for<'b> Fn(&'b Self) -> bool) {
            |data: &Self| data.matches(n)
        }

        fn matcher_res(n: &'a str) -> impl (for<'b> Fn(&'b Self) -> Result<bool, Delay<Lbl>>) {
            |data: &Self| Ok(data.matches(n))
        }

        fn matcher_fut(n: &'a str) -> impl (for<'b> Fn(&'b Self) -> FutureWrapper<'b, bool>) {
            |data: &Self| {
                let matches = data.matches(n);
                FutureWrapper::new(async move { matches })
            }
        }

        fn from_default(name: &'a str) -> Self {
            Data { name, data: 0 }
        }
    }

    #[ctor::ctor]
    fn init() {
        env_logger::init();
    }

    #[test]
    fn test_label_order_async() {
        futures::executor::block_on(async {
            let storage = Storage::new();
            let completeness = FutureCompleteness::default();
            let scope_graph: ScopeGraph<Lbl, TData, FutureCompleteness<Lbl>> =
                ScopeGraph::new(&storage, completeness);

            let s0 = scope_graph.add_scope_default_closed();
            let (s_with, with_lex, with_imp) = add_scope!(&scope_graph, [Lex, Imp]);
            let (s_rec, rec_def) = add_scope!(&scope_graph, [Def]);
            let (s_let, let_lex, let_def) = add_scope!(&scope_graph, [Lex, Def]);

            scope_graph
                .ext_edge(&with_lex, s0)
                .expect("edge closed unexpectedly");
            scope_graph
                .ext_edge(&with_imp, s_rec)
                .expect("edge closed unexpectedly");
            scope_graph
                .ext_edge(&let_lex, s_with)
                .expect("edge closed unexpectedly");

            scope_graph
                .ext_decl(&rec_def, Data { name: "x", data: 1 })
                .expect("edge closed unexpectedly");
            scope_graph
                .ext_decl(&let_def, Data { name: "x", data: 2 })
                .expect("edge closed unexpectedly");

            // close edges
            with_lex.close();
            with_imp.close();
            rec_def.close();
            let_lex.close();
            let_def.close();

            let query = scope_graph
                .query()
                .with_path_wellformedness(query_regex!(Lbl: Lex* Imp? Def))
                .with_data_wellformedness(TData::matcher_fut("x"))
                .with_label_order(label_order!(Lbl: Def < Imp < Lex));
            type DWF = impl for<'sg> DataWellformedness<'sg, TData<'sg>, Output = FutureWrapper<'sg, bool>>;
            let env = crate::resolve::Query::<
                '_,
                '_,
                '_,
                _,
                _,
                _,
                _,
                DWF,
                _,
                _
            >::resolve(&query, s_let).await;

            let env_vec = env.into_iter().collect::<Vec<_>>();
            assert_eq!(1, env_vec.len());

            let path: &ResolvedPath<Lbl, TData> = &env_vec[0];
            assert!(matches!(path.data(), &Data { name: "x", data: 2 }));

            // todo!("assert the correct edges are closed!")
        });
    }

    #[test]
    fn test_match_data() {
        let storage = Storage::new();
        let scope_graph: ScopeGraph<Lbl, TData, UncheckedCompleteness> =
            unsafe { ScopeGraph::raw(&storage) };

        let s0 = scope_graph.add_scope_default();
        scope_graph.add_decl(s0, Def, TData::from_default("x"));

        let env = scope_graph.query();
        let query = env
            .with_path_wellformedness(query_regex!(Lbl: Def))
            .with_data_wellformedness(TData::matcher("x"));
        let env = query.resolve(s0);

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(1, env_vec.len());

        let path: &ResolvedPath<Lbl, TData> = &env_vec[0];
        assert!(path.data().matches("x"))
    }

    #[test]
    fn test_no_match_other_data() {
        let storage = Storage::new();
        let scope_graph: ScopeGraph<Lbl, TData, UncheckedCompleteness> =
            unsafe { ScopeGraph::raw(&storage) };

        let s0 = scope_graph.add_scope_default();
        scope_graph.add_decl(s0, Def, TData::from_default("x"));

        let env = scope_graph
            .query()
            .with_path_wellformedness(query_regex!(Lbl: Def))
            .with_data_wellformedness(TData::matcher("y"))
            .resolve(s0);

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(0, env_vec.len());
    }

    #[test]
    fn test_regex_enforces_step() {
        let storage = Storage::new();
        let scope_graph: ScopeGraph<Lbl, TData, UncheckedCompleteness> =
            unsafe { ScopeGraph::raw(&storage) };

        let s0 = scope_graph.add_scope_default();
        let s1 = scope_graph.add_scope_default();
        scope_graph.add_edge(s0, Lex, s1);
        scope_graph.add_decl(s0, Def, Data { name: "x", data: 0 });
        scope_graph.add_decl(s1, Def, Data { name: "x", data: 1 });

        let env = scope_graph
            .query()
            .with_path_wellformedness(query_regex!(Lbl: Lex Def))
            .with_data_wellformedness(TData::matcher("x"))
            .resolve(s0);

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(1, env_vec.len());

        let path: &ResolvedPath<Lbl, TData> = &env_vec[0];
        assert!(matches!(path.data(), &Data { name: "x", data: 1 }))
    }

    #[test]
    fn test_regex_filter() {
        let storage = Storage::new();
        let scope_graph: ScopeGraph<Lbl, TData, UncheckedCompleteness> =
            unsafe { ScopeGraph::raw(&storage) };

        let s0 = scope_graph.add_scope_default();
        let s1 = scope_graph.add_scope_default();
        scope_graph.add_edge(s0, Lex, s1);
        scope_graph.add_decl(s0, Def, Data { name: "x", data: 0 });

        let env = scope_graph
            .query()
            .with_path_wellformedness(query_regex!(Lbl: Lex Def))
            .with_data_wellformedness(TData::matcher("x"))
            .resolve(s0);

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(0, env_vec.len());
    }

    #[test]
    fn test_shadow_applied() {
        let storage = Storage::new();
        let scope_graph: ScopeGraph<Lbl, TData, UncheckedCompleteness> =
            unsafe { ScopeGraph::raw(&storage) };

        let s0 = scope_graph.add_scope_default();
        let s1 = scope_graph.add_scope_default();
        let s2 = scope_graph.add_scope_default();
        scope_graph.add_edge(s0, Imp, s1);
        scope_graph.add_edge(s0, Lex, s2);
        scope_graph.add_decl(s1, Def, Data { name: "x", data: 0 });
        scope_graph.add_decl(s2, Def, Data { name: "x", data: 1 });

        let env = scope_graph
            .query()
            .with_path_wellformedness(query_regex!(Lbl: Lex Def))
            .with_data_wellformedness(TData::matcher("x"))
            .with_label_order(label_order!(Lbl: Lex < Imp))
            .resolve(s0);

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(1, env_vec.len());

        let path: &ResolvedPath<Lbl, TData> = &env_vec[0];
        assert!(matches!(path.data(), &Data { name: "x", data: 1 }))
    }

    #[test]
    fn test_label_order_complex_raw() {
        let storage = Storage::new();
        let scope_graph: ScopeGraph<Lbl, TData, UncheckedCompleteness> =
            unsafe { ScopeGraph::raw(&storage) };

        let s0 = scope_graph.add_scope_default();
        let s_with = scope_graph.add_scope_default();
        let s_rec = scope_graph.add_scope_default();
        let s_let = scope_graph.add_scope_default();

        scope_graph.add_edge(s_with, Lex, s0);
        scope_graph.add_edge(s_with, Imp, s_rec);
        scope_graph.add_edge(s_let, Lex, s_with);

        scope_graph.add_decl(s_rec, Def, Data { name: "x", data: 1 });
        scope_graph.add_decl(s_let, Def, Data { name: "x", data: 2 });

        let env = scope_graph
            .query()
            .with_path_wellformedness(query_regex!(Lbl: Lex* Imp? Def))
            .with_data_wellformedness(TData::matcher("x"))
            .with_label_order(label_order!(Lbl: Def < Imp < Lex))
            .resolve(s_let);

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(1, env_vec.len());

        let path: &ResolvedPath<Lbl, TData> = &env_vec[0];
        assert!(matches!(path.data(), &Data { name: "x", data: 2 }));
    }

    #[test]
    fn test_label_order_complex_implicit_close() {
        let storage = Storage::new();
        let scope_graph: ScopeGraph<Lbl, TData, ImplicitClose<Lbl>> =
            ScopeGraph::new(&storage, ImplicitClose::default());

        let s0 = scope_graph.add_scope_default();
        let s_with = scope_graph.add_scope_default();
        let s_rec = scope_graph.add_scope_default();
        let s_let = scope_graph.add_scope_default();

        scope_graph
            .add_edge(s_with, Lex, s0)
            .expect("edge closed unexpectedly");
        scope_graph
            .add_edge(s_with, Imp, s_rec)
            .expect("edge closed unexpectedly");
        scope_graph
            .add_edge(s_let, Lex, s_with)
            .expect("edge closed unexpectedly");

        scope_graph
            .add_decl(s_rec, Def, Data { name: "x", data: 1 })
            .expect("edge closed unexpectedly");
        scope_graph
            .add_decl(s_let, Def, Data { name: "x", data: 2 })
            .expect("edge closed unexpectedly");

        let env = scope_graph
            .query()
            .with_path_wellformedness(query_regex!(Lbl: Lex* Imp? Def))
            .with_data_wellformedness(TData::matcher("x"))
            .with_label_order(label_order!(Lbl: Def < Imp < Lex))
            .resolve(s_let);

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(1, env_vec.len());

        let path: &ResolvedPath<Lbl, TData> = &env_vec[0];
        assert!(matches!(path.data(), &Data { name: "x", data: 2 }));

        // todo!("assert the correct edges are closed!")
    }

    #[test]
    fn test_label_order_complex_explicit_close() {
        let storage = Storage::new();
        let scope_graph: ScopeGraph<Lbl, TData, ExplicitClose<Lbl>> =
            ScopeGraph::new(&storage, ExplicitClose::default());

        let s0 = add_scope!(&scope_graph);
        let (s_with, with_lex, with_imp) = add_scope!(&scope_graph, [Lex, Imp]);
        let (s_rec, rec_def) = add_scope!(&scope_graph, [Def]);
        let (s_let, let_lex, let_def) = add_scope!(&scope_graph, [Lex, Def]);

        scope_graph
            .ext_edge(&with_lex, s0)
            .expect("edge closed unexpectedly");
        scope_graph
            .ext_edge(&with_imp, s_rec)
            .expect("edge closed unexpectedly");
        scope_graph
            .ext_edge(&let_lex, s_with)
            .expect("edge closed unexpectedly");

        scope_graph
            .ext_decl(&rec_def, Data { name: "x", data: 1 })
            .expect("edge closed unexpectedly");
        scope_graph
            .ext_decl(&let_def, Data { name: "x", data: 2 })
            .expect("edge closed unexpectedly");

        with_lex.close();
        with_imp.close();
        rec_def.close();
        let_lex.close();
        let_def.close();

        let env = scope_graph
            .query()
            .with_path_wellformedness(query_regex!(Lbl: Lex* Imp? Def))
            .with_data_wellformedness(TData::matcher_res("x"))
            .with_label_order(label_order!(Lbl: Def < Imp < Lex))
            .resolve(s_let)
            .unwrap();

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(1, env_vec.len());

        let path: &ResolvedPath<Lbl, TData> = &env_vec[0];
        assert!(matches!(path.data(), &Data { name: "x", data: 2 }));

        // todo!("assert the correct edges are closed!")
    }

    #[test]
    fn test_caching() {
        let storage = Storage::new();
        let scope_graph: ScopeGraph<Lbl, TData, ImplicitClose<Lbl>> =
            ScopeGraph::new(&storage, ImplicitClose::default());

        let s0 = scope_graph.add_scope_default_with([Def]);
        scope_graph
            .add_decl(
                s0,
                Def,
                Data {
                    name: "x",
                    data: 42,
                },
            )
            .expect("edge unexpectedly closed");

        let env = scope_graph
            .query()
            .with_path_wellformedness(query_regex!(Lbl: Lex* Imp? Def))
            .with_data_wellformedness(TData::matcher("x"))
            // Because of the label order, `Def` will be in the smaller set of both Lex and Imp
            // Due to the caching, it will be reused rather than recomputed.
            // This can be seen when running this test with RUST_LOG=info.
            .with_label_order(label_order!(Lbl: Def < { Lex, Imp }))
            .resolve(s0);

        let env_vec = env.into_iter().collect::<Vec<_>>();
        assert_eq!(1, env_vec.len());

        let path: &ResolvedPath<Lbl, TData> = &env_vec[0];
        assert!(matches!(
            path.data(),
            &Data {
                name: "x",
                data: 42
            }
        ));
    }
}
