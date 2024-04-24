use crate::completeness::private::Sealed;
use crate::completeness::{
    Completeness, CriticalEdgeBasedCompleteness, CriticalEdgeSet, Delay, EdgeClosedError,
    EdgesOrDelay, FutureCompleteness,
};
use crate::scopegraph::{InnerScopeGraph, Scope, ScopeGraph};
use crate::Label;
use std::{collections::HashSet, hash::Hash};

/// Critical-edge based [`Completeness`] implementation.
///
/// Unlike [`ImplicitClose`](super::ImplicitClose), this implementation shifts responsibility of closing edges to the
/// _type checker writer_. I.e., they have to insert `sg.close(scope, label)` statements at the
/// appropriate positions in the code.
///
/// Returns [`EdgeClosedError`] when an edge is added to a scope in which the label is already
/// closed (by an explicit close of the type checker writer).
///
/// Returns [`Delay`] when edges are retrieved (e.g. during query resolution) for an edge that is
/// not yet closed.
#[derive(Debug)]
pub struct ExplicitClose<LABEL> {
    critical_edges: CriticalEdgeSet<LABEL>,
}

impl<LABEL> Default for ExplicitClose<LABEL> {
    fn default() -> Self {
        ExplicitClose {
            critical_edges: CriticalEdgeSet::default(),
        }
    }
}

impl<LABEL> Sealed for ExplicitClose<LABEL> {}

impl<LABEL: Hash + Eq + Label, DATA> Completeness<LABEL, DATA> for ExplicitClose<LABEL> {
    fn cmpl_new_scope(&self, _: &InnerScopeGraph<LABEL, DATA>, _: Scope) {
        <ExplicitClose<LABEL> as CriticalEdgeBasedCompleteness<LABEL, DATA>>::init_scope_with(
            self,
            LABEL::iter().collect(), // init with all labels: programmer is responsible for closing edges
        )
    }

    fn cmpl_new_complete_scope(&self, _: &InnerScopeGraph<LABEL, DATA>, _: Scope) {
        <ExplicitClose<LABEL> as CriticalEdgeBasedCompleteness<LABEL, DATA>>::init_scope_with(
            self,
            HashSet::new(), // init with empty label set to prevent extension
        )
    }

    type NewEdgeResult = Result<(), EdgeClosedError<LABEL>>;

    fn cmpl_new_edge(
        &self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult {
        if self.critical_edges.is_open(src, &lbl) {
            inner_scope_graph.add_edge(src, lbl, dst);
            Ok(())
        } else {
            Err(EdgeClosedError {
                scope: src,
                label: lbl,
            })
        }
    }

    type GetEdgesResult<'rslv> = EdgesOrDelay<Vec<Scope>, LABEL>
        where
            Self: 'rslv, LABEL: 'rslv, DATA: 'rslv;

    fn cmpl_get_edges<'rslv>(
        &self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
    ) -> Self::GetEdgesResult<'rslv>
    where
        LABEL: 'rslv,
        DATA: 'rslv,
    {
        if self.critical_edges.is_open(src, &lbl) {
            Err(Delay {
                scope: src,
                label: lbl,
            })
        } else {
            Ok(inner_scope_graph.get_edges(src, lbl))
        }
    }
}

impl<LABEL: Hash + Eq + Label, DATA> CriticalEdgeBasedCompleteness<LABEL, DATA>
    for ExplicitClose<LABEL>
{
    fn init_scope_with(&self, open_labels: HashSet<LABEL>) {
        self.critical_edges.init_scope(open_labels)
    }
}

impl<LABEL: Hash + Eq> ExplicitClose<LABEL> {
    /// Close a scope for a certain label
    /// // TODO: link to "closing" in concepts
    pub fn close(&self, scope: Scope, label: &LABEL) {
        self.critical_edges.close(scope, label);
    }
}

impl<'sg, LABEL: Hash + Eq, DATA> ScopeGraph<'sg, LABEL, DATA, ExplicitClose<LABEL>> {
    // TODO: fix this sentence
    /// Closes an edge, (i.e., prohibit future new
    ///
    /// For example, the following program will return an error.
    /// ```
    /// # use scopegraphs::completeness::ExplicitClose;
    /// # use scopegraphs::Label;
    /// # use scopegraphs::Storage;
    /// # use scopegraphs::ScopeGraph;
    ///
    /// # #[derive(Eq, Hash, PartialEq, Label)] enum Lbl { Def }
    /// # use Lbl::*;
    /// let storage = Storage::new();
    /// let mut sg = ScopeGraph::<Lbl, usize, _>::new(&storage, ExplicitClose::default());
    ///
    /// let s1 = sg.add_scope_with(0, [Def]);
    /// let s2 = sg.add_scope_closed(42);
    ///
    /// sg.close(s1, &Def);
    /// sg.add_edge(s1, Def, s2).expect_err("cannot add edge after closing edge");
    /// ```
    ///
    /// Closing is required to permit queries to traverse these edges:
    /// ```
    ///
    /// # use scopegraphs::completeness::ExplicitClose;
    /// # use scopegraphs::ScopeGraph;
    /// # use scopegraphs::resolve::{DefaultDataEquivalence, DefaultLabelOrder, EdgeOrData, Resolve};
    /// # use scopegraphs_macros::{compile_regex, Label};
    /// # use scopegraphs::Storage;
    /// #
    /// # #[derive(Eq, Hash, PartialEq, Label, Debug, Copy, Clone)]
    /// # enum Lbl { Def }
    /// # use Lbl::*;
    /// # type LblD = EdgeOrData<Lbl>;
    /// #
    /// # compile_regex!(type Regex<Lbl> = Def);
    /// let storage = Storage::new();
    /// let mut sg = ScopeGraph::<Lbl, usize, _>::new(&storage, ExplicitClose::default());
    ///
    /// let s1 = sg.add_scope_with(0, [Def]);
    /// let s2 = sg.add_scope_closed(42);
    ///
    /// // Note: not calling `sg.close(s1, &Def)`
    ///
    /// let query_result = sg.query()
    ///     .with_path_wellformedness(Regex::new()) // regex: `Def`
    ///     .with_data_wellformedness(|x: &usize| *x == 42) // match `42`
    ///     .resolve(s1);
    ///
    /// query_result.expect_err("require s1/Def to be closed");
    /// ```
    ///
    /// Closing allows queries to resolve:
    /// ```
    ///
    /// # use scopegraphs::completeness::ExplicitClose;
    /// # use scopegraphs::ScopeGraph;
    /// # use scopegraphs::resolve::{DefaultDataEquivalence, DefaultLabelOrder, EdgeOrData, Resolve};
    /// # use scopegraphs_macros::{compile_regex, Label};
    /// # use scopegraphs::Storage;
    /// #
    /// # #[derive(Eq, Hash, PartialEq, Label, Debug, Copy, Clone)]
    /// # enum Lbl { Def }
    /// # use Lbl::*;
    /// # type LblD = EdgeOrData<Lbl>;
    /// #
    /// # compile_regex!(type Regex<Lbl> = Def);
    /// let storage = Storage::new();
    /// let mut sg = ScopeGraph::<Lbl, usize, _>::new(&storage, ExplicitClose::default());
    ///
    /// let s1 = sg.add_scope_with(0, [Def]);
    /// let s2 = sg.add_scope_closed(42);
    ///
    /// // Note: closing the edge *after* creating all edges, *before* doing the query
    /// sg.close(s1, &Def);
    ///
    /// let query_result = sg.query()
    ///     .with_path_wellformedness(Regex::new()) // regex: `Def`
    ///     .with_data_wellformedness(|x: &usize| *x == 42) // match `42`
    ///     .resolve(s1);
    ///
    /// query_result.expect("query should return result");
    /// ```
    pub fn close(&self, scope: Scope, label: &LABEL) {
        self.completeness.close(scope, label)
    }
}

impl<'sg, LABEL: Hash + Eq + Copy, DATA> ScopeGraph<'sg, LABEL, DATA, FutureCompleteness<LABEL>> {
    // TODO: update this example to use futures
    // TODO: fix this sentence
    /// Closes an edge, (i.e., prohibit future new
    ///
    /// For example, the following program will return an error.
    /// ```
    /// # use scopegraphs::completeness::ExplicitClose;
    /// # use scopegraphs::ScopeGraph;
    /// # use scopegraphs_macros::Label;
    /// # use scopegraphs::Storage;
    /// # #[derive(Eq, Hash, PartialEq, Label)] enum Lbl { Def }
    /// # use Lbl::*;
    /// let storage = Storage::new();
    /// let mut sg = ScopeGraph::<Lbl, usize, _>::new(&storage, ExplicitClose::default());
    ///
    /// let s1 = sg.add_scope_with(0, [Def]);
    /// let s2 = sg.add_scope_closed(42);
    ///
    /// sg.close(s1, &Def);
    /// sg.add_edge(s1, Def, s2).expect_err("cannot add edge after closing edge");
    /// ```
    ///
    /// Closing is required to permit queries to traverse these edges:
    /// ```
    ///
    /// # use scopegraphs::completeness::ExplicitClose;
    /// # use scopegraphs::ScopeGraph;
    /// # use scopegraphs::resolve::{DefaultDataEquivalence, DefaultLabelOrder, EdgeOrData, Resolve};
    /// # use scopegraphs_macros::{compile_regex, Label};
    /// # use scopegraphs::Storage;
    /// #
    /// # #[derive(Eq, Hash, PartialEq, Label, Debug, Copy, Clone)]
    /// # enum Lbl { Def }
    /// # use Lbl::*;
    /// # type LblD = EdgeOrData<Lbl>;
    /// #
    /// # compile_regex!(type Regex<Lbl> = Def);
    /// let storage = Storage::new();
    /// let mut sg = ScopeGraph::<Lbl, usize, _>::new(&storage, ExplicitClose::default());
    ///
    /// let s1 = sg.add_scope_with(0, [Def]);
    /// let s2 = sg.add_scope_closed(42);
    ///
    /// // Note: not calling `sg.close(s1, &Def)`
    ///
    /// let query_result = sg.query()
    ///     .with_path_wellformedness(Regex::new()) // regex: `Def`
    ///     .with_data_wellformedness(|x: &usize| *x == 42) // match `42`
    ///     .resolve(s1);
    ///
    /// query_result.expect_err("require s1/Def to be closed");
    /// ```
    ///
    /// Closing allows queries to resolve:
    /// ```
    ///
    /// # use scopegraphs::completeness::ExplicitClose;
    /// # use scopegraphs::ScopeGraph;
    /// # use scopegraphs::resolve::{DefaultDataEquivalence, DefaultLabelOrder, EdgeOrData, Resolve};
    /// # use scopegraphs_macros::{compile_regex, Label};
    /// # use scopegraphs::Storage;
    /// #
    /// # #[derive(Eq, Hash, PartialEq, Label, Debug, Copy, Clone)]
    /// # enum Lbl { Def }
    /// # use Lbl::*;
    /// # type LblD = EdgeOrData<Lbl>;
    /// #
    /// # compile_regex!(type Regex<Lbl> = Def);
    /// let storage = Storage::new();
    /// let mut sg = ScopeGraph::<Lbl, usize, _>::new(&storage, ExplicitClose::default());
    ///
    /// let s1 = sg.add_scope_with(0, [Def]);
    /// let s2 = sg.add_scope_closed(42);
    ///
    /// // Note: closing the edge *after* creating all edges, *before* doing the query
    /// sg.close(s1, &Def);
    ///
    /// let query_result = sg.query()
    ///     .with_path_wellformedness(Regex::new()) // regex: `Def`
    ///     .with_data_wellformedness(|x: &usize| *x == 42) // match `42`
    ///     .resolve(s1);
    ///
    /// query_result.expect("query should return result");
    /// ```
    pub fn close(&self, scope: Scope, label: &LABEL) {
        self.completeness.close(scope, label)
    }
}