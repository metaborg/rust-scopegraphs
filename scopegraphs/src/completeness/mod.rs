//! This module contains several utilities to guarantee query stability.
//!
//! Query stability means that the result of a query, once resolved, will remain valid after future
//! additions to the scope graph.
//! This allows safe interleaving of name resolution and scope graph construction.
//!
//! The main trait of this module is [`Completeness`]. An implementation of this trait should be passed
//! to [`ScopeGraph::new`](super::ScopeGraph::new) to obtain a scope graph instance.
//!
//! Currently, the module contains three safe implementations.
//! * [`ImplicitClose`] is the easiest to use, and most likely the preferred choice for simple
//!   sequential type checkers.
//! * [`ExplicitClose`] requires some manual bookkeeping, but allows more flexible handling of
//!   queries. This is the most suitable choice for type checkers that need to do dynamic scheduling.
//!   Running queries can return an error, because scopes relevant to the query weren't closed yet.
//! * [`FutureCompleteness`] is like [`ExplicitClose`], except queries can no longer error. Instead,
//!   queries return a [`Future`](std::future::Future) that resolves when all scopes related to the query are closed.

mod future;

pub use future::*;

mod critical_edge;
pub use critical_edge::*;
mod explicit;
pub use explicit::*;
mod implicit;
pub use implicit::*;
mod unchecked;
pub use unchecked::*;

mod private {
    pub trait Sealed {}
}

use crate::scopegraph::{InnerScopeGraph, Scope};
use crate::{Label, ScopeGraph};
use private::Sealed;
use std::rc::Rc;
use std::{fmt::Debug, hash::Hash, marker::PhantomData};

/*** Completeness trait ***/

/// Types implementing this trait have the responsibility to guarantee query stability.
///
/// This means that the result of a query, once computed, may not be invalidated by later extensions
/// of the scope graph. See [`crate::concepts::completeness`]
///
/// A [`super::ScopeGraph`] will call the handlers of its completeness on each operation.
/// For scope addition and data access, it can update some internal state.
/// For edge addition and retrieval, it can override/augment the default result as well.
///
/// The way query stability is guaranteed is very flexible. Current implementations use
/// - critical edges, explicitly closed by the client ([`ExplicitClose`]).
/// - critical edges, implicitly closed when retrieved ([`ImplicitClose`]).
///
/// Future implementations may also:
/// - delay retrieval of edges until it is closed (by returning a future). TODO: [this is implemented now](`FutureCompleteness`)
/// - guarantee stability by retaining residual queries, and checking those do not return any
///   results for later additions.
/// - ...
///
/// This trait is sealed.
/// You cannot define your own completeness strategies to ensure that only verified implementations are available.
// TODO: @Aron could you document this?
#[allow(missing_docs)]
pub trait Completeness<LABEL: Label, DATA>: Sealed {
    fn cmpl_new_scope(&self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope);

    /// Should initialize a scope without possibility to extend it with edges
    fn cmpl_new_complete_scope(
        &self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        scope: Scope,
    );

    type NewEdgeResult;
    fn cmpl_new_edge(
        &self,
        inner_scope_graph: &InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
        dst: Scope,
    ) -> Self::NewEdgeResult;

    // type GetDataResult;
    // fn get_data(&mut self, inner_scope_graph: &InnerScopeGraph<LABEL, DATA>, scope: Scope) -> Self::GetDataResult;

    type GetEdgesResult<'rslv>
    where
        Self: 'rslv,
        LABEL: 'rslv,
        DATA: 'rslv;
    fn cmpl_get_edges<'rslv>(
        &'rslv self,
        inner_scope_graph: &'rslv InnerScopeGraph<LABEL, DATA>,
        src: Scope,
        lbl: LABEL,
    ) -> Self::GetEdgesResult<'rslv>
    where
        LABEL: 'rslv,
        DATA: 'rslv;
}

// There are two main ways of creating edges, which depend on the completeness mode:
// - user-closed: requires the user to close each open edge exactly once; extension based on `ScopeExt`
// - implicit: user does not have explicit control over closing edges
// These are marker traits, that are used in impl block that provide different APIs to different

#[doc(hidden)]
pub struct Witness(pub(crate) ());

/// Marker trait for completeness strategies that require edges to be closed explicitly.
pub trait UserClosed<LABEL: Label, DATA>: Completeness<LABEL, DATA> {
    #[doc(hidden)]
    fn close(&self, scope: Scope, label: &LABEL, witness: Witness);
}

/// Marker trait for completeness strategies that do not require edges to be closed explicitly.
///
/// Used to make the correct API's available on scope graphs.
pub trait Implicit<LABEL: Label, DATA>: Completeness<LABEL, DATA> {}

// Edge extension permissions for UserClosed Completenesses
#[derive(Debug)]
struct ScopeExtPermInner<
    'ext,
    LABEL: Hash + Label + Debug, /* <= Bound here required for Drop implementation */
    DATA,
    CMPL: UserClosed<LABEL, DATA>, // Bound required for Drop implementation
> {
    /// Scope for which this object witnesses the permission to extend.
    scope: Scope,
    /// Label with which [scope] may be extended
    label: LABEL,
    /// Scope graph in which the scope may be extended.
    sg: &'ext CMPL,
    _data: PhantomData<DATA>, // FIXME: required for using `where CMPL: UserClosed<LABEL, DATA>` in impl blocks. Can it be removed some way?
}

impl<'ext, LABEL: Hash + Label + Debug, DATA, CMPL> Drop
    for ScopeExtPermInner<'ext, LABEL, DATA, CMPL>
where
    CMPL: UserClosed<LABEL, DATA>,
{
    /// This is the trick! When the permission is dropped, we know for sure that no future extensions will be made (otherwise, [self] should have been kept alive)
    /// Thus, we can close the critical edge.
    fn drop(&mut self) {
        self.sg.close(self.scope, &self.label, Witness(()))
    }
}

/// Represents the permission to extend a scope with a particular label.
///
/// Do not instantiate this struct manually, but use the [add_scope] macro instead.
#[derive(Debug)]
pub struct ScopeExtPerm<'ext, LABEL: Hash + Label + Debug, DATA, CMPL: UserClosed<LABEL, DATA>>(
    Rc<ScopeExtPermInner<'ext, LABEL, DATA, CMPL>>,
);

impl<'ext, LABEL: Hash + Label + Debug, DATA, CMPL: UserClosed<LABEL, DATA>> Clone
    for ScopeExtPerm<'ext, LABEL, DATA, CMPL>
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<'ext, LABEL: Hash + Label + Debug, DATA, CMPL: UserClosed<LABEL, DATA>>
    ScopeExtPerm<'ext, LABEL, DATA, CMPL>
where
    CMPL: UserClosed<LABEL, DATA>,
{
    pub(super) fn scope(&self) -> &Scope {
        &self.0.scope
    }

    pub(super) fn label(&self) -> &LABEL {
        &self.0.label
    }

    /// This is an implementation detail of the [add_scope!] macro and should not be called directly!
    #[doc(hidden)]
    pub unsafe fn init<'storage>(
        scope: Scope,
        label: LABEL,
        sg: &'ext ScopeGraph<'storage, LABEL, DATA, CMPL>,
    ) -> ScopeExtPerm<'ext, LABEL, DATA, CMPL> {
        ScopeExtPerm(Rc::new(ScopeExtPermInner {
            scope,
            label,
            sg: &sg.completeness,
            _data: PhantomData,
        }))
    }

    /// Closes the edge, meaning that it cannot be extended anymore.
    /// // TODO: fix this sentence
    /// Closes an edge, (i.e., prohibit future new extensions, by losing ownership of the extends permission).
    ///
    /// For example, the following program will return an error.
    /// ```compile_fail
    /// # use scopegraphs::completeness::ExplicitClose;
    /// # use scopegraphs::Label;
    /// # use scopegraphs::Storage;
    /// # use scopegraphs::ScopeGraph;
    /// # use scopegraphs::add_scope;
    ///
    /// # #[derive(Eq, Hash, PartialEq, Label, Clone, Copy)] enum Lbl { Def }
    /// # use Lbl::*;
    /// let storage = Storage::new();
    /// let mut sg = ScopeGraph::<Lbl, usize, _>::new(&storage, ExplicitClose::default());
    ///
    /// let (s1, s1_def) = add_scope!(&sg, 0, [Def]);
    /// let s2 = sg.add_scope_closed(42);
    ///
    /// s1_def.close();
    /// sg.ext_edge(&s1_def, s2).expect_err("cannot add edge after closing edge");
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
    /// # use scopegraphs::add_scope;
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
    /// let (s1, s1_def) = add_scope!(&sg, 0, [Def]);
    /// let s2 = sg.add_scope_closed(42);
    ///
    /// // Note: not calling `s1_def.close()`
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
    /// # use scopegraphs::add_scope;
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
    /// let (s1, s1_def) = add_scope!(&sg, [Def]);
    /// let s2 = sg.add_scope_closed(42);
    ///
    /// // Note: closing the edge *after* creating all edges, *before* doing the query
    /// s1_def.close();
    ///
    /// let query_result = sg.query()
    ///     .with_path_wellformedness(Regex::new()) // regex: `Def`
    ///     .with_data_wellformedness(|x: &usize| *x == 42) // match `42`
    ///     .resolve(s1);
    ///
    /// query_result.expect("query should return result");
    /// ```
    pub fn close(self) {
        // self dropped at the end of this block
    }
}

/// Creates a scope (with some data if specified), and permission to extend it for each label specified in the label list argument.
///
/// TODO: Better documentation, examples.
///
/// ```
/// # use scopegraphs::completeness::ExplicitClose;
/// # use scopegraphs::ScopeGraph;
/// # use scopegraphs::resolve::{DefaultDataEquivalence, DefaultLabelOrder, EdgeOrData, Resolve};
/// # use scopegraphs_macros::{compile_regex, Label};
/// # use scopegraphs::Storage;
/// # use scopegraphs::add_scope;
///
/// # let storage = Storage::new();
/// # let scope_graph: ScopeGraph<(), (), ExplicitClose<()>> = ScopeGraph::new(&storage, ExplicitClose::default());
///
/// add_scope!(sg<let s = ()> with open labels: ());
/// println!("{:?}", s);
/// ```
#[macro_export]
macro_rules! add_scope {
  ($sg:ident[let $s:ident |-> $data:expr] request open: $(- $lbl:expr => let $lbl_id:ident)*) => {
    // put initialized code in block
    let sg = &$sg;       // evaluate scope graph expression
    let data = $data;   // evaluate data expression

    // create new scope
    let $s = sg.add_scope_with(data, [$($lbl),*]);

    // return the scope, and the extension permissions
    $(
        let $lbl_id = unsafe { $crate::completeness::ScopeExtPerm::init($s, $lbl, sg) };
    )*
  };

  ($sg:expr, $data:expr, [ $($lbl:expr),* ]) => {
    {
        // put initialized code in block
        let sg = $sg;       // evaluate scope graph expression
        let data = $data;   // evaluate data expression

        // create new scope
        let scope = sg.add_scope_with(data, [$($lbl),*]);

        // return the scope, and the extension permissions
        (scope, $(unsafe { $crate::completeness::ScopeExtPerm::init(scope, $lbl, sg) }),*)
    }
  };

  // case when no data is given: falls back on [Default] value for data
  ($sg:expr, [$($lbl:expr),* ]) => { add_scope!($sg, Default::default(), [$($lbl),*]) };

  // case when no data nor labels are given
  ($sg:expr) => { add_scope!($sg, Default::default(), []).0 };
}

/// Adds an edge to the scope graph
// TODO: better explanation, (link to) examples.
#[macro_export]
macro_rules! add_edge {
    // case for call with extension permission
    ($sg:expr, $ext:expr, $tgt:expr) => {
        sg.ext_edge($ext, $tgt)
    };

    // case when giving source and target scopes explicitly
    ($sg:expr, $src:expr, $lbl:expr, $tgt:expr) => {
        sg.add_edge($src, $lbl, $tgt)
    };
}

// TODO: Residual-query-based Completeness requires access to query resolution

#[cfg(test)]
mod tests {
    use crate::Label;
    use std::fmt::Debug;

    #[test]
    pub fn test_new_add_macro() {
        #[derive(Label, Debug, PartialEq, Eq, Clone, Copy, Hash)]
        enum Lbl {
            L1,
            L2,
        }

        pub mod scopegraphs {
            pub use crate::*;
        }

        use crate::completeness::ImplicitClose;
        use crate::ScopeGraph;

        use crate::Storage;

        let storage = Storage::new();
        let scope_graph: ScopeGraph<Lbl, (), ImplicitClose<Lbl>> =
            ScopeGraph::new(&storage, ImplicitClose::default());

        add_scope!(
            scope_graph[let s |-> ()] request open:
            - Lbl::L1 => let ext_l1
            - Lbl::L2 => let ext_l2
        );
        println!("{:?}", s);
    }
}
