//! # Concepts of scope graphs
//!
//! To use a scope graph,
//! you need to understand a few basic concepts.
//! First of all, the components that comprise a scope graph:
//!
//! * [A scope](scope)
//! * [A scope's data](scope_data)
//! * [Labelled edges between scopes](edges)
//!
//! Once you know what a scope graph is,
//! we can start talking about running queries over them.
//! For a query to return the desired result, we may need to
//! specify the following properties:
//!
//! * A [Path Well-Formedness](path_wellformedness), dictating what paths are valid for the query to take. Comes in the form of a Regular Expression.
//! * A [Data Well-Formedness](data_wellformedness), dictating which scopes are valid for the query to return. Only if a scope's data satisfies the data well-formedness then a query can return it.
//! * A [Label Ordering](label_ordering), dictating ...
//! * A [Data Equivalence](data_equivalence), dictating ... 
//!
//! To resolve queries over a scope graph, that scope graph should have a notion of [Completeness](completeness)
//!
#![cfg_attr(rustfmt, rustfmt_skip)]

use scopegraphs_render_docs::render_scopegraphs;


#[render_scopegraphs]
/// A scope, is a place in which names can live.
/// Names, of variables, or of functions, or of types, or of modules.
/// Anything really.
///
/// In a programming language, like Rust, a scope might be represented as a set of braces:
/// ```rust
/// {
///     let a = 3;
///     let b = 4;
/// }
/// ```
/// The scope contains two names: `a` and `b`.
///
/// In scope graphs, every scope has some kind of associated [scope data](crate::concepts::scope_data),
/// and any number of [connections](crate::concepts::edges) to other scopes (0 connections is also fine).
///
/// In this library, a scope is represented using the [`Scope`](crate::Scope) type.
///
/// In the next section, we'll talk about how to create a scope. We'll already use the names `Data` and `Lbl`,
/// which you can read more about in the linked sections.
///
/// # Creating a Scope
///
/// To make a scope, you first need a scope graph object, and to build a scope graph, you first need a scope graph storage object:
///
/// ```rust
/// # use scopegraphs::*;
/// # use completeness::{ImplicitClose};
/// # use resolve::{DataWellformedness, Resolve, ResolvedPath};
/// # use render::{RenderSettings, RenderScopeData, RenderScopeLabel};
/// #
/// # #[derive(Label, Hash, PartialEq, Eq, Debug, Clone, Copy)]
/// # enum Lbl {}
/// # #[derive(Hash, PartialEq, Eq, Debug, Clone)]
/// # enum Data {}
/// # impl RenderScopeData for Data {}
/// #
/// # impl RenderScopeLabel for Lbl {
/// #     fn render(&self) -> String {
/// #         match self {
/// #              _ => todo!(),
/// #         }
/// #     }
/// # }
/// #
/// let storage = Storage::new();
///
/// let sg: ScopeGraph<Lbl, Data, ImplicitClose<Lbl>> = ScopeGraph::new(&storage, ImplicitClose::default());
/// ```
///
/// That's a lot of type parameters! And you'll need to type it quite a few times when using scope graphs in your project.
/// I recommend that one of the first thing you do once you start using scope graphs is to make a type alias:
///
/// ```
/// # use scopegraphs::*;
/// # use completeness::{ImplicitClose};
/// # use resolve::{DataWellformedness, Resolve, ResolvedPath};
/// # use render::{RenderSettings, RenderScopeData, RenderScopeLabel};
/// #
/// # #[derive(Label, Hash, PartialEq, Eq, Debug, Clone, Copy)]
/// # enum Lbl {}
/// # #[derive(Hash, PartialEq, Eq, Debug, Clone)]
/// # enum Data {}
/// # impl RenderScopeData for Data {}
/// #
/// # impl RenderScopeLabel for Lbl {
/// #     fn render(&self) -> String {
/// #         match self {
/// #              _ => todo!(),
/// #         }
/// #     }
/// # }
/// #
/// type MyScopeGraph<'s> = ScopeGraph<'s, Lbl, Data, ImplicitClose<Lbl>>;
///
/// let storage = Storage::new();
/// let sg = MyScopeGraph::new(&storage, ImplicitClose::default());
/// ```
/// In these examples, I'm referring to something called [`ImplicitClose`](crate::completeness::ImplicitClose),
/// which is an example of a [completeness strategy](crate::concepts::completeness).
/// For now, you can just ignore that. We haven't gotten to that part yet.
///
/// Now, we can finally create a scope by using [`add_scope_default`](crate::ScopeGraph::add_scope_default).
/// ```rust
/// # use scopegraphs::*;
/// # use completeness::{UncheckedCompleteness};
/// # use resolve::{DataWellformedness, Resolve, ResolvedPath};
/// # use render::{RenderSettings, RenderScopeData, RenderScopeLabel};
/// #
/// # #[derive(Label, Hash, PartialEq, Eq, Debug, Clone, Copy)]
/// # enum Lbl {
/// #     Lex,
/// #     Imp,
/// #     Def,
/// # }
/// # use Lbl::*;
/// #
/// # #[derive(Hash, PartialEq, Eq, Debug, Default, Clone)]
/// # enum TData<'a> {
/// #     #[default]
/// #     NoData,
/// #     Data {
/// #         name: &'a str,
/// #         data: usize,
/// #     },
/// # }
/// #
/// # use TData::*;
/// #
/// # impl RenderScopeData for TData<'_> {
/// #     fn render_node(&self) -> Option<String> {
/// #         match self {
/// #             NoData => None,
/// #             Data {name, data} => Some(format!("{name}: {data}")),
/// #         }
/// #     }
/// # }
/// #
/// # impl RenderScopeLabel for Lbl {
/// #     fn render(&self) -> String {
/// #         match self {
/// #             Lex => "lex",
/// #             Imp => "imp",
/// #             Def => "def",
/// #         }.to_string()
/// #     }
/// # }
/// #
/// # impl<'a> TData<'a> {
/// #     fn matches(&self, n: &str) -> bool {
/// #         match self {
/// #             NoData => false,
/// #             Data { name, .. } => *name == n,
/// #         }
/// #     }
/// #
/// #     fn matcher(n: &'a str) -> impl DataWellformedness<Self> {
/// #         |data: &Self| data.matches(n)
/// #     }
/// #
/// #     fn from_default(name: &'a str) -> Self {
/// #         Data { name, data: 0 }
/// #     }
/// # }
/// # let storage = Storage::new();
/// # let sg: ScopeGraph<Lbl, TData, UncheckedCompleteness> =
/// # unsafe { ScopeGraph::raw(&storage) };
///
/// // make a new scope, and call it s0
/// let s0 = sg.add_scope_default();
/// ```
///
/// # Visualizing a Scope Graph
///
/// Once we're making a lot of scope graphs in this tutorial,
/// or when you're using this library to build something,
/// it's useful to visualize the graph.
/// The following example shows how to do that:
///
/// ```rust
/// # use scopegraphs::*;
/// # use completeness::{UncheckedCompleteness};
/// # use resolve::{DataWellformedness, Resolve, ResolvedPath};
/// # use render::{RenderSettings, RenderScopeData, RenderScopeLabel};
/// #
/// # #[derive(Label, Hash, PartialEq, Eq, Debug, Clone, Copy)]
/// # enum Lbl {
/// #     Lex,
/// #     Imp,
/// #     Def,
/// # }
/// # use Lbl::*;
/// #
/// # #[derive(Hash, PartialEq, Eq, Debug, Default, Clone)]
/// # enum TData<'a> {
/// #     #[default]
/// #     NoData,
/// #     Data {
/// #         name: &'a str,
/// #         data: usize,
/// #     },
/// # }
/// #
/// # use TData::*;
/// #
/// # impl RenderScopeData for TData<'_> {
/// #     fn render_node(&self) -> Option<String> {
/// #         match self {
/// #             NoData => None,
/// #             Data {name, data} => Some(format!("{name}: {data}")),
/// #         }
/// #     }
/// # }
/// #
/// # impl RenderScopeLabel for Lbl {
/// #     fn render(&self) -> String {
/// #         match self {
/// #             Lex => "lex",
/// #             Imp => "imp",
/// #             Def => "def",
/// #         }.to_string()
/// #     }
/// # }
/// #
/// # impl<'a> TData<'a> {
/// #     fn matches(&self, n: &str) -> bool {
/// #         match self {
/// #             NoData => false,
/// #             Data { name, .. } => *name == n,
/// #         }
/// #     }
/// #
/// #     fn matcher(n: &'a str) -> impl DataWellformedness<Self> {
/// #         |data: &Self| data.matches(n)
/// #     }
/// #
/// #     fn from_default(name: &'a str) -> Self {
/// #         Data { name, data: 0 }
/// #     }
/// # }
/// # let storage = Storage::new();
/// # let sg: ScopeGraph<Lbl, TData, UncheckedCompleteness> =
/// # unsafe { ScopeGraph::raw(&storage) };
///
/// // make a new scope, and call it s0
/// let s0 = sg.add_scope_default();
/// let s1 = sg.add_scope_default();
///
/// sg.render_to("output.mmd", RenderSettings::default()).unwrap()
/// ```
/// And as you can see, this renders two scopes (0 and 1), not connected by any [edges](crate::concepts::edges) yet.
///
/// Now that you know how to make a scope, I recommend you continue reading about [scope data](crate::concepts::scope_data)
pub mod scope {}

#[render_scopegraphs]
/// A [scope](crate::concepts::scope) always has associated data.
/// Scopes need data, because that's where the information is stored about what names are defined.
/// Later on, when we start talking about [queries over scope graphs](crate::concepts::query):
/// this data is what the query will be looking for.
///
/// This data is completely customizable, as the scope graph is generic over this.
/// However, often scope data will look somewhat similar to the following:
///
/// ```rust
/// # use scopegraphs::*;
/// # use completeness::{UncheckedCompleteness};
/// # use resolve::{DataWellformedness, Resolve, ResolvedPath};
/// # use render::{RenderSettings, RenderScopeData, RenderScopeLabel};
/// #[derive(Hash, PartialEq, Eq, Debug, Default, Clone)]
/// enum Data<'a> {
///     // some scopes don't have data
///     #[default]
///     NoData,
///
///     // in other scopes, a variable might be defined
///     Variable {
///         name: &'a str,
///     },
/// }
/// ```
/// <div class="warning">
///
/// Note: if you want some scopes to have *no* associated data, you're responsible for that.
/// Either explicitly add a variant `NoData` as we did above, or possibly make the scope graph DATA type an `Option`.
///
/// </div>
///
/// Previously, in [scopes](crate::concepts::scope) we've seen how we can create a scope using [`add_scope_default`](crate::ScopeGraph::add_scope_default).
/// However, that's slightly simplified. That creates a scope with **default data**.
/// In the example data definition above, that would mean the associated data of the new scope is `NoData`.
///
/// For example, let's say we want to create a new scope in which a variable is defined.
///
/// ```rust
/// # use scopegraphs::*;
/// # use completeness::{UncheckedCompleteness};
/// # use resolve::{DataWellformedness, Resolve, ResolvedPath};
/// # use render::{RenderSettings, RenderScopeData, RenderScopeLabel};
/// #
/// # #[derive(Label, Hash, PartialEq, Eq, Debug, Clone, Copy)]
/// # enum Lbl {
/// #     Lex,
/// #     Imp,
/// #     Def,
/// # }
/// # use Lbl::*;
/// #
/// # #[derive(Hash, PartialEq, Eq, Debug, Default, Clone)]
/// # enum Data<'a> {
/// #     #[default]
/// #     NoData,
/// #     Variable {
/// #         name: &'a str,
/// #     },
/// # }
/// #
/// # impl RenderScopeData for Data<'_> {
/// #     fn render_node(&self) -> Option<String> {
/// #         Some(format!("{self:?}"))
/// #     }
/// # }
/// #
/// # impl RenderScopeLabel for Lbl {
/// #     fn render(&self) -> String {
/// #         match self {
/// #             Lex => "lex",
/// #             Imp => "imp",
/// #             Def => "def",
/// #         }.to_string()
/// #     }
/// # }
/// #
/// # let storage = Storage::new();
/// # let sg: ScopeGraph<Lbl, Data, UncheckedCompleteness> =
/// # unsafe { ScopeGraph::raw(&storage) };
/// // make a new scope, with default data (`NoData`)
/// let s0 = sg.add_scope_default();
/// // the same as above
/// let s1 = sg.add_scope(Data::NoData);
/// // create a scope in which the variable `example` is defined
/// let s2 = sg.add_scope(Data::Variable {name: "example"});
///
/// sg.render_to("output.mmd", RenderSettings::default()).unwrap()
/// ```
///
/// <div class="warning">
///
/// Note: if you want to have multiple variable declarations in one scope, you likely do not want to simply make the Data a [`Vec`](std::vec::Vec).
/// Instead, it's common to make declarations their own scope. We'll look at examples of this in the chapter on [edges](crate::concepts::edges)
///
/// </div>
///
/// # Visualizing Scope Data
///
/// Because scope data is custom, how it's rendered can also be customized.
/// To do so, implement the [`RenderScopeData`](crate::render::RenderScopeData) trait for your scope's data.
/// In the above examples, we've used this hidden implementation of RenderScopeData:
///
/// ```
/// # use scopegraphs::*;
/// # use completeness::{UncheckedCompleteness};
/// # use resolve::{DataWellformedness, Resolve, ResolvedPath};
/// # use render::{RenderSettings, RenderScopeData, RenderScopeLabel};
/// # #[derive(Hash, PartialEq, Eq, Debug, Default, Clone)]
/// # enum Data<'a> {
/// #     #[default]
/// #     NoData,
/// #     Variable {
/// #         name: &'a str,
/// #     },
/// # }
/// #
/// impl RenderScopeData for Data<'_> {
///     fn render_node(&self) -> Option<String> {
///         Some(format!("{self:?}"))
///     }
/// }
/// ```
pub mod scope_data {}

#[render_scopegraphs]
/// Edges connect [`Scopes`](crate::concepts::scope)
pub mod edges {}

pub mod completeness;
pub mod data_wellformedness;
pub mod data_equivalence;
pub mod path_wellformedness;

/// # Label Ordering
///
/// ```mermaid
/// graph LR
///     s([Source]) --> a[[aquamarine]]
///     r[[rustdoc]] --> f([Docs w/ Mermaid!])
///     subgraph rustc[Rust Compiler]
///     a -. "inject mermaid.js" .-> r
///     end
/// ```
pub mod label_ordering {}
