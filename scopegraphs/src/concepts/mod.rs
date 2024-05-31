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


/// A scope is a node in a scope graph.
///
///
///
/// # use scopegraphs_render_docs::render_scopegraphs;
#[render_scopegraphs]
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
/// let s0 = sg.add_scope_default();
/// let s1 = sg.add_scope_default();
/// sg.add_edge(s0, Lex, s1);
/// sg.add_decl(s0, Def, Data { name: "x", data: 0 });
/// sg.add_decl(s1, Def, Data { name: "x", data: 1 });
///
/// sg.render_to("output.dot", RenderSettings::default()).unwrap();
/// ```
pub mod scope {}
pub mod scope_data;
pub mod edges;

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
