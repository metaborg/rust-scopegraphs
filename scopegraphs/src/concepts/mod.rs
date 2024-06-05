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
/// Other examples of scopes are
/// * Functions
/// * Any other block using braces like `if` and `loop`
/// * Modules
/// * Structs, or classes in other languages than Rust (they contain fields)
/// * traits (they contain methods)
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
///
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
/// Note: if you want to have multiple variable definitions in one scope, you likely do not want to simply make the Data a [`Vec`](std::vec::Vec).
/// Instead, it's common to make definitions their own scope. We'll look at examples of this in the chapter on [edges](crate::concepts::edges)
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
/// Edges connect [`Scopes`](crate::concepts::scope) together, based on the rules of the language you're trying to create.
///
/// By adding edges, you're really starting to use scope graphs.
///
/// You can think of edges as the path a name resolution can take. Edges are always directed.
/// Let's construct a simple scope graph with some edges, and discuss it:
///
/// ```rust
/// # use scopegraphs::*;
/// # use completeness::{UncheckedCompleteness};
/// # use resolve::{DataWellformedness, Resolve, ResolvedPath};
/// # use render::{RenderSettings, RenderScopeData, RenderScopeLabel};
/// #
/// #[derive(Label, Hash, PartialEq, Eq, Debug, Clone, Copy)]
/// enum Lbl {
///      Lexical,
///      Definition,
/// }
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
/// #         match self {
/// #             Self::Variable {..} => Some(format!("{self:?}")),
/// #             _ => None,
/// #         }
/// #     }
/// #
/// #     fn definition(&self) -> bool {
/// #         matches!(self, Self::Variable {..})
/// #     }
/// # }
/// #
/// # impl RenderScopeLabel for Lbl {
/// #     fn render(&self) -> String {
/// #         match self {
/// #             Self::Lexical => "lexical",
/// #             Self::Definition => "definition",
/// #         }.to_string()
/// #     }
/// # }
/// #
/// # let storage = Storage::new();
/// # let sg: ScopeGraph<Lbl, Data, UncheckedCompleteness> =
/// # unsafe { ScopeGraph::raw(&storage) };
///
/// let global = sg.add_scope_default();
/// let fn_foo = sg.add_scope_default();
///
/// // create a scope in which the variable `bar` is defined
/// let declares_a_global = sg.add_scope(Data::Variable {name: "bar"});
///
/// // create another scope in which the variable `bar` is defined inside foo
/// let declares_a_local_in_foo = sg.add_scope(Data::Variable {name: "baz"});
///
/// // Add some edges
/// sg.add_edge(fn_foo, Lbl::Lexical, global);
///
/// sg.add_edge(global, Lbl::Definition, declares_a_global);
/// sg.add_edge(fn_foo, Lbl::Definition, declares_a_local_in_foo);
///
/// sg.render_to("output.mmd", RenderSettings::default()).unwrap()
/// ```
///
/// Alright, let's analyse the example!
///
/// In total we define 4 scopes.
/// * A global scope, which in the visualization gets ID 0
/// * A local scope, which in the visualization gets ID 1
/// * A scope in which `bar` is defined
/// * A scope in which `baz` is defined
///
/// Note, it's common to create new scopes for each variable definition like this.
///
/// A rough approximation of a program which would have such a scope structure would be:
/// ```ignore
/// // in the global scope
/// let bar = 3;
/// fn foo() {
///     // in foo's scope
///     let baz = 4;
/// }
/// ```
///
/// We gave the four scopes various edges between to reflect this program structure
///
/// * there's an edge between `foo`'s scope and the global scope labelled `Lexical`,
///   which represents the fact that the function `foo` is *in* the global scope, and code *in*
///   foo can access the global scope.
/// * there are two edges labelled `Definition`, to represent that a certain name is defined in that scope.
///     * `bar` is declared
///     * `baz` is declared in fn `foo`
///
/// As you can see, edges here have *labels*.
/// Labels can be an arbitrary type, in the example it's an enum with several options.
/// Simply put, labels define in _what way_ two scopes are related. `Lexical` and `Definition` are common labels,
/// but you could also imagine a `Imports`, `Inherits` or `TypeDefinition` label being useful in a programming language.
/// These labels can later be used to give direction to [queries](crate::concepts) over the scope graph.
///
/// # Looking up symbols in a scope graph
///
/// The point of a scope graph is that after we have one, we can use it to compute name resolution.
/// Although we won't get into [queries](crate::concepts) yet, what would such a lookup look like for the tiny
/// example we've just constructed?
///
/// ## Example 1: in the local scope
///
/// Let's go for the simple case first. Let's say we now write the following example:
///
/// ```ignore
/// // in the global scope
/// let bar = 3;
/// fn foo() {
///     // in foo's scope
///     let baz = 4;
///
///     println!("{}", baz);
/// }
/// ```
///
/// On the last line, we want to look up the definition of `baz` to print it.
/// So first, we look in the current scope: `foo`'s scope. We immediately find a `Definition`
/// edge which brings us to a variable definition with the name `baz`. So we're done!
///
/// ```rust
/// # use scopegraphs::*;
/// # use completeness::{UncheckedCompleteness};
/// # use resolve::{DataWellformedness, Resolve, ResolvedPath};
/// # use render::{RenderSettings, RenderScopeData, RenderScopeLabel};
/// #
/// # #[derive(Label, Hash, PartialEq, Eq, Debug, Clone, Copy)]
/// # enum Lbl {
/// #      Lexical,
/// #      Definition,
/// # }
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
/// #         match self {
/// #             Self::Variable {..} => Some(format!("{self:?}")),
/// #             _ => None,
/// #         }
/// #     }
/// #
/// #     fn definition(&self) -> bool {
/// #         matches!(self, Self::Variable {..})
/// #     }
/// # }
/// #
/// # impl RenderScopeLabel for Lbl {
/// #     fn render(&self) -> String {
/// #         match self {
/// #             Self::Lexical => "lexical",
/// #             Self::Definition => "definition",
/// #         }.to_string()
/// #     }
/// # }
/// #
/// # let storage = Storage::new();
/// # let sg: ScopeGraph<Lbl, Data, UncheckedCompleteness> =
/// # unsafe { ScopeGraph::raw(&storage) };
/// #
/// # let global = sg.add_scope_default();
/// # let fn_foo = sg.add_scope_default();
/// #
/// # // create a scope in which the variable `bar` is defined
/// # let declares_a_global = sg.add_scope(Data::Variable {name: "bar"});
/// #
/// # // create another scope in which the variable `bar` is defined inside foo
/// # let declares_a_local_in_foo = sg.add_scope(Data::Variable {name: "baz"});
/// #
/// # // Add some edges
/// # sg.add_edge(fn_foo, Lbl::Lexical, global);
/// #
/// # sg.add_edge(global, Lbl::Definition, declares_a_global);
/// # sg.add_edge(fn_foo, Lbl::Definition, declares_a_local_in_foo);
/// #
/// # let res = sg.query()
/// #     .with_path_wellformedness(query_regex!(Lbl: Lexical* Definition))
/// #     .with_data_wellformedness(|a: &Data| matches!(a, Data::Variable {name: "baz"}))
/// #     .resolve(fn_foo);
/// #
/// # sg.render_to("output.mmd", RenderSettings {
/// #     path: Some(res.get_only_item().unwrap()),
/// #     ..Default::default()
/// # }).unwrap()
/// ```
///
/// ## Example 2: in the enclosing (global) scope
/// Alright, now for a slightly more complicated example:
///
/// ```ignore
/// // in the global scope
/// let bar = 3;
/// fn foo() {
///     // in foo's scope
///     let baz = 4;
///
///     println!("{}", bar);
/// }
/// ```
///
/// Now, we cannot find a definition of `bar` in `foo`'s scope directly.
/// So, we can choose to instead traverse the `Lexical` edge to look in the global scope.
/// Now we *can* find a definition of `bar` (using a `Definition` edge), so we're done.
///
/// ```rust
/// # use scopegraphs::*;
/// # use completeness::{UncheckedCompleteness};
/// # use resolve::{DataWellformedness, Resolve, ResolvedPath};
/// # use render::{RenderSettings, RenderScopeData, RenderScopeLabel};
/// #
/// # #[derive(Label, Hash, PartialEq, Eq, Debug, Clone, Copy)]
/// # enum Lbl {
/// #      Lexical,
/// #      Definition,
/// # }
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
/// #         match self {
/// #             Self::Variable {..} => Some(format!("{self:?}")),
/// #             _ => None,
/// #         }
/// #     }
/// #
/// #     fn definition(&self) -> bool {
/// #         matches!(self, Self::Variable {..})
/// #     }
/// # }
/// #
/// # impl RenderScopeLabel for Lbl {
/// #     fn render(&self) -> String {
/// #         match self {
/// #             Self::Lexical => "lexical",
/// #             Self::Definition => "definition",
/// #         }.to_string()
/// #     }
/// # }
/// #
/// # let storage = Storage::new();
/// # let sg: ScopeGraph<Lbl, Data, UncheckedCompleteness> =
/// # unsafe { ScopeGraph::raw(&storage) };
/// #
/// # let global = sg.add_scope_default();
/// # let fn_foo = sg.add_scope_default();
/// #
/// # // create a scope in which the variable `bar` is defined
/// # let declares_a_global = sg.add_scope(Data::Variable {name: "bar"});
/// #
/// # // create another scope in which the variable `bar` is defined inside foo
/// # let declares_a_local_in_foo = sg.add_scope(Data::Variable {name: "baz"});
/// #
/// # // Add some edges
/// # sg.add_edge(fn_foo, Lbl::Lexical, global);
/// #
/// # sg.add_edge(global, Lbl::Definition, declares_a_global);
/// # sg.add_edge(fn_foo, Lbl::Definition, declares_a_local_in_foo);
/// #
/// # let res = sg.query()
/// #     .with_path_wellformedness(query_regex!(Lbl: Lexical* Definition))
/// #     .with_data_wellformedness(|a: &Data| matches!(a, Data::Variable {name: "bar"}))
/// #     .resolve(fn_foo);
/// #
/// # sg.render_to("output.mmd", RenderSettings {
/// #     path: Some(res.get_only_item().unwrap()),
/// #     ..Default::default()
/// # }).unwrap()
/// ```
///
/// ## Example 3: when name resolution fails
///
/// Finally, let's look at an example in which name resolution should obviously fail,
/// and discuss why it does, using the scope graph we constructed.
///
/// ```ignore
/// // in the global scope
/// let bar = 3;
/// fn foo() {
///     // in foo's scope
///     let baz = 4;
/// }
/// println!("{}", baz);
/// ```
/// Now, we try to access `baz` from the global scope,
/// but we shouldn't be able to because `baz` is defined inside `foo`'s scope.
///
/// So what would happen when we performed name resolution here?
/// Well, we would start in the global scope. There's only a definition of `bar` there,
/// so we don't instantly succeed. However, we also cannot look any further.
/// There's no `Lexical` edge from the global scope to `foo`'s scope.
///
/// Thus, no results are found when looking up `baz` here.
///
/// <div class="warning">
///
/// I've heard of people who are confused by the direction of the edges. They would say something like:
/// In the example, `foo` is _inside of_ the global scope, so why is there an edge _from_ `foo` _towards_
/// the global scope, and not the other way around?
///
/// Keep in mind that the edge direction specifies the direction in which we can walk the graph to find new variable names.
/// So, if in `foo`'s scope, we want to access a global variable,
/// we can do that by using the edge _towards_ the global scope and find all the variables there.
///
/// Conversely, in the global scope, we cannot access variables in `foo`'s scope,
/// which is why there is no edge _from_ the global scope _towards_ `foo`'s scope.
///
/// </div>
///
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
