//! Scope graphs are an abstraction that allow you to express the complicated
//! name resolution patterns that many programming languages have.
//! Put simply, a scope graph encodes what names are defined in which scopes of
//! a program, and how scopes relate to each other.
//! Then, we can run queries over this graph to create links from usages of names
//! to definitions of names.
//! However, to make name resolution flexible,
//! the building of the graph and the querying over the graph can happen concurrently:
//! we don't need an entire graph before we can start querying it.
//!
//! This library, and its documentation serve as both a kind of reference implementation of scope graphs,
//! a usable library for your programming language,
//! a tutorial of how to use scope graphs
//! and a tutorial of how you could implement scope graphs yourself.
//!
//! ## Research
//!
//! Scope graphs are based on research.
//! These are some papers that introduce the topic in a more scientific fashion than we will here.
//! That is on purpose: The documentation of this library are meant to be the more informal explanation of scope graphs.
//!
//! * [Néron, P., Tolmach, A., Visser, E., & Wachsmuth, G. (2015). A theory of name resolution.](https://web.cecs.pdx.edu/~apt/esop15.pdf)
//!   Containing first introduction of scope graphs.
//! * [van Antwerpen, H., Bach Poulsen, C., Rouvoet, A., & Visser, E. (2018). Scopes as types.](https://repository.tudelft.nl/islandora/object/uuid:9aad733b-23d4-45d7-b52f-331b80c5d029/datastream/OBJ/download)
//!   Presents a refinement of the older scope graphs, which this library is based on.
//! * [Zwaan, A., & van Antwerpen, H. (2023). Scope graphs: The story so far.](https://repository.tudelft.nl/islandora/object/uuid:3024d587-7c5d-44bd-8471-27b7c2e59160/datastream/OBJ/download)
//!   Provides a more detailed overview of all work that involved scope graphs until the date of publication.
//!
//! But more research is ongoing!
//!
//! ## This Documentation
//!
//! * [Explanation of Concepts used in scope graphs](_concepts)
//! * Examples:
//!     * [Standard patterns](_patterns)
//! * API Docs (you're there!)
#![cfg_attr(any(RUSTC_IS_NIGHTLY, docsrs), feature(doc_auto_cfg, doc_cfg))]

pub use scopegraphs_lib::*;
pub use scopegraphs_macros::*;
pub use scopegraphs_regular_expressions::*;

#[doc(hidden)]
mod docs_support {
    #[doc(hidden)]
    #[macro_export]
    macro_rules! docs {
        ($($(#[$meta:meta])* pub mod $name: ident);* $(;)?) => {
            #[cfg(feature = "doc")]
            mod docs {
                $(
                    $(#[$meta])*
                    pub mod $name;
                )*
            }
            #[doc(hidden)]
            mod __index {
                /// Documentation Index:
                pub mod _________________________________________ {
                    $(
                        pub use super::super::docs::$name;
                    )*
                }
            }


            $(
                #[cfg(feature = "doc")]
                $(#[$meta])*
                pub mod $name {
                    pub use super::docs::$name::*;

                    pub use super::__index::*;

                    pub use super::docs::*;
                }
            )*
        };
    }
}

#[doc(hidden)]
mod __hidden {
    pub mod ____________API_DOCS_BELOW___________ {}
}
#[doc(hidden)]
mod __hidden2 {
    pub mod _________DOCUMENTATION_BELOW________ {}
}

pub use __hidden::*;
pub use __hidden2::_________DOCUMENTATION_BELOW________;

docs! {
    /// # Concepts of scope graphs
    ///
    /// To use a scope graph,
    /// you need to understand a few basic concepts.
    /// First of all, the components that comprise a scope graph:
    ///
    /// * [A scope](scope)
    /// * [A scope's data](scope_data)
    /// * [Labelled edges between scopes](edges)
    ///
    /// Once you know what a scope graph is,
    /// we can start talking about running queries over them.
    /// For a query to return the desired result, we may need to
    /// specify the following properties:
    ///
    /// * [Regular Expression]()
    /// * [Completeness]()
    /// * [Data Well-Formedness]()
    /// * [Data Equivalence]()
    /// * [Label Ordering]()
    ///
    pub mod concepts;

    /// # Common patterns in scope graphs
    pub mod patterns;
}
