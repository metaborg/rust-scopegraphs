![Crates.io MSRV](https://img.shields.io/crates/msrv/scopegraphs?style=for-the-badge)
![docs.rs](https://img.shields.io/docsrs/scopegraphs?style=for-the-badge)
![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/metaborg/rust-scopegraphs/rust.yml?style=for-the-badge)

Scope graphs are an abstraction that allow you to express the complicated
name resolution patterns that many programming languages have.
Put simply, a scope graph encodes what names are defined in which scopes of
a program, and how scopes relate to each other.
Then, we can run queries over this graph to create links from usages of names
to definitions of names.
However, to make name resolution flexible,
the building of the graph and the querying over the graph can happen concurrently:
we don't need an entire graph before we can start querying it.

This library, and its documentation serve as both a kind of reference implementation of scope graphs,
a usable library for your programming language,
a tutorial of how to use scope graphs
and a tutorial of how you could implement scope graphs yourself.

## Research

Scope graphs are based on research.
These are some papers that introduce the topic in a more scientific fashion than we will here.
That is on purpose: The documentation of this library are meant to be the more informal explanation of scope graphs.

* [Néron, P., Tolmach, A., Visser, E., & Wachsmuth, G. (2015). A theory of name resolution.](https://web.cecs.pdx.edu/~apt/esop15.pdf)
  Containing first introduction of scope graphs.
* [van Antwerpen, H., Bach Poulsen, C., Rouvoet, A., & Visser, E. (2018). Scopes as types.](https://repository.tudelft.nl/islandora/object/uuid:9aad733b-23d4-45d7-b52f-331b80c5d029/datastream/OBJ/download)
  Presents a refinement of the older scope graphs, which this library is based on.
* [Zwaan, A., & van Antwerpen, H. (2023). Scope graphs: The story so far.](https://repository.tudelft.nl/islandora/object/uuid:3024d587-7c5d-44bd-8471-27b7c2e59160/datastream/OBJ/download)
  Provides a more detailed overview of all work that involved scope graphs until the date of publication.

But more research is ongoing! See
our [research overview page](https://pl.ewi.tudelft.nl/research/projects/scope-graphs/) for an overview of all research
that involves scope
graphs.