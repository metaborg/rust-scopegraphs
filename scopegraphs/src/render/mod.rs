//! Render scope graphs to graphviz `.dot` files.
//!
//! Generally, use `sg.render_to(filename, Settings::default()` for the most basic rendering.

use crate::completeness::Completeness;
use crate::resolve::ResolvedPath;
use crate::{Scope, ScopeGraph};
use std::fs::File;
use std::io;
use std::io::Write;
use std::path::Path;

mod traverse;

/// The backend to generate a graph for
#[derive(Default, Copy, Clone, Debug)]
pub enum Target {
    /// Generate graphviz dot code
    Dot,
    /// Generate a mermaid graph
    #[default]
    Mermaid,
}

/// Global settings related to rendering scope graphs.
pub struct RenderSettings<'sg, LABEL, DATA> {
    /// Whether to display label text next to edges
    pub show_edge_labels: bool,
    /// The title which should be displayed above the graph.
    ///
    /// Defaults to the filename given to [`ScopeGraph::render_to`].
    pub title: Option<String>,
    /// The output format to use for the visualization.
    pub target: Target,
    /// A resolved path that should also be rendered. Useful for debugging queries.
    pub path: Option<ResolvedPath<'sg, LABEL, DATA>>,
}

impl<'sg, LABEL, DATA> RenderSettings<'sg, LABEL, DATA> {
    /// Sets the name of the scope graph
    pub fn with_name(mut self, name: impl AsRef<str>) -> Self {
        self.title = Some(name.as_ref().to_string());
        self
    }
}

impl<'sg, LABEL, DATA> Default for RenderSettings<'sg, LABEL, DATA> {
    fn default() -> Self {
        Self {
            show_edge_labels: true,
            title: None,
            target: Default::default(),
            path: None,
        }
    }
}

/// The style of an edge (TODO)
pub struct EdgeStyle {}

/// An edge from one scope to another
pub struct Edge {
    /// the source scope, where the edge starts
    pub from: Scope,
    /// A description of the destination, as well as all actual edge information.
    pub to: EdgeTo,
}

/// Information about the destination and style of an edge from one scope to another.
pub struct EdgeTo {
    /// the destination
    pub to: Scope,
    /// what style does this edge get?
    pub edge_style: EdgeStyle,
    /// What label is displayed next to the edge
    ///
    /// Note: this label is hidden if you disable `show_label_text` in [`RenderSettings`].
    pub label_text: String,
}

/// Modifies how an edge label is rendered.
pub trait RenderScopeLabel {
    /// Render a single label
    fn render(&self) -> String;
}

/// Modifies how a scope is rendered based on user-defined scope data.
pub trait RenderScopeData {
    /// Renders a scope (or probably rather, the data in a scope)
    /// in the scope graph. This will be shown in the middle of the node.
    ///
    /// Can return None if there's no data to render.
    fn render_node(&self) -> Option<String> {
        None
    }

    /// Renders a scope (or probably rather, the data in a scope)
    /// in the scope graph. This will be shown next to the node.
    ///
    /// Can return None if there's no data to render.
    fn render_node_label(&self) -> Option<String> {
        None
    }

    /// Returns any extra edge your scope might want to render.
    ///
    /// If a scope's data contains a reference to another scope,
    /// this is like a hidden edge you might want to draw.
    fn extra_edges(&self) -> Vec<EdgeTo> {
        Vec::new()
    }

    /// Returns whether this scope is a definition of some variable
    ///
    /// Defaults to whether the outcome of [`render`](RenderScopeData::render_node_label) is Some,
    /// because often non-definition scopes have no data associated with them.
    fn definition(&self) -> bool {
        self.render_node().is_some()
    }

    /// Allows you to relabel specific scope IDs, can be useful for documentation purposes.
    fn explicit_relabel(&self, _scope_id: usize) -> Option<String> {
        None
    }
}

fn scope_to_node_name(s: Scope) -> String {
    format!("scope_{}", s.0)
}

fn escape_text_dot(inp: &str) -> String {
    inp.replace('"', "\\\"")
}

fn escape_text_mermaid(inp: &str) -> String {
    inp.replace('"', "&amp;quot;")
}

impl<
        LABEL: Clone + RenderScopeLabel,
        DATA: RenderScopeData + Clone,
        CMPL: Completeness<LABEL, DATA>,
    > ScopeGraph<'_, LABEL, DATA, CMPL>
{
    /// Visualize the entire scope graph as a graph, by emitting a graphviz dot file.
    ///
    /// Note: you can also visualize a [single regular expression this way](crate::Automaton::render)
    pub fn render<W: Write>(
        &self,
        output: &mut W,
        settings: RenderSettings<'_, LABEL, DATA>,
    ) -> io::Result<()> {
        match settings.target {
            Target::Dot => self.render_dot(output, settings),
            Target::Mermaid => self.render_mermaid(output, settings),
        }
    }

    fn render_mermaid<W: Write>(
        &self,
        output: &mut W,
        settings: RenderSettings<'_, LABEL, DATA>,
    ) -> io::Result<()> {
        let (mut edges, nodes) = traverse::traverse(self);

        if let Some(ref i) = settings.title {
            // title
            writeln!(output, "---")?;
            writeln!(output, r#"title: {}"#, escape_text_mermaid(i))?;
            writeln!(output, "---")?;
        }

        writeln!(output, "flowchart LR")?;

        // color scheme
        // writeln!(
        //     output,
        //     r#"node [colorscheme="ylgnbu6",width="0.1",height="0.1",margin="0.01",xlp="b"]"#
        // )?;

        // straight edges
        // writeln!(output, r#"splines=false;"#)?;

        // nodes
        for (scope, data) in nodes {
            edges.extend(
                data.extra_edges()
                    .into_iter()
                    .map(|to| Edge { from: scope, to }),
            );
            let name = scope_to_node_name(scope);

            let mut attrs = Vec::new();

            let label = escape_text_mermaid(
                &data
                    .render_node()
                    .or_else(|| data.explicit_relabel(scope.0))
                    .unwrap_or_else(|| scope.0.to_string()),
            );

            if let Some(label) = data.render_node_label() {
                attrs.push(format!(r#"[xlabel="{}"]"#, escape_text_mermaid(&label)))
            }

            attrs.push(r#"[penwidth="2.0"]"#.to_string());

            // writeln!(output, r#"{name} {}"#, attrs.join(""))?
            if data.definition() {
                writeln!(output, r#"    {name}["{label}"]"#)?;
            } else {
                writeln!(output, r#"    {name}(("{label}"))"#)?;
            }
        }

        // edges
        for (idx, edge) in edges.iter().enumerate() {
            let (from, to) = (edge.from, edge.to.to);

            let from_str = scope_to_node_name(from);
            let to_str = scope_to_node_name(to);
            let label = escape_text_mermaid(&edge.to.label_text);

            if settings.show_edge_labels {
                writeln!(output, r#"    {from_str} -->|"{label}"| {to_str}"#)?
            } else {
                writeln!(output, "    {from_str} --> {to_str}")?
            }

            if let Some(ref i) = settings.path {
                let mut prev = None;
                let mut part_of_path = false;
                for curr in i.scopes() {
                    if let Some(ref mut prev) = prev {
                        if (to, from) == (*prev, curr) {
                            part_of_path = true;
                            break;
                        }

                        *prev = curr;
                    } else {
                        prev = Some(curr);
                    }
                }

                let mut link_style = Vec::new();
                if part_of_path {
                    link_style.push("stroke: red".to_string());
                }

                if !link_style.is_empty() {
                    writeln!(output, "    linkStyle {idx} {}", link_style.join(","))?;
                }
            }
        }

        if let Some(i) = settings.path {
            writeln!(output, r#"    path-start["start of lookup"]"#)?;

            if let Some(first_scope) = i.scopes().last() {
                let to_str = scope_to_node_name(first_scope);
                writeln!(output, "    path-start --> {to_str}")?;
                writeln!(output, "    linkStyle {} stroke: red", edges.len())?;
            }
        }

        Ok(())
    }

    fn render_dot<W: Write>(
        &self,
        output: &mut W,
        settings: RenderSettings<'_, LABEL, DATA>,
    ) -> io::Result<()> {
        let (mut edges, nodes) = traverse::traverse(self);

        writeln!(output, "digraph {{")?;

        // color scheme
        writeln!(
            output,
            r#"node [colorscheme="ylgnbu6",width="0.1",height="0.1",margin="0.01",xlp="b"]"#
        )?;

        if let Some(ref i) = settings.title {
            // title
            writeln!(output, r#"labelloc="t";"#)?;
            writeln!(output, r#"label="{}";"#, escape_text_dot(i))?;
        }

        // straight edges
        writeln!(output, r#"splines=false;"#)?;

        // nodes
        for (scope, data) in nodes {
            edges.extend(
                data.extra_edges()
                    .into_iter()
                    .map(|to| Edge { from: scope, to }),
            );
            let name = scope_to_node_name(scope);

            let mut attrs = Vec::new();

            if data.definition() {
                attrs.push(r#"[shape="square"]"#.to_string())
            } else {
                attrs.push(r#"[shape="circle"]"#.to_string())
            };
            let label = escape_text_dot(&data.render_node().unwrap_or_else(|| scope.0.to_string()));
            attrs.push(format!(r#"[label="{label}"]"#));

            if let Some(label) = data.render_node_label() {
                attrs.push(format!(r#"[xlabel="{}"]"#, escape_text_dot(&label)))
            }

            attrs.push(r#"[penwidth="2.0"]"#.to_string());

            writeln!(output, r#"{name} {}"#, attrs.join(""))?
        }

        // edges
        for edge in edges {
            let from = scope_to_node_name(edge.from);
            let to = scope_to_node_name(edge.to.to);
            let label = edge.to.label_text;

            if settings.show_edge_labels {
                writeln!(output, "{from} -> {to} [label={label}]")?
            } else {
                writeln!(output, "{from} -> {to}")?
            }
        }

        writeln!(output, "}}")?;

        Ok(())
    }

    /// [`render`](ScopeGraph::render) directly to a file.
    pub fn render_to(
        &self,
        path: impl AsRef<Path>,
        mut settings: RenderSettings<'_, LABEL, DATA>,
    ) -> io::Result<()> {
        let path = path.as_ref();
        let mut w = File::create(path)?;

        if settings.title.is_none() {
            settings.title = Some(
                path.file_stem()
                    .expect("path must have filename for File::create to work")
                    .to_string_lossy()
                    .to_string(),
            );
        }

        self.render(&mut w, settings)
    }
}
