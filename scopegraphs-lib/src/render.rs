use crate::completeness::Completeness;
use crate::{Scope, ScopeGraph};
use dot::LabelText::LabelStr;
use dot::{Edges, Id, LabelText, Nodes};
use std::borrow::Cow;
use std::fmt::Debug;
use std::fs::File;
use std::io;
use std::io::Write;
use std::path::Path;

pub trait RenderScopeData {
    /// Renders a scope (or probably rather, the data in a scope).
    /// Can return None if there's no data to render.
    fn render(&self) -> Option<String>;

    /// Returns whether this scope is a definition of some variable
    ///
    /// Defaults to whether the outcome of [`render`](RenderScopeData::render) is Some,
    /// because often non-definition scopes have no data associated with them.
    fn definition(&self) -> bool {
        self.render().is_some()
    }
}

impl<LABEL: Clone + Debug, DATA: RenderScopeData + Clone, CMPL: Completeness<LABEL, DATA>>
    ScopeGraph<'_, LABEL, DATA, CMPL>
{
    pub fn render<W: Write>(&self, output: &mut W, name: &str) -> io::Result<()> {
        dot::render(&(self, name), output)
    }

    pub fn render_to(&self, path: impl AsRef<Path>) -> io::Result<()> {
        let path = path.as_ref();
        let mut w = File::create(path)?;
        let name = path
            .file_stem()
            .expect("path must have filename for File::create to work")
            .to_string_lossy();
        self.render(&mut w, &name)
    }
}

impl<'a, LABEL: Clone + Debug, DATA: Clone + RenderScopeData, CMPL: Completeness<LABEL, DATA>>
    dot::Labeller<'a, Scope, (Scope, LABEL, Scope)> for (&ScopeGraph<'_, LABEL, DATA, CMPL>, &str)
{
    fn graph_id(&'a self) -> Id<'a> {
        Id::new(self.1).unwrap()
    }

    fn node_id(&'a self, n: &Scope) -> Id<'a> {
        Id::new(format!("s{}", n.0)).unwrap()
    }

    fn node_label(&self, n: &Scope) -> LabelText {
        let data = self.0.get_data(*n);
        LabelText::label(data.render().unwrap_or_else(|| format!("scope {}", n.0)))
    }
    fn edge_label(&self, edge: &(Scope, LABEL, Scope)) -> LabelText {
        LabelText::label(format!("{:?}", edge.1))
    }

    fn node_shape(&'a self, node: &Scope) -> Option<LabelText<'a>> {
        let data = self.0.get_data(*node);
        if data.definition() {
            Some(LabelStr(Cow::Borrowed("box")))
        } else {
            Some(LabelStr(Cow::Borrowed("circle")))
        }
    }
}

impl<'a, LABEL: Clone, DATA: Clone, CMPL> dot::GraphWalk<'a, Scope, (Scope, LABEL, Scope)>
    for (&ScopeGraph<'_, LABEL, DATA, CMPL>, &str)
{
    fn nodes(&'a self) -> Nodes<'a, Scope> {
        (0..self.0.inner_scope_graph.data.borrow().len())
            .map(Scope)
            .collect()
    }

    fn edges(&'a self) -> Edges<'a, (Scope, LABEL, Scope)> {
        self.0
            .inner_scope_graph
            .edges
            .borrow()
            .iter()
            .enumerate()
            .flat_map(|(scope, edges)| {
                (*edges)
                    .borrow()
                    .iter()
                    .flat_map(|(lbl, edges_with_lbl)| {
                        edges_with_lbl
                            .iter()
                            .map(|edge| (Scope(scope), lbl.clone(), *edge))
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>()
            })
            .collect()
    }

    fn source(&'a self, edge: &(Scope, LABEL, Scope)) -> Scope {
        edge.0
    }

    fn target(&'a self, edge: &(Scope, LABEL, Scope)) -> Scope {
        edge.2
    }
}
