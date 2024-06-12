use crate::completeness::Completeness;
use crate::render::{Edge, EdgeStyle, EdgeTo, RenderScopeLabel};
use crate::{Scope, ScopeGraph};

pub fn traverse<'sg, LABEL: RenderScopeLabel, DATA, CMPL: Completeness<LABEL, DATA>>(
    sg: &'sg ScopeGraph<'_, LABEL, DATA, CMPL>,
) -> (Vec<Edge>, Vec<(Scope, &'sg DATA)>) {
    let edges: Vec<_> = sg
        .inner_scope_graph
        .edges
        .borrow()
        .iter()
        .enumerate()
        .flat_map(|(scope, edges)| {
            (*edges)
                .borrow()
                .as_ref()
                .iter()
                .zip(LABEL::iter())
                .flat_map(|(edges_with_lbl, lbl)| {
                    edges_with_lbl
                        .iter()
                        .map(|edge| Edge {
                            from: Scope(scope),
                            to: EdgeTo {
                                to: *edge,
                                edge_style: EdgeStyle {},
                                label_text: lbl.render(),
                            },
                        })
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let nodes = (0..sg.inner_scope_graph.data.borrow().len())
        .map(Scope)
        .map(|i| (i, sg.get_data(i)))
        .collect();

    (edges, nodes)
}
