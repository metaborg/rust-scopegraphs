use std::slice::Iter;

pub trait ScopeGraph {
    type Scope;
    type Label;
    type Data;

    fn add_scope(&mut self, data: Self::Data) -> &Self::Scope;

    fn add_edge(&mut self, src: &Self::Scope, lbl: &Self::Label, dst: &Self::Scope);

    fn get_data(&self, scope: &Self::Scope) -> &Self::Data;

    fn get_edges(&self, scope: &Self::Scope, lbl: &Self::Data) -> Iter<'_, Self::Scope>;
}
