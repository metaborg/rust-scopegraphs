use crate::{resolve::Path, Scope};

pub trait ScopeContainer<LABEL> {
    type PathContainer;

    fn lift_step(self, lbl: LABEL, prefix: &Path<LABEL>) -> Self::PathContainer;
}

pub fn lift_step_iterator<'a, LABEL: Copy>(
    iter: impl Iterator<Item = Scope> + 'a,
    lbl: LABEL,
    prefix: &'a Path<LABEL>,
) -> impl Iterator<Item = Path<LABEL>> + 'a {
    iter.filter_map(move |s| prefix.step(lbl, s))
}

impl<LABEL: Copy> ScopeContainer<LABEL> for Vec<Scope> {
    type PathContainer = Vec<Path<LABEL>>;

    fn lift_step(self, lbl: LABEL, prefix: &Path<LABEL>) -> Self::PathContainer {
        lift_step_iterator(self.into_iter(), lbl, prefix).collect()
    }
}

impl<LABEL, SC: ScopeContainer<LABEL>, E> ScopeContainer<LABEL> for Result<SC, E> {
    type PathContainer = Result<SC::PathContainer, E>;

    fn lift_step(self, lbl: LABEL, prefix: &Path<LABEL>) -> Self::PathContainer {
        self.map(|sc| sc.lift_step(lbl, prefix))
    }
}
