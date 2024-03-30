use crate::{resolve::Path, Scope};

/// Interface for scope containers that support the operations required for query resolution.
pub trait ScopeContainer<LABEL> {
    /// The type containing paths obtained after stepping to this scope.
    type PathContainer;

    /// Lift the [`Path::step`] operation into this container.
    ///
    /// Should retain the contract that for all scopes `s` in `self`, `prefix.step(lbl, s)` is
    /// included in the resulting path container (excpet cyclic paths).
    fn lift_step(self, lbl: LABEL, prefix: &Path<LABEL>) -> Self::PathContainer;
}

impl<LABEL: Copy> ScopeContainer<LABEL> for Vec<Scope> {
    type PathContainer = Vec<Path<LABEL>>;

    fn lift_step(self, lbl: LABEL, prefix: &Path<LABEL>) -> Self::PathContainer {
        self.into_iter()
            .filter_map(move |s| prefix.step(lbl, s))
            .collect()
    }
}

impl<LABEL, SC: ScopeContainer<LABEL>, E> ScopeContainer<LABEL> for Result<SC, E> {
    type PathContainer = Result<SC::PathContainer, E>;

    fn lift_step(self, lbl: LABEL, prefix: &Path<LABEL>) -> Self::PathContainer {
        self.map(|sc| sc.lift_step(lbl, prefix))
    }
}
