use super::ExplicitClose;

/// Completeness implementation that leverages the [Drop] trait to close edges.
#[derive(Debug, Default)]
pub struct DropClose<LABEL> {
    explicit_close: ExplicitClose<LABEL>,
}

impl<LABEL> Sealed for DropClose<LABEL> {}
