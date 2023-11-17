use crate::{label::Label, scopegraph::ScopeGraph};

use super::Env;

pub fn resolve<'sg, SCOPE, LABEL, DATA>(
    _sg: &'sg ScopeGraph<SCOPE, LABEL, DATA>,
    // regexp : RegExp<LABEL>,
    _data_wellformedness: impl for<'a> Fn(&'a DATA) -> bool,
    _label_order: impl Fn(LABEL, LABEL) -> bool, // FIXME: LabelOrder trait
    _data_order: impl Fn(DATA, DATA) -> bool,
    _scope: &'sg SCOPE,
) -> Env<'sg, SCOPE, LABEL, DATA>
where
    LABEL: Label,
{
    todo!()
}
