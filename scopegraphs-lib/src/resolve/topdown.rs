use std::hash::Hash;

use scopegraphs_regular_expressions::RegexMatcher;

use crate::{label::Label, scopegraph::ScopeGraph};
use super::{Env, Path, ResolvedPath};

pub fn resolve<'sg, SCOPE, LABEL, DATA>(
    sg: &'sg ScopeGraph<SCOPE, LABEL, DATA>,
    path_wellformedness : impl RegexMatcher<LABEL>,
    data_wellformedness: impl for<'a> Fn(&'a DATA) -> bool,
    label_order: impl Fn(LABEL, LABEL) -> bool, // FIXME: LabelOrder trait
    data_order: impl for<'a> Fn(&'a DATA, &'a DATA) -> bool,
    source: &'sg SCOPE,
) -> Env<'sg, SCOPE, LABEL, DATA>
where
    SCOPE: Hash + Eq,
    LABEL: Hash + Eq + Label,
    DATA: Hash + Eq,
{
    todo!()
}



fn resolve_all<'sg, SCOPE, LABEL, DATA>(
    _sg: &'sg ScopeGraph<SCOPE, LABEL, DATA>,
    _path_wellformedness : impl RegexMatcher<LABEL>,
    _data_wellformedness: impl for<'a> Fn(&'a DATA) -> bool,
    _label_order: impl Fn(LABEL, LABEL) -> bool, // FIXME: LabelOrder trait
    _data_order: impl for<'a> Fn(&'a DATA, &'a DATA) -> bool,
    _path: Path<'sg, SCOPE, LABEL>,
) -> Env<'sg, SCOPE, LABEL, DATA>
where
    LABEL: Label,
{
    todo!()
}


fn resolve_path<'sg, SCOPE, LABEL, DATA>(
    sg: &'sg ScopeGraph<SCOPE, LABEL, DATA>,
    data_wellformedness: impl for<'a> Fn(&'a DATA) -> bool,
    path: Path<'sg, SCOPE, LABEL>,
) -> Option<ResolvedPath<'sg, SCOPE, LABEL, DATA>> {
    let data = sg.get_data(path.target()); 
    data_wellformedness(data).then(|| path.resolve(data))
}
