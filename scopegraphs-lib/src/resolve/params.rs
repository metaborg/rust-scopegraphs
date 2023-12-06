use crate::resolve::EdgeOrData;

/// Unary predicate over `DATA`.
///
/// Used to select declarations that a query can resolve to.
pub trait DataWellformedness<DATA> {
    fn data_wf(&self, data: &DATA) -> bool;
}

impl<DATA, T> DataWellformedness<DATA> for T
where
    for<'sg> T: Fn(&'sg DATA) -> bool,
{
    fn data_wf(&self, data: &DATA) -> bool {
        self(data)
    }
}

#[derive(Default)]
pub struct DefaultDataWellformedness {}

impl<DATA> DataWellformedness<DATA> for DefaultDataWellformedness {
    fn data_wf(&self, _data: &DATA) -> bool {
        true // match all data by default
    }
}

/// Strict partial order on labels. Used to perform shadowing.
///
/// For example, suppose that in some scope `s`, declarations for some query are reachable via an
/// `Lex` edge and an `Imp` edge (for lexical parent and import, respectively). When the label order
/// Indicates `Lex < Imp` (i.e., declarations from a lexically enclosing scope have higher priority),
/// the declaration over the `Imp` edge is shadowed, and will thus not be included in the
/// environment. If `Imp < Lex`, imports have higher priority, and that one will be included.
/// Otherwise, paths to both declarations are included in the environment.
pub trait LabelOrder<LABEL> {
    fn less_than(&self, l1: &EdgeOrData<LABEL>, l2: &EdgeOrData<LABEL>) -> bool;
}
impl<LABEL, T> LabelOrder<LABEL> for T
where
    T: for<'a, 'b> Fn(&'a EdgeOrData<LABEL>, &'b EdgeOrData<LABEL>) -> bool,
{
    fn less_than(&self, l1: &EdgeOrData<LABEL>, l2: &EdgeOrData<LABEL>) -> bool {
        self(l1, l2)
    }
}

#[derive(Default)]
pub struct DefaultLabelOrder {}

impl<LABEL> LabelOrder<LABEL> for DefaultLabelOrder {
    fn less_than(&self, _l1: &EdgeOrData<LABEL>, _l2: &EdgeOrData<LABEL>) -> bool {
        false // no shadowing by default
    }
}

/// Data equivalence relation.
///
/// Defines equivalence classes of declarations. Shadowing will only be applied with respect to
/// declarations in the same equivalence class. That is, the shadowing explained in [`LabelOrder`]
/// will only be applied if the declarations are equivalent.
pub trait DataEquiv<DATA> {
    fn data_equiv(&self, d1: &DATA, d2: &DATA) -> bool;
}

impl<DATA, T> DataEquiv<DATA> for T
where
    for<'sg> T: Fn(&'sg DATA, &'sg DATA) -> bool,
{
    fn data_equiv(&self, d1: &DATA, d2: &DATA) -> bool {
        self(d1, d2)
    }
}

#[derive(Default)]
pub struct DefaultDataEquiv {}

impl<DATA> DataEquiv<DATA> for DefaultDataEquiv {
    fn data_equiv(&self, _d1: &DATA, _d2: &DATA) -> bool {
        true // all data in same equivalence class by default
    }
}
