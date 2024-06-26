use crate::resolve::EdgeOrData;

/// Unary predicate over `DATA`.
///
/// Used to select declarations that a query can resolve to.
pub trait DataWellformedness<DATA> {
    /// returns true if the data is well-formed.
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

/// When you don't specify the data well-formedness, this is the default.
/// It considers all data well-formed.
#[derive(Default)]
pub struct DefaultDataWellformedness {}

impl<DATA> DataWellformedness<DATA> for DefaultDataWellformedness {
    fn data_wf(&self, _data: &DATA) -> bool {
        true // match all data by default
    }
}

/// Strict partial order on labels. Used to perform shadowing. Lower is more important.
///
/// Defaults to [`DefaultLabelOrder`],
/// and should be constructed using the [`label_order`](crate::label_order) macro.
///
/// For example, suppose that in some scope `s`,
/// declarations for some query are reachable via an `Lex` edge and an `Imp` edge
/// (for lexical parent and import, respectively).
///
/// When the label order indicates `Lex < Imp`
/// (i.e., declarations from a lexically enclosing scope have higher priority),
/// the declaration over the `Imp` edge is shadowed,
/// and will thus not be included in the environment.
///
/// If `Imp < Lex`, imports have higher priority, and that one will be included.
/// Otherwise, paths to both declarations are included in the environment.
pub trait LabelOrder<LABEL> {
    /// Defines the strict partial ordering. Lower is more important
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

/// Default label ordering: no label is more important than any other.
#[derive(Default)]
pub struct DefaultLabelOrder {}

impl<LABEL> LabelOrder<LABEL> for DefaultLabelOrder {
    fn less_than(&self, _l1: &EdgeOrData<LABEL>, _l2: &EdgeOrData<LABEL>) -> bool {
        false // no shadowing by default
    }
}

/// Data equivalence relation.
///
/// Defaults to [`DefaultDataEquivalence`].
///
/// Defines equivalence classes of declarations. Shadowing will only be applied with respect to
/// declarations in the same equivalence class. That is, the shadowing explained in [`LabelOrder`]
/// will only be applied if the declarations are equivalent.
pub trait DataEquivalence<DATA> {
    /// Returns true if d1 is equivalent to d2
    fn data_equiv(&self, d1: &DATA, d2: &DATA) -> bool;

    /// Returns if for this data equivalence, any data is always equivalent to any other data.
    fn always_equivalent(&self) -> bool {
        false
    }
}

impl<DATA, T> DataEquivalence<DATA> for T
where
    for<'sg> T: Fn(&'sg DATA, &'sg DATA) -> bool,
{
    fn data_equiv(&self, d1: &DATA, d2: &DATA) -> bool {
        self(d1, d2)
    }
}

/// Default data equivalence: data is always equivalent.
#[derive(Default)]
pub struct DefaultDataEquivalence {}

impl<DATA> DataEquivalence<DATA> for DefaultDataEquivalence {
    fn data_equiv(&self, _d1: &DATA, _d2: &DATA) -> bool {
        true // all data in same equivalence class by default
    }

    fn always_equivalent(&self) -> bool {
        true
    }
}
