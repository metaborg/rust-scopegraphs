use crate::resolve::EdgeOrData;

/// Unary predicate over `DATA`.
///
/// Used to select declarations that a query can resolve to.
///
/// `O` is the output of applying the function to some data.
/// When executing the query, needs to be compatible with
/// the completeness strategy for the underlying scope graph.
pub trait DataWellformedness<'sg, DATA> {
    /// Type of the well-formedness result. Should be a wrapper around a boolean.
    type Output;
    /// returns true if the data is well-formed.
    fn data_wf(&self, data: &'sg DATA) -> Self::Output;
}

impl<'sg, DATA: 'sg, T, O: 'sg> DataWellformedness<'sg, DATA> for T
where
    T: Fn(&'sg DATA) -> O,
{
    type Output = O;

    fn data_wf(&self, data: &'sg DATA) -> O {
        self(data)
    }
}

/// Default Data wellformedness implementation
/// 
/// Matches all data by default.
#[derive(Default)]
pub struct DefaultDataWellformedness {}

impl<'sg, DATA> DataWellformedness<'sg, DATA> for DefaultDataWellformedness {
    type Output = bool;

    fn data_wf(&self, _data: &'sg DATA) -> bool {
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
