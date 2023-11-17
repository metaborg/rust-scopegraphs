pub enum RegexRef<'a, S> {
    Heap(Box<RegularExpression<'a, S>>),
    Static(&'a RegularExpression<'a, S>),
}

pub enum RegularExpression<'a, S> {
    Empty,
    Epsilon,
    Symbol(S),
    Concat(RegexRef<'a, S>, RegexRef<'a, S>),
    // TODO: rename
    Repeat(RegexRef<'a, S>),
    Or(RegexRef<'a, S>, RegexRef<'a, S>),
    And(RegexRef<'a, S>, RegexRef<'a, S>),
    Complement(RegexRef<'a, S>),
}
