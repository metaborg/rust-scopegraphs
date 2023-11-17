pub mod matcher;

#[derive(Debug, Clone)]
pub enum RegexRef<'a, S> {
    Heap(Box<Regex<'a, S>>),
    Ref(&'a Regex<'a, S>),
}

impl<'a, S> From<&'a Regex<'a, S>> for RegexRef<'a, S> {
    fn from(value: &'a Regex<'a, S>) -> Self {
        Self::Ref(value)
    }
}

impl<'a, S> From<Box<Regex<'a, S>>> for RegexRef<'a, S> {
    fn from(value: Box<Regex<'a, S>>) -> Self {
        Self::Heap(value)
    }
}

impl<'a, S> From<Regex<'a, S>> for RegexRef<'a, S> {
    fn from(value: Regex<'a, S>) -> Self {
        Self::Heap(Box::new(value))
    }
}

#[derive(Debug, Clone)]
pub enum Regex<'a, S> {
    Empty,
    Epsilon,
    Symbol(S),
    Concat(RegexRef<'a, S>, RegexRef<'a, S>),
    Repeat(RegexRef<'a, S>),
    Or(RegexRef<'a, S>, RegexRef<'a, S>),
    And(RegexRef<'a, S>, RegexRef<'a, S>),
    Complement(RegexRef<'a, S>),
}
