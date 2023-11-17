pub mod matcher;

#[derive(Debug, Clone)]
pub enum Regex<'a, S> {
    Empty,
    Epsilon,
    Symbol(S),
    Concat(&'a Regex<'a, S>, &'a Regex<'a, S>),
    Repeat(&'a Regex<'a, S>),
    Or(&'a Regex<'a, S>, &'a Regex<'a, S>),
    And(&'a Regex<'a, S>, &'a Regex<'a, S>),
    Complement(&'a Regex<'a, S>),
}
