use proc_macro2::TokenStream;
use quote::quote;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use syn::Path;

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Symbol {
    pub(super) name: Path,
}

impl From<&str> for Symbol {
    fn from(value: &str) -> Self {
        Self {
            name: syn::parse2(value.parse::<TokenStream>().unwrap()).unwrap(),
        }
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = &self.name;
        write!(f, "{}", quote!(#name))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Regex {
    Empty,
    Epsilon,
    Symbol(Symbol),
    Repeat(Box<Regex>),
    Complement(Box<Regex>),
    Or(Box<Regex>, Box<Regex>),
    And(Box<Regex>, Box<Regex>),
    Concat(Box<Regex>, Box<Regex>),
}

impl Regex {
    pub fn is_nullable(&self) -> bool {
        match self {
            Regex::Empty => false,
            Regex::Epsilon => true,
            Regex::Symbol(_) => false,
            Regex::Concat(l, r) => l.is_nullable() && r.is_nullable(),
            Regex::Repeat(_) => true,
            Regex::Or(l, r) => l.is_nullable() || r.is_nullable(),
            Regex::And(l, r) => l.is_nullable() && r.is_nullable(),
            Regex::Complement(i) => !i.is_nullable(),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Regex::Empty)
    }

    pub fn alphabet(&self) -> HashSet<&Symbol> {
        let mut alphabet = HashSet::new();
        self.search_alphabet(&mut alphabet);
        alphabet
    }

    fn search_alphabet<'a>(&'a self, alphabet: &mut HashSet<&'a Symbol>) {
        match self {
            Regex::Empty => {}
            Regex::Epsilon => {}
            Regex::Symbol(s) => {
                alphabet.insert(s);
            }
            Regex::Repeat(i) | Regex::Complement(i) => i.search_alphabet(alphabet),
            Regex::Or(l, r) | Regex::And(l, r) | Regex::Concat(l, r) => {
                l.search_alphabet(alphabet);
                r.search_alphabet(alphabet);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse_regex;
    use quote::quote;

    #[test]
    fn nullable() {
        assert!(parse_regex("e").unwrap().is_nullable());
        assert!(!parse_regex("0").unwrap().is_nullable());

        assert!(parse_regex("e | e").unwrap().is_nullable());
        assert!(parse_regex("e | A").unwrap().is_nullable());
        assert!(parse_regex("A | e").unwrap().is_nullable());
        assert!(!parse_regex("A | B").unwrap().is_nullable());

        assert!(parse_regex("e | e").unwrap().is_nullable());
        assert!(!parse_regex("e & A").unwrap().is_nullable());
        assert!(!parse_regex("e & A").unwrap().is_nullable());
        assert!(!parse_regex("A & B").unwrap().is_nullable());

        assert!(parse_regex("e | e").unwrap().is_nullable());
        assert!(!parse_regex("A e").unwrap().is_nullable());
        assert!(!parse_regex("e B").unwrap().is_nullable());
        assert!(!parse_regex("A B").unwrap().is_nullable());

        assert!(parse_regex("A*").unwrap().is_nullable());
        assert!(!parse_regex("A+").unwrap().is_nullable());
        assert!(parse_regex("A?").unwrap().is_nullable());
    }
}
