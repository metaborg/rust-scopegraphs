use crate::compile::AlphabetOrder;
use proc_macro2::TokenStream;
use std::collections::HashSet;
use std::fmt::Debug;
#[cfg(feature = "pretty-print")]
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;
use syn::Path;

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub(super) name: Path,
}

#[cfg(feature = "pretty-print")]
impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = &self.name;
        write!(f, "{}", quote::quote!(#name))
    }
}

impl From<&str> for Symbol {
    fn from(value: &str) -> Self {
        Self {
            name: syn::parse2(value.parse::<TokenStream>().unwrap()).unwrap(),
        }
    }
}

/// A regular expression over a scope graph
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Regex {
    /// `e`
    EmptyString,
    /// `0`
    EmptySet,
    /// A symbol like `a`
    Symbol(Rc<Symbol>),
    /// x*
    Repeat(Rc<Regex>),
    /// ~x
    Complement(Rc<Regex>),
    /// x | y
    Or(Rc<Regex>, Rc<Regex>),
    /// x & y
    And(Rc<Regex>, Rc<Regex>),
    /// x y
    Concat(Rc<Regex>, Rc<Regex>),
}

impl Regex {
    /// Applies a symbol to a regex. When a symbol is applied to a regex, a new regex
    /// is returned that matches everything after this symbol.
    /// Essentially this "strips" one symbol off of the regex, like from
    /// state regex(abab), if we see a symbol a, we go to state regex(bab)
    ///
    /// symbol can be None, to see what happens when a token outside of the alphabet
    /// is given to see what state we go to then. (TODO: I think??)
    pub fn apply_symbol(&self, symbol: Option<&Rc<Symbol>>) -> Rc<Regex> {
        match self {
            // a: 0 => 0
            Regex::EmptySet => Regex::EmptySet.into(),
            // a: e => e
            Regex::EmptyString => Regex::EmptySet.into(),
            // a: a => e
            // a: b => 0
            Regex::Symbol(s) => {
                if Some(s) == symbol {
                    Regex::EmptyString.into()
                } else {
                    Regex::EmptySet.into()
                }
            }
            // a: (ab)* => b(ab)*
            Regex::Repeat(r) => {
                Regex::Concat(r.apply_symbol(symbol), Regex::Repeat(r.clone()).into()).into()
            }
            // a: ~(ab) => ~(b)
            Regex::Complement(c) => Regex::Complement(c.apply_symbol(symbol)).into(),
            // a: (ab | ac) => b | c
            Regex::Or(l, r) => Regex::Or(l.apply_symbol(symbol), r.apply_symbol(symbol)).into(),
            // a: (ab & ac) => b & c
            Regex::And(l, r) => Regex::And(l.apply_symbol(symbol), r.apply_symbol(symbol)).into(),
            // a: ab => b
            Regex::Concat(l, r) => {
                let new = Regex::Concat(l.apply_symbol(symbol), r.clone()).into();

                if l.is_nullable() {
                    Regex::Or(new, r.apply_symbol(symbol)).into()
                } else {
                    new
                }
            }
        }
    }

    /// Normalizes a regex to a standard form.
    ///
    /// For example, `a e` is equivalent to `a`, and this transformation is made here.
    pub fn normalize(self: &Rc<Self>, ab: &AlphabetOrder) -> Rc<Regex> {
        match self.deref() {
            Regex::EmptyString => self.clone(),
            Regex::EmptySet => self.clone(),
            Regex::Symbol(_) => self.clone(),
            Regex::Repeat(r) => {
                let r = r.normalize(ab);
                match r.deref() {
                    // e* => 0
                    Regex::EmptySet => Regex::EmptyString.into(),
                    // 0* => 0
                    Regex::EmptyString => r,
                    // a** => a*
                    Regex::Repeat(_) => r,
                    _ => Regex::Repeat(r).into(),
                }
            }
            Regex::Complement(c) => {
                let c = c.normalize(ab);
                match c.deref() {
                    // ~~a => a
                    Regex::Complement(c) => c.clone(),
                    _ => Regex::Complement(c).into(),
                }
            }
            Regex::Or(l, r) => normalize_or(l, r, ab),
            Regex::And(l, r) => normalize_and(l, r, ab),
            Regex::Concat(l, r) => {
                let l = l.normalize(ab);
                let r = r.normalize(ab);

                match (l.deref(), r.deref()) {
                    // 0 a => 0
                    (Regex::EmptySet, _) => l,
                    // e a => e
                    (Regex::EmptyString, _) => r,
                    // (a b) c => a (b c)
                    (Regex::Concat(il, ir), _) => {
                        Regex::Concat(il.clone(), Regex::Concat(ir.clone(), r).into()).into()
                    }
                    // a 0 => 0
                    (_, Regex::EmptySet) => r,
                    // a e => a
                    (_, Regex::EmptyString) => l,
                    _ => Regex::Concat(l, r).into(),
                }
            }
        }
    }

    /// Returns whether this regex accepts the empty set or oblivion
    pub fn is_oblivion(&self) -> bool {
        matches!(self, Regex::EmptySet)
    }

    /// Returns whether this regex is nullable.
    ///
    /// That's either the empty string `e`, or some regex which has the empty string in the right place, like `e | a` is nullable because `e` is nullable.
    pub fn is_nullable(&self) -> bool {
        match self {
            Regex::EmptySet => false,
            Regex::EmptyString => true,
            Regex::Symbol(_) => false,
            Regex::Concat(l, r) => l.is_nullable() && r.is_nullable(),
            Regex::Repeat(_) => true,
            Regex::Or(l, r) => l.is_nullable() || r.is_nullable(),
            Regex::And(l, r) => l.is_nullable() && r.is_nullable(),
            Regex::Complement(i) => !i.is_nullable(),
        }
    }

    /// Searches for the alphabet used in this regular expression.
    ///
    /// Uses depth-first search to traverse the regex to get the alphabet in use.
    pub fn alphabet(&self) -> HashSet<Rc<Symbol>> {
        let mut alphabet = HashSet::new();
        self.search_alphabet(&mut alphabet);
        alphabet
    }

    fn search_alphabet(&self, alphabet: &mut HashSet<Rc<Symbol>>) {
        match self {
            Regex::EmptyString => {}
            Regex::EmptySet => {}
            Regex::Symbol(s) => {
                alphabet.insert(s.clone());
            }
            Regex::Repeat(i) | Regex::Complement(i) => i.search_alphabet(alphabet),
            Regex::Or(l, r) | Regex::And(l, r) | Regex::Concat(l, r) => {
                l.search_alphabet(alphabet);
                r.search_alphabet(alphabet);
            }
        }
    }

    fn order(&self, ab: &AlphabetOrder) -> i64 {
        match self {
            Regex::EmptyString => 1,
            Regex::EmptySet => 2,
            Regex::Repeat(_) => 3,
            Regex::Complement(_) => 4,
            Regex::Or(_, _) => 5,
            Regex::And(_, _) => 6,
            Regex::Concat(_, _) => 7,
            Regex::Symbol(s) => ab.get(s) + 8,
        }
    }

    fn compare(&self, other: &Regex, ab: &AlphabetOrder) -> i64 {
        match (self, other) {
            (Regex::Concat(l1, r1), Regex::Concat(l2, r2)) => {
                let ans = l1.compare(l2, ab);
                if ans == 0 {
                    r1.compare(r2, ab)
                } else {
                    ans
                }
            }
            (Regex::Repeat(l), Regex::Repeat(r)) => l.compare(r, ab),
            (Regex::Or(l1, r1), Regex::Or(l2, r2)) => {
                let ans = l1.compare(l2, ab);
                if ans == 0 {
                    r1.compare(r2, ab)
                } else {
                    ans
                }
            }
            (Regex::And(l1, r1), Regex::And(l2, r2)) => {
                let ans = l1.compare(l2, ab);
                if ans == 0 {
                    r1.compare(r2, ab)
                } else {
                    ans
                }
            }
            (Regex::Complement(l), Regex::Complement(r)) => l.compare(r, ab),
            _ => self.order(ab) - other.order(ab),
        }
    }
}

fn normalize_or(l: &Rc<Regex>, r: &Rc<Regex>, ab: &AlphabetOrder) -> Rc<Regex> {
    let l = l.normalize(ab);
    let r = r.normalize(ab);

    match (l.deref(), r.deref()) {
        // a | a => a
        (a, b) if a == b => l,
        // e | a => a
        (Regex::EmptySet, _) => r,
        // (a | b) | c => a | (b | c)
        (Regex::Or(il, ir), _) => normalize_or(il, &normalize_or(ir, &r, ab), ab),
        // ~e | a => ~e
        (Regex::Complement(c), _) if c.deref() == &Regex::EmptySet => l,
        // ?
        (_, Regex::Or(il, ir)) if l.compare(il, ab) > 0 => {
            normalize_or(il, &normalize_or(&l, ir, ab), ab)
        }
        // ?
        _ if l.compare(&r, ab) > 0 => normalize_or(&r, &l, ab),
        _ => Regex::Or(l, r).into(),
    }
}

fn normalize_and(l: &Rc<Regex>, r: &Rc<Regex>, ab: &AlphabetOrder) -> Rc<Regex> {
    let l = l.normalize(ab);
    let r = r.normalize(ab);

    match (l.deref(), r.deref()) {
        // a & a => a
        (a, b) if a == b => l,
        // e & a => e
        (Regex::EmptySet, _) => l,
        // (a & b) & c => a & (b & c)
        (Regex::And(il, ir), _) => normalize_and(il, &normalize_and(ir, &r, ab), ab),
        // ~e & a => a
        (Regex::Complement(c), _) if c.deref() == &Regex::EmptySet => r,
        // ?
        (_, Regex::And(il, ir)) if l.compare(ir, ab) > 0 => {
            normalize_and(il, &normalize_and(&l, ir, ab), ab)
        }
        // ?
        _ if l.compare(&r, ab) > 0 => normalize_and(&r, &l, ab),
        _ => Regex::And(l, r).into(),
    }
}

#[cfg(feature = "pretty-print")]
impl Display for Regex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Regex::EmptyString => write!(f, "e"),
            Regex::EmptySet => write!(f, "0"),
            Regex::Symbol(s) => write!(f, "{s}"),
            Regex::Repeat(r) => write!(f, "({r})*"),
            Regex::Complement(c) => write!(f, "~({c})"),
            Regex::Or(a, b) => write!(f, "{a} | {b}"),
            Regex::And(a, b) => write!(f, "{a} | {b}"),
            Regex::Concat(a, b) => write!(f, "{a} {b}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::compile::AlphabetOrder;
    use crate::parse_regex;
    use crate::regex::Symbol;
    use std::rc::Rc;

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

    #[test]
    fn apply_symbol() {
        let a = Rc::new(Symbol::from("a"));
        let b = Rc::new(Symbol::from("b"));
        let c = Rc::new(Symbol::from("c"));
        let ab = AlphabetOrder::new(&[a.clone(), b.clone(), c.clone()].into_iter().collect());

        assert_eq!(
            parse_regex("a b")
                .unwrap()
                .apply_symbol(Some(&a))
                .normalize(&ab),
            parse_regex("b").unwrap().into()
        );
        assert_eq!(
            parse_regex("a b")
                .unwrap()
                .apply_symbol(Some(&b))
                .normalize(&ab),
            parse_regex("0").unwrap().into()
        );
    }
}
