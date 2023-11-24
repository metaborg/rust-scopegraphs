use crate::compile::AlphabetOrder;
use std::hash::Hash;
use std::collections::HashSet;
use std::fmt::Debug;
#[cfg(feature = "pretty-print")]
use std::fmt::Display;
use std::fmt::Formatter;
use std::ops::Deref;
use std::rc::Rc;

/// A regular expression over a scope graph
#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub enum Regex<L> {
    /// `e`
    EmptyString,
    /// `0`
    EmptySet,
    /// A symbol like `a`
    Symbol(Rc<L>),
    /// x*
    Repeat(Rc<Regex<L>>),
    /// ~x
    Complement(Rc<Regex<L>>),
    /// x | y
    Or(Rc<Regex<L>>, Rc<Regex<L>>),
    /// x & y
    And(Rc<Regex<L>>, Rc<Regex<L>>),
    /// x y
    Concat(Rc<Regex<L>>, Rc<Regex<L>>),
}

impl<L> Regex<L>
    where L : Hash + PartialEq + Eq
{
    /// Applies a symbol to a regex. When a symbol is applied to a regex, a new regex
    /// is returned that matches everything after this symbol.
    /// Essentially this "strips" one symbol off of the regex, like from
    /// state regex(abab), if we see a symbol a, we go to state regex(bab)
    ///
    /// symbol can be None, to see what happens when a token outside of the alphabet
    /// is given to see what state we go to then.
    ///
    /// This is called the Brzozowski derivative
    /// Janusz A. Brzozowski (1964). "Derivatives of Regular Expressions". J ACM. 11 (4): 481–494. doi:10.1145/321239.321249
    pub fn derive(&self, symbol: Option<&Rc<L>>) -> Rc<Regex<L>> {
        match self {
            // a: 0 => 0
            Regex::EmptySet => Regex::EmptySet.into(),
            // a: e => 0
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
                Regex::Concat(r.derive(symbol), Regex::Repeat(r.clone()).into()).into()
            }
            // a: ~(ab) => ~(b)
            Regex::Complement(c) => Regex::Complement(c.derive(symbol)).into(),
            // a: (ab | ac) => b | c
            Regex::Or(l, r) => Regex::Or(l.derive(symbol), r.derive(symbol)).into(),
            // a: (ab & ac) => b & c
            Regex::And(l, r) => Regex::And(l.derive(symbol), r.derive(symbol)).into(),
            // a: ab => b
            Regex::Concat(l, r) => {
                let new = Regex::Concat(l.derive(symbol), r.clone()).into();

                if l.is_nullable() {
                    Regex::Or(new, r.derive(symbol)).into()
                } else {
                    new
                }
            }
        }
    }

    /// Normalizes a regex to a standard form.
    ///
    /// For example, `a e` is equivalent to `a`, and this transformation is made here.
    pub fn normalize(self: &Rc<Self>, ab: &AlphabetOrder<L>) -> Rc<Self> {
        match self.deref() {
            Regex::EmptyString => self.clone(),
            Regex::EmptySet => self.clone(),
            Regex::Symbol(_) => self.clone(),
            Regex::Repeat(r) => {
                let r = r.normalize(ab);
                match r.deref() {
                    // 0* => e
                    Regex::EmptySet => Regex::EmptyString.into(),
                    // e* => e
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

    /// Returns whether this regex is nullable (i.e., accepts the empty string `e`).
    ///
    /// Examples of this include the `e` itself, any 0-ary repeat `A*`, `e | A`, etc.
    ///
    /// ```
    /// assert!(Regex::EmptyString.is_nullable())
    /// ```
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
    pub fn alphabet(&self) -> HashSet<Rc<L>> {
        let mut alphabet = HashSet::new();
        self.search_alphabet(&mut alphabet);
        alphabet
    }

    fn search_alphabet(&self, alphabet: &mut HashSet<Rc<L>>) {
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

    fn order(&self, ab: &AlphabetOrder<L>) -> i64 {
        // ordering of these variants is the same as the one in the java implementation of spoofax
        // at https://github.com/metaborg/nabl/blob/802559782da2216b66d290f90179c2ac8f21ba3f/scopegraph/src/main/java/mb/scopegraph/regexp/impl/RegExpNormalizingBuilder.java#L164.
        // The exact order is likely not necessary for correctness, and it's unclear if it's a good
        // order for speed, but we decided to use the same order regardless, just to be sure.
        match self {
            Regex::EmptySet => 1,
            Regex::EmptyString => 2,
            Regex::Concat(_, _) => 3,
            Regex::Repeat(_) => 4,
            Regex::Or(_, _) => 5,
            Regex::And(_, _) => 6,
            Regex::Complement(_) => 7,
            Regex::Symbol(s) => ab.get(s) + 8,
        }
    }

    fn compare(&self, other: &Regex<L>, ab: &AlphabetOrder<L>) -> i64 {
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

impl<'a, L: 'a> Regex<L> {
    pub fn convert<T: From<&'a L>>(&self) -> Regex<T> {
        match &self {
            Regex::EmptyString => Regex::EmptyString,
            Regex::EmptySet => Regex::EmptySet,
            Regex::Symbol(s) => Regex::Symbol(Rc::new(s.as_ref().into())),
            Regex::Repeat(re) => Regex::Repeat(Rc::new(re.convert())),
            Regex::Complement(re) => Regex::Complement(Rc::new(re.convert())),
            Regex::Or(l, r) => Regex::Or(
                Rc::new(l.convert()),
                Rc::new(r.convert())),
            Regex::And(l, r) => Regex::And(
                Rc::new(l.convert()),
                Rc::new(r.convert())),
            Regex::Concat(l, r) => Regex::Concat(
                Rc::new(l.convert()),
                Rc::new(r.convert())),
        }
    }
    pub fn try_convert<T: TryFrom<&'a L>>(&self) -> Result<Regex<T>, T::Error> {
        match &self {
            Regex::EmptyString => Ok(Regex::EmptyString),
            Regex::EmptySet => Ok(Regex::EmptySet),
            Regex::Symbol(s) => Ok(Regex::Symbol(Rc::new(s.as_ref().try_into()?))),
            Regex::Repeat(re) => Ok(Regex::Repeat(Rc::new(re.try_convert()?))),
            Regex::Complement(re) => Ok(Regex::Complement(Rc::new(re.try_convert()?))),
            Regex::Or(l, r) => Ok(Regex::Or(
                Rc::new(l.try_convert()?),
                Rc::new(r.try_convert()?))),
            Regex::And(l, r) => Ok(Regex::And(
                Rc::new(l.try_convert()?),
                Rc::new(r.try_convert()?))),
            Regex::Concat(l, r) => Ok(Regex::Concat(
                Rc::new(l.try_convert()?),
                Rc::new(r.try_convert()?))),
        }
    }
}

// FIXME: following gives invalid trait bound errors:
// impl<L, T> From<Regex<T>> for Regex<L>
//     where T: for<'a> From<&'a L>
// {
//     fn from(value: Regex<T>) -> Self {
//         value.convert()
//     }
// }

fn normalize_or<L: Hash + PartialEq + Eq>(l: &Rc<Regex<L>>, r: &Rc<Regex<L>>, ab: &AlphabetOrder<L>) -> Rc<Regex<L>> {
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

fn normalize_and<L: Hash + PartialEq + Eq>(l: &Rc<Regex<L>>, r: &Rc<Regex<L>>, ab: &AlphabetOrder<L>) -> Rc<Regex<L>> {
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
impl<L> Display for Regex<L> 
    where L : Display
{
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
        let a = Rc::new(String::from("a"));
        let b = Rc::new(String::from("b"));
        let c = Rc::new(String::from("c"));
        let ab = AlphabetOrder::new(&[a.clone(), b.clone(), c.clone()].into_iter().collect());

        assert_eq!(
            parse_regex("a b").unwrap().convert().derive(Some(&a)).normalize(&ab),
            parse_regex("b").unwrap().convert().into()
        );
        assert_eq!(
            parse_regex("a b").unwrap().convert().derive(Some(&b)).normalize(&ab),
            parse_regex("0").unwrap().convert().into()
        );
    }
}
