use scopegraphs::{
    regex,
    regex::{Regex, RegexRef},
    Label,
};

macro_rules! assert_matches {
    ($expression:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {
        match $expression {
            $pattern $(if $guard)? => {}
            outcome => assert!(false, "expected {:?} to match {}", outcome, stringify!($pattern $(if $guard)?))
        }
    };
}

#[derive(Label, Copy, Clone, Eq, PartialEq, Debug)]
pub enum Alphabet {
    A,
    B,
    C,
}

#[test]
fn test_regex() {
    use Alphabet::*;

    assert_matches!(regex!(A), Regex::Symbol(A));
    assert_matches!(
        regex!(A | B),
        Regex::Or(
            RegexRef::Ref(&Regex::Symbol(A)),
            RegexRef::Ref(&Regex::Symbol(B))
        )
    );
    assert_matches!(
        regex!(A | B | C),
        Regex::Or(
            RegexRef::Ref(&Regex::Or(
                RegexRef::Ref(&Regex::Symbol(A)),
                RegexRef::Ref(&Regex::Symbol(B))
            )),
            RegexRef::Ref(&Regex::Symbol(C))
        )
    );
    assert_matches!(
        regex!(A & B),
        Regex::And(
            RegexRef::Ref(&Regex::Symbol(A)),
            RegexRef::Ref(&Regex::Symbol(B))
        )
    );
    assert_matches!(
        regex!(A* B*),
        Regex::Concat(
            RegexRef::Ref(&Regex::Repeat(RegexRef::Ref(&Regex::Symbol(A)))),
            RegexRef::Ref(&Regex::Repeat(RegexRef::Ref(&Regex::Symbol(B))))
        )
    );
    assert_matches!(
        regex!(~A ~B),
        Regex::Concat(
            RegexRef::Ref(&Regex::Complement(RegexRef::Ref(&Regex::Symbol(A)))),
            RegexRef::Ref(&Regex::Complement(RegexRef::Ref(&Regex::Symbol(B))))
        )
    );
}
