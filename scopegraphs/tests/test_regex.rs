use scopegraphs::{regex, regex::Regex, Label};

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
        Regex::Or(&Regex::Symbol(A), &Regex::Symbol(B))
    );
    assert_matches!(
        regex!(A | B | C),
        Regex::Or(
            &Regex::Or(&Regex::Symbol(A), &Regex::Symbol(B)),
            &Regex::Symbol(C)
        )
    );
    assert_matches!(
        regex!(A & B),
        Regex::And(&Regex::Symbol(A), &Regex::Symbol(B))
    );
    assert_matches!(
        regex!(A* B*),
        Regex::Concat(
            &Regex::Repeat(&Regex::Symbol(A)),
            &Regex::Repeat(&Regex::Symbol(B))
        )
    );
    assert_matches!(
        regex!(~A ~B),
        Regex::Concat(
            &Regex::Complement(&Regex::Symbol(A)),
            &Regex::Complement(&Regex::Symbol(B))
        )
    );
}
