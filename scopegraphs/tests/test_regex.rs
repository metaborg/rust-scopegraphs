use scopegraphs_macros::compile_regex;
use scopegraphs_regular_expressions::RegexMatcher;

pub enum Alphabet {
    A,
    B,
    C,
}

#[test]
fn test_repeat() {
    use Alphabet::*;

    compile_regex!(type Machine<Alphabet> = A* B);

    assert!(Machine::new().accepts([B]));
    assert!(Machine::new().accepts([A, B]));
    assert!(Machine::new().accepts([A, A, A, B]));
    assert!(!Machine::new().accepts([B, B]));
    assert!(!Machine::new().accepts([B, A]));
    assert!(!Machine::new().accepts([A, B, A]));
    assert!(!Machine::new().accepts([]));
}

#[test]
fn test_concat() {
    use Alphabet::*;

    compile_regex!(type Machine<Alphabet> = A B);

    assert!(Machine::new().accepts([A, B]));
    assert!(!Machine::new().accepts([B, A]));
    assert!(!Machine::new().accepts([]));
    assert!(!Machine::new().accepts([A]));
    assert!(!Machine::new().accepts([A, B, C]));
}

#[test]
fn test_optional() {
    use Alphabet::*;

    compile_regex!(type Machine<Alphabet> = A B?);

    assert!(!Machine::new().accepts([]));
    assert!(Machine::new().accepts([A]));
    assert!(Machine::new().accepts([A, B]));
    assert!(!Machine::new().accepts([A, B, C]));
}

#[test]
fn test_one_or_more() {
    use Alphabet::*;

    compile_regex!(type Machine<Alphabet> = A+);

    assert!(!Machine::new().accepts([]));
    assert!(Machine::new().accepts([A]));
    assert!(Machine::new().accepts([A, A]));
    assert!(Machine::new().accepts([A, A, A, A]));
    assert!(!Machine::new().accepts([B]));
    assert!(!Machine::new().accepts([A, B]));
}

#[test]
fn test_or_2() {
    use Alphabet::*;

    compile_regex!(type Machine<Alphabet> = A | B);

    assert!(!Machine::new().accepts([]));
    assert!(Machine::new().accepts([A]));
    assert!(Machine::new().accepts([B]));
    assert!(!Machine::new().accepts([C]));
}

#[test]
fn test_or_3() {
    use Alphabet::*;

    compile_regex!(type Machine<Alphabet> = A | B | C);

    assert!(!Machine::new().accepts([]));
    assert!(Machine::new().accepts([A]));
    assert!(Machine::new().accepts([B]));
    assert!(Machine::new().accepts([C]));
}

#[test]
fn test_negate() {
    use Alphabet::*;

    compile_regex!(type Machine<Alphabet> = ~A);

    assert!(Machine::new().accepts([]));
    assert!(!Machine::new().accepts([A]));
    assert!(Machine::new().accepts([B]));
    assert!(Machine::new().accepts([C]));
}

#[test]
fn test_negate_or() {
    use Alphabet::*;

    compile_regex!(#[graph="negate_or.dot"] type Machine<Alphabet> = ~(A | B));

    assert!(Machine::new().accepts([]));
    assert!(!Machine::new().accepts([A]));
    assert!(!Machine::new().accepts([B]));
    assert!(Machine::new().accepts([C]));
}

#[test]
fn test_empty() {
    use Alphabet::*;

    compile_regex!(#[graph="empty.dot"] type Machine<Alphabet> = );

    assert!(Machine::new().accepts([]));
    assert!(Machine::new().accepts([A]));
    assert!(Machine::new().accepts([B]));
    assert!(Machine::new().accepts([C]));
    assert!(Machine::new().accepts([A, B, C]));
}
