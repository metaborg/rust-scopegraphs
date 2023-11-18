use scopegraphs_macros::regex;
use scopegraphs_regular_expressions::RegexMatcher;

pub enum Alphabet {
    A,
    B,
}

#[test]
fn test_regex() {
    use Alphabet::*;

    let mut res = regex!(Alphabet; A * B);

    assert!(!res.is_empty());
    assert!(!res.is_accepting());
    res.accept(A);
    assert!(!res.is_empty());
    assert!(!res.is_accepting());
    res.accept(B);
    assert!(!res.is_empty());
    assert!(res.is_accepting());
}
