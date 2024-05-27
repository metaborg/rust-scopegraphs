use crate::{ast, parse::parse, typecheck};

fn test_example(program: &str, expected_main_type: ast::Type) {
    let ast = parse(program).expect("parse failure");
    let ty = typecheck(&ast).expect("type not instantiated");
    assert_eq!(ty, expected_main_type)
}

#[test]
fn test_integer() {
    test_example("main = 42;", ast::Type::Int)
}

#[test]
fn test_letrec() {
    test_example("main = letrec a = 42; in a;", ast::Type::Int)
}

#[test]
fn test_let() {
    test_example("main = let a = 42; in a;", ast::Type::Int)
}

#[test]
fn test_complex() {
    test_example(
        "
record A {
    b: B,
    x: int,
}
record B {
    a: A,
    x: int,
}

main = letrec
    a = new A {x: 4, b: b};
    b = new B {x: 3, a: a};
in a.b.a.x;
        ",
        ast::Type::Int,
    )
}
