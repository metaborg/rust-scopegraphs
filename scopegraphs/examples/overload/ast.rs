#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
    pub main: Expr,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<Arg>,
    pub return_type: Option<Type>,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub name: String,
    pub type_ann: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    IntT,
    BoolT,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    IntLit(u64),
    BoolLit(bool),
    Ident(String),
    Plus(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    FunCall(String, Vec<Expr>),
    Ascribe(Box<Expr>, Type),
}
