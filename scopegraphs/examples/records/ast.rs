#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    StructRef(String),
    Int,
}

#[derive(Debug)]
pub struct RecordDef {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    StructInit {
        name: String,
        fields: Vec<(String, Expr)>,
    },
    #[allow(unused)]
    Add(Box<Expr>, Box<Expr>),
    Number(u64),
    Ident(String),
    FieldAccess(Box<Expr>, String),
    #[allow(unused)]
    Let {
        name: String,
        value: Box<Expr>,
        in_expr: Box<Expr>,
    },
    LetRec {
        values: Vec<(String, Expr)>,
        in_expr: Box<Expr>,
    },
}

#[derive(Debug)]
pub struct Program {
    /// Items can occur in any order. Like in Rust!
    pub record_types: Vec<RecordDef>,
    pub main: Expr,
}
