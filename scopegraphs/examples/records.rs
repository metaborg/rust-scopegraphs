use futures::future::join;
use scopegraphs::completeness::FutureCompleteness;
use scopegraphs::resolve::Resolve;
use scopegraphs::{query_regex, Scope, ScopeGraph, Storage};
use scopegraphs_macros::{label_order, Label};
use smol::LocalExecutor;
use std::cell::RefCell;
use std::collections::HashMap;
use std::env::var;
use std::fmt::{Debug, Formatter};

#[derive(Debug, Label, Copy, Clone, Hash, PartialEq, Eq)]
enum RecordLabel {
    TypeDefinition,
    Definition,
    Lexical,
}

#[derive(Debug, Default, Hash, Eq, PartialEq)]
enum RecordData {
    VarDecl {
        name: String,
        ty: PartialType,
    },
    TypeDecl {
        name: String,
        ty: Scope,
    },

    #[default]
    Nothing,
}

enum Constraint {
    Equal(PartialType, PartialType),
}

#[derive(Clone, Debug, Default, Hash, Eq, PartialEq, Copy)]
struct TypeVar(usize);

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
enum PartialType {
    Variable(TypeVar),
    Struct { name: String, scope: Scope },
    Number,
}

impl PartialType {
    pub fn unify(&self, other: &Self) -> (PartialType, Vec<Constraint>) {
        todo!()
    }
}

#[derive(Clone)]
pub struct UnionFind {
    parent: Vec<PartialType>,
    vars: usize,
}

impl Debug for UnionFind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (idx, p) in self.parent.iter().enumerate() {
            write!(f, "{idx} -> {p:?}")?;
            if (idx + 1) < self.parent.len() {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

impl UnionFind {
    pub fn new() -> Self {
        Self {
            parent: vec![],
            vars: 0,
        }
    }

    pub fn fresh(&mut self) -> TypeVar {
        let old = self.vars;
        self.vars += 1;

        TypeVar(old)
    }

    pub fn union(&mut self, a: TypeVar, b: PartialType) -> Vec<Constraint> {
        let (a_tv, a_ty) = self.find(a);
        let (b_tv, b_ty) = self.find_ty(b);

        let (new_parent, new_constraints) = a_ty.unify(&b_ty);

        if let Some(b_tv) = b_tv {
            *self.get(a_tv) = new_parent.clone();
            *self.get(b_tv) = new_parent;
        } else {
            *self.get(a_tv) = new_parent;
        }

        new_constraints
    }

    pub fn find(&mut self, ty: TypeVar) -> (TypeVar, PartialType) {
        let res = self.get(ty);
        if let PartialType::Variable(v) = *res {
            if v == ty {
                return (v, PartialType::Variable(ty));
            }

            let root = self.find(v);
            *self.get(v) = root.1.clone();
            root
        } else {
            (ty, self.parent[ty.0].clone())
        }
    }

    pub fn find_ty(&mut self, ty: PartialType) -> (Option<TypeVar>, PartialType) {
        if let PartialType::Variable(v) = ty {
            let (a, b) = self.find(v);
            (Some(a), b)
        } else {
            (None, ty)
        }
    }

    fn get(&mut self, tv: TypeVar) -> &mut PartialType {
        let mut parent = &mut self.parent;
        for i in parent.len()..=tv.0 {
            parent.push(PartialType::Variable(TypeVar(i)));
        }

        &mut parent[tv.0]
    }

    pub fn type_of(&mut self, var: TypeVar) -> Option<Type> {
        todo!()
        // Some(match self.find(var).1 {
        //
        // })
    }
}

#[derive(Debug)]
enum Type {
    StructRef(String),
    Int,
}

#[derive(Debug)]
struct StructDef {
    name: String,
    fields: HashMap<String, Type>,
}

#[derive(Debug)]
enum Expr {
    StructInit {
        name: String,
        fields: HashMap<String, Expr>,
    },
    Add(Box<Expr>, Box<Expr>),
    Number(u64),
    Ident(String),
    FieldAccess(Box<Expr>, String),
    Let {
        name: String,
        value: Box<Expr>,
        in_expr: Box<Expr>,
    },
}

#[derive(Debug)]
struct Ast {
    items: Vec<StructDef>,
    main: Expr,
}

async fn typecheck_expr<'sg>(
    ast: &Expr,
    scope: Scope,
    sg: &RecordScopegraph<'sg>,
    uf: &RefCell<UnionFind>,
) -> PartialType {
    match ast {
        Expr::StructInit { .. } => {
            todo!()
        }
        Expr::Add(l, r) => {
            let (l, r) = Box::pin(join(
                typecheck_expr(l, scope, sg, uf),
                typecheck_expr(r, scope, sg, uf),
            ))
            .await;

            let lvar = uf.borrow_mut().fresh();
            let rvar = uf.borrow_mut().fresh();
            uf.borrow_mut().union(lvar, l);
            uf.borrow_mut().union(lvar, PartialType::Number);

            uf.borrow_mut().union(rvar, r);
            uf.borrow_mut().union(rvar, PartialType::Number);

            PartialType::Number
        }
        Expr::Number(_) => PartialType::Number,
        Expr::Ident(varname) => {
            let res = sg
                .query()
                .with_path_wellformedness(query_regex!(RecordLabel: Lexical* Definition))
                .with_label_order(label_order!(RecordLabel: Definition < Lexical))
                .with_data_wellformedness(|record_data: &RecordData| -> bool {
                    match record_data {
                        RecordData::VarDecl { name, .. } if name == varname => true,
                        _ => false,
                    }
                })
                .resolve(scope)
                .await;

            let mut r_iter = res.iter();
            let first = r_iter.next().expect("no query results");
            assert!(r_iter.next().is_none(), "multiple results");

            match first.data() {
                RecordData::VarDecl { ty, .. } => ty.clone(),
                RecordData::TypeDecl { .. } => panic!("varialbe name refers to type"),
                RecordData::Nothing => panic!("?"),
            }
        }
        Expr::FieldAccess(inner, field) => {
            let res = Box::pin(typecheck_expr(inner, scope, sg, uf)).await;
            match res {
                PartialType::Variable(_) => {}
                PartialType::Struct { .. } => {}
                PartialType::Number => panic!("number has no field {field}"),
            }
        }
        Expr::Let {
            name,
            value,
            in_expr,
        } => {
            let new_scope =
                sg.add_scope_default_with([RecordLabel::Lexical, RecordLabel::Definition]);
            sg.add_edge(new_scope, RecordLabel::Lexical, scope)
                .expect("already closed");
            sg.close(new_scope, &RecordLabel::Lexical);

            let tv = uf.borrow_mut().fresh();
            sg.add_decl(
                new_scope,
                RecordLabel::Definition,
                RecordData::VarDecl {
                    name: name.clone(),
                    ty: PartialType::Variable(tv),
                },
            )
            .expect("already closed");

            sg.close(new_scope, &RecordLabel::Definition);

            Box::pin(typecheck_expr(in_expr, new_scope, sg, uf)).await
        }
    }
}

async fn typecheck_structdef<'sg>(
    ast: &StructDef,
    scope: Scope,
    sg: &RecordScopegraph<'sg>,
    uf: &RefCell<UnionFind>,
) {
    let field_scope = sg.add_scope_default();
    let decl_scope = sg.add_scope(RecordData::TypeDecl {
        name: ast.name.clone(),
        ty: field_scope,
    });
    sg.add_edge(decl_scope, RecordLabel::TypeDefinition, scope)
        .expect("already closed");
    // NO AWAIT ABOVE THIS
}

type RecordScopegraph<'sg> =
    ScopeGraph<'sg, RecordLabel, RecordData, FutureCompleteness<RecordLabel>>;

fn typecheck(ast: &Ast) {
    let storage = Storage::new();
    let sg = RecordScopegraph::new(&storage, FutureCompleteness::default());
    let uf = RefCell::new(UnionFind::new());

    let global_scope = sg.add_scope_default();

    {
        let fut = async {
            let local = LocalExecutor::new();

            for item in &ast.items {
                local
                    .spawn(typecheck_structdef(item, global_scope, &sg, &uf))
                    .detach();
            }

            // We can close for type definitions since the scopes for this are synchronously made
            // even before the future is returned and spawned.
            sg.close(global_scope, &RecordLabel::TypeDefinition);

            local
                .spawn(typecheck_expr(&ast.main, global_scope, &sg, &uf))
                .detach();

            while !local.is_empty() {
                local.tick().await;
            }
        };

        // sg.close(global_scope)

        smol::block_on(fut);
    }
}

fn main() {
    let example = Ast {
        items: vec![StructDef {
            name: "A".to_string(),
            fields: {
                let mut m = HashMap::new();
                m.insert("y".to_string(), Type::Int);
                m
            },
        }],
        main: Expr::Let {
            name: "x".to_string(),
            value: Box::new(Expr::StructInit {
                name: "A".to_string(),
                fields: {
                    let mut m = HashMap::new();
                    m.insert(
                        "y".to_string(),
                        Expr::Add(Box::new(Expr::Number(4)), Box::new(Expr::Number(5))),
                    );
                    m
                },
            }),
            in_expr: Box::new(Expr::FieldAccess(
                Box::new(Expr::Ident("x".to_string())),
                "y".to_string(),
            )),
        },
    };

    println!("{:?}", example);
}
