use futures::future::{join, join_all};
use scopegraphs::completeness::FutureCompleteness;
use scopegraphs::resolve::Resolve;
use scopegraphs::{query_regex, Scope, ScopeGraph, Storage};
use scopegraphs_macros::{label_order, Label};
use smol::LocalExecutor;
use std::cell::RefCell;
use std::collections::HashMap;
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

impl RecordData {
    pub fn expect_var_decl(&self) -> &PartialType {
        match self {
            RecordData::VarDecl { ty, .. } => ty,
            _ => panic!("expected var decl, got {:?}", &self),
        }
    }

    pub fn expect_type_decl(&self) -> &Scope {
        match self {
            RecordData::TypeDecl { ty, .. } => ty,
            _ => panic!("expected type decl, got {:?}", &self),
        }
    }
}

#[derive(Clone, Debug, Default, Hash, Eq, PartialEq, Copy)]
pub struct TypeVar(usize);

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
enum PartialType {
    Variable(TypeVar),
    Struct { name: String, scope: Scope },
    Number,
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

    fn fresh(&mut self) -> TypeVar {
        let old = self.vars;
        self.vars += 1;

        TypeVar(old)
    }

    fn unify(&mut self, a: PartialType, b: PartialType) {
        let mut worklist = vec![(self.find_ty(a), self.find_ty(b))];

        // FIXME: worklist is unnecessary, as there are no composite types.
        // infrastructure is there for future extension
        while let Some((left, right)) = worklist.pop() {
            // if left variable
            if let PartialType::Variable(v_left) = left {
                // arbitrarily choose right as new representative
                // FIXME: use rank heuristic in case right is a variable?
                *self.get(v_left) = right;
            } else if let PartialType::Variable(_) = right {
                // left is a variable/number, but right is a variable
                worklist.push((right, left)) // will match first case in next iteration
            } else {
                if left != right {
                    panic!("Cannot unify {:?} and {:?}", left, right);
                }
            }
        }
    }

    fn find(&mut self, ty: TypeVar) -> PartialType {
        let res = self.get(ty);
        if let PartialType::Variable(v) = *res {
            if v == ty {
                return PartialType::Variable(ty);
            }

            let root = self.find(v);
            *self.get(v) = root.clone();
            root
        } else {
            res.clone()
        }
    }

    fn find_ty(&mut self, ty: PartialType) -> PartialType {
        if let PartialType::Variable(v) = ty {
            self.find(v)
        } else {
            ty
        }
    }

    fn get(&mut self, tv: TypeVar) -> &mut PartialType {
        let parent = &mut self.parent;
        for i in parent.len()..=tv.0 {
            parent.push(PartialType::Variable(TypeVar(i)));
        }

        &mut parent[tv.0]
    }

    fn type_of(&mut self, var: TypeVar) -> Option<Type> {
        match self.find(var) {
            PartialType::Variable(_) => None,
            PartialType::Struct { name, .. } => Some(Type::StructRef(name)),
            PartialType::Number => Some(Type::Int),
        }
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

#[async_recursion::async_recursion(?Send)]
async fn typecheck_expr<'sg>(
    ast: &Expr,
    scope: Scope,
    sg: &RecordScopegraph<'sg>,
    uf: &RefCell<UnionFind>,
) -> PartialType {
    match ast {
        Expr::StructInit { name, fields } => {
            let struct_scope = resolve_struct_ref(sg, scope, name).await;
            let fld_futures = fields.iter().map(|(fld_name, fld_init)| async {
                let (decl_type, init_type) = join(
                    resolve_member_ref(sg, struct_scope, fld_name),
                    typecheck_expr(fld_init, scope, sg, uf),
                )
                .await;

                uf.borrow_mut().unify(decl_type, init_type)
            });

            // FIXME: field init exhaustiveness check omitted
            join_all(fld_futures).await;
            // FIXME: can we make it 'return' the type before all field initializations are checked?
            PartialType::Struct {
                name: name.clone(),
                scope: struct_scope,
            }
        }
        Expr::Add(l, r) => {
            let (l, r) = Box::pin(join(
                typecheck_expr(l, scope, sg, uf),
                typecheck_expr(r, scope, sg, uf),
            ))
            .await;

            // assert equalities
            let mut _uf = uf.borrow_mut();
            _uf.unify(l, PartialType::Number);
            _uf.unify(r, PartialType::Number);

            PartialType::Number
        }
        Expr::Number(_) => PartialType::Number,
        Expr::Ident(var_name) => resolve_lexical_ref(sg, scope, var_name).await,
        Expr::FieldAccess(inner, field) => {
            let res = Box::pin(typecheck_expr(inner, scope, sg, uf)).await;
            let inner_expr_type = uf.borrow_mut().find_ty(res);
            match inner_expr_type {
                PartialType::Variable(_) => todo!("no delay mechanism yet"),
                PartialType::Struct { name, scope } => {
                    let env = sg
                        .query()
                        .with_path_wellformedness(query_regex!(RecordLabel: Lexical* Definition))
                        .with_data_wellformedness(|decl: &RecordData| match decl {
                            RecordData::VarDecl { name: var_name, .. } => &name == var_name,
                            _ => false,
                        })
                        .with_label_order(label_order!(RecordLabel: Definition < Lexical))
                        .resolve(scope)
                        .await;
                    env.get_only_item()
                        .expect("variable did not resolve uniquely")
                        .data()
                        .expect_var_decl()
                        .clone()
                }
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

            let ty_var = PartialType::Variable(uf.borrow_mut().fresh());
            sg.add_decl(
                new_scope,
                RecordLabel::Definition,
                RecordData::VarDecl {
                    name: name.clone(),
                    ty: ty_var.clone(),
                },
            )
            .expect("already closed");

            sg.close(new_scope, &RecordLabel::Definition);

            // compute type of the variable
            let ty_var_future = async {
                let ty = typecheck_expr(value, scope, sg, uf).await;
                uf.borrow_mut().unify(ty_var, ty);
            };

            // compute type of the result expression
            let ty_res_future = typecheck_expr(in_expr, new_scope, sg, uf);

            // run both computations concurrently
            //
            // this construct is set up in this way to ensure
            // the `unify(tv, ty_var)` can be executed before
            // the result type is computed.
            // this prevents deadlocks when the result type
            // is dependent on the value type, for example
            // ```
            // record A { x: int }
            // let r = A { x = 42 } in r.x
            // ```
            let (_, ty_res) = Box::pin(join(ty_var_future, ty_res_future)).await;

            // return
            ty_res
        }
    }
}

fn init_structdef<'sg>(struct_def: &StructDef, scope: Scope, sg: &RecordScopegraph<'sg>) -> Scope {
    let field_scope = sg.add_scope_default_with([RecordLabel::Definition]);
    // FIXME: use Decl
    let decl_scope = sg.add_scope(RecordData::TypeDecl {
        name: struct_def.name.clone(),
        ty: field_scope,
    });
    sg.add_edge(scope, RecordLabel::TypeDefinition, decl_scope)
        .expect("already closed");

    field_scope
}

async fn typecheck_structdef<'sg>(
    struct_def: &StructDef,
    scope: Scope,
    field_scope: Scope,
    sg: &RecordScopegraph<'sg>,
) {
    // NO AWAIT ABOVE THIS
    let fld_decl_futures = struct_def
        .fields
        .iter()
        .map(|(fld_name, fld_ty)| async move {
            let ty = match fld_ty {
                Type::StructRef(n) => {
                    let struct_scope = resolve_struct_ref(sg, scope, n).await;
                    PartialType::Struct {
                        name: n.clone(),
                        scope: struct_scope,
                    }
                }
                Type::Int => PartialType::Number,
            };

            sg.add_decl(
                field_scope,
                RecordLabel::Definition,
                RecordData::VarDecl {
                    name: fld_name.clone(),
                    ty,
                },
            )
            .expect("unexpected close");
        });

    join_all(fld_decl_futures).await;
}

type RecordScopegraph<'sg> =
    ScopeGraph<'sg, RecordLabel, RecordData, FutureCompleteness<RecordLabel>>;

fn typecheck(ast: &Ast) {
    let storage = Storage::new();
    let sg = RecordScopegraph::new(&storage, FutureCompleteness::default());
    let uf = RefCell::new(UnionFind::new());

    let global_scope = sg.add_scope_default_with([RecordLabel::TypeDefinition]);

    {
        let fut = async {
            let local = LocalExecutor::new();

            for item in &ast.items {
                // synchronously init record decl
                let field_scope = init_structdef(item, global_scope, &sg);
                local
                    .spawn(typecheck_structdef(item, global_scope, field_scope, &sg))
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

        let type_checker_result = smol::block_on(fut);
        type_checker_result

        // FIXME: return type of `main`
    }
}

async fn resolve_struct_ref(sg: &RecordScopegraph<'_>, scope: Scope, ref_name: &String) -> Scope {
    let env = sg
        .query()
        .with_path_wellformedness(query_regex!(RecordLabel: Lexical* TypeDefinition))
        .with_data_wellformedness(|record_data: &RecordData| match record_data {
            RecordData::TypeDecl {
                name: decl_name, ..
            } => decl_name == ref_name,
            _ => false,
        })
        .with_label_order(label_order!(RecordLabel: Definition < Lexical))
        .resolve(scope)
        .await;

    *env.get_only_item()
        .expect("record name did not resolve properly")
        .data()
        .expect_type_decl()
}

async fn resolve_lexical_ref(
    sg: &RecordScopegraph<'_>,
    scope: Scope,
    var_name: &String,
) -> PartialType {
    let env = sg
        .query()
        .with_path_wellformedness(query_regex!(RecordLabel: Lexical* Definition))
        .with_label_order(label_order!(RecordLabel: Definition < Lexical))
        .with_data_wellformedness(|record_data: &RecordData| -> bool {
            match record_data {
                RecordData::VarDecl { name, .. } if name == var_name => true,
                _ => false,
            }
        })
        .resolve(scope)
        .await;

    env.get_only_item()
        .expect("variable did not resolve uniquely")
        .data()
        .expect_var_decl()
        .clone()
}

async fn resolve_member_ref(
    sg: &RecordScopegraph<'_>,
    struct_scope: Scope,
    ref_name: &String,
) -> PartialType {
    let env = sg
        .query()
        .with_path_wellformedness(query_regex!(RecordLabel: Definition))
        .with_data_wellformedness(|record_data: &RecordData| match record_data {
            RecordData::VarDecl {
                name: decl_name, ..
            } => decl_name == ref_name,
            _ => false,
        })
        .resolve(struct_scope)
        .await;

    env.get_only_item()
        .expect("field name did not resolve properly")
        .data()
        .expect_var_decl()
        .clone()
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

    println!("{:?}", typecheck(&example));
}
