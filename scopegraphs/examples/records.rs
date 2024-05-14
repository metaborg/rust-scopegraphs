use futures::future::{join, join_all};
use scopegraphs::completable_future::{CompletableFuture, CompletableFutureSignal};
use scopegraphs::completeness::FutureCompleteness;
use scopegraphs::resolve::Resolve;
use scopegraphs::{query_regex, Scope, ScopeGraph, Storage};
use scopegraphs_macros::{label_order, Label};
use smol::LocalExecutor;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::future::IntoFuture;
use std::rc::Rc;
use std::vec;

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

pub struct UnionFind {
    parent: Vec<PartialType>,
    vars: usize,
    callbacks: Vec<Vec<CompletableFutureSignal<PartialType>>>,
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
            callbacks: vec![],
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
                *self.get(v_left) = right.clone();
                for mut fut in std::mem::replace(&mut self.callbacks[v_left.0], vec![]) {
                    fut.complete(right.clone());
                }
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

    #[allow(unused)]
    fn type_of(&mut self, var: TypeVar) -> Option<Type> {
        match self.find(var) {
            PartialType::Variable(_) => None,
            PartialType::Struct { name, .. } => Some(Type::StructRef(name)),
            PartialType::Number => Some(Type::Int),
        }
    }

    fn callback(&mut self, tv: TypeVar) -> impl std::future::Future<Output = PartialType> {
        let future = CompletableFuture::<PartialType>::new();
        let callbacks = &mut self.callbacks;
        for _i in callbacks.len()..=tv.0 {
            callbacks.push(vec![]);
        }

        callbacks[tv.0].push(future.signal());
        future
    }
}

#[derive(Debug)]
enum Type {
    #[allow(unused)]
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

type RecordScopegraph<'sg> =
    ScopeGraph<'sg, RecordLabel, RecordData, FutureCompleteness<RecordLabel>>;

struct TypeChecker<'sg> {
    sg: RecordScopegraph<'sg>,
    uf: RefCell<UnionFind>,
    ex: LocalExecutor<'sg>, // make executor part of type checker to allow run-and-forget semantics of type checking subtasks (see run_detached() usage sites)
}

impl<'a, 'sg> TypeChecker<'a> {
    fn run_detached<T: 'a>(&self, fut: impl std::future::Future<Output = T> + 'a) {
        self.ex.spawn(fut).detach()
    }

    #[async_recursion::async_recursion(?Send)]
    async fn typecheck_expr<'ast: 'a>(tc: Rc<Self>, ast: &'ast Expr, scope: Scope) -> PartialType
    where
        'a: 'async_recursion,
    {
        let ty_res = match ast {
            Expr::StructInit { name, fields } => {
                let struct_scope = resolve_struct_ref(&tc.sg, scope, name).await;
                let fld_futures = fields.iter().map(|(fld_name, fld_init)| {
                    let tc = tc.clone();
                    async move {
                        let (decl_type, init_type) = join(
                            resolve_member_ref(&tc.clone().sg, struct_scope, fld_name),
                            Self::typecheck_expr(tc.clone(), fld_init, scope),
                        )
                        .await;

                        tc.with_unifier(|uf| uf.unify(decl_type, init_type));
                    }
                });

                // FIXME: field init exhaustiveness check omitted

                // asynchronously check field initializations
                for fut in fld_futures {
                    tc.ex.spawn(fut).detach()
                }
                // .. but eagerly return the struct type
                PartialType::Struct {
                    name: name.clone(),
                    scope: struct_scope,
                }
            }
            Expr::Add(l, r) => {
                // type check left-hand-side asynchronously
                let _scope = scope;
                let _tc = tc.clone();
                tc.clone().run_detached(async move {
                    let l_ty = Self::typecheck_expr(_tc.clone(), l, _scope).await;
                    _tc.with_unifier(|uf| uf.unify(l_ty, PartialType::Number));
                });
                // and type-check the right-hand-side asynchronously
                let _tc = tc.clone();
                tc.clone().run_detached(async move {
                    let r_ty = Self::typecheck_expr(_tc.clone(), r, scope).await;
                    _tc.with_unifier(|uf| uf.unify(r_ty, PartialType::Number));
                });

                // ... but immediately return the current type
                PartialType::Number
            }
            Expr::Number(_) => PartialType::Number,
            Expr::Ident(var_name) => resolve_lexical_ref(&tc.sg, scope, var_name).await,
            Expr::FieldAccess(inner, field) => {
                let res = Self::typecheck_expr(tc.clone(), inner, scope).await;
                let inner_expr_type = tc.with_unifier(|uf| uf.find_ty(res));
                Self::type_check_field_access(tc, inner_expr_type, field).await
            }
            Expr::Let {
                name,
                value,
                in_expr,
            } => {
                let new_scope = tc
                    .sg
                    .add_scope_default_with([RecordLabel::Lexical, RecordLabel::Definition]);
                tc.sg
                    .add_edge(new_scope, RecordLabel::Lexical, scope)
                    .expect("already closed");
                tc.sg.close(new_scope, &RecordLabel::Lexical);

                let ty_var = PartialType::Variable(tc.with_unifier(|uf| uf.fresh()));
                tc.sg
                    .add_decl(
                        new_scope,
                        RecordLabel::Definition,
                        RecordData::VarDecl {
                            name: name.clone(),
                            ty: ty_var.clone(),
                        },
                    )
                    .expect("already closed");

                tc.sg.close(new_scope, &RecordLabel::Definition);

                // compute type of the variable
                let _tc = tc.clone();
                let ty_var_future = async move {
                    let ty = Self::typecheck_expr(_tc.clone(), value, scope).await;
                    _tc.with_unifier(|uf| uf.unify(ty_var, ty))
                };

                // compute type of the result expression
                let ty_res_future = Self::typecheck_expr(tc.clone(), in_expr, new_scope);

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
                tc.run_detached(ty_var_future);

                // return
                ty_res_future.await
            }
        };
        ty_res
    }

    #[async_recursion::async_recursion(?Send)]
    async fn type_check_field_access(
        tc: Rc<TypeChecker<'a>>, 
        inner_expr_type: PartialType, 
        field: &String
    ) -> PartialType 
    where 'a: 'async_recursion
    {
        match inner_expr_type {
            PartialType::Variable(tv) => {
                let refined_type = tc.with_unifier(|uf| uf.callback(tv)).await;
                Self::type_check_field_access(tc, refined_type, field).await
            }
            PartialType::Struct { scope, .. } => {
                resolve_member_ref(&tc.sg, scope, field).await
            }
            PartialType::Number => panic!("number has no field {field}"),
        }
    }

    fn init_structdef(&self, struct_def: &StructDef, scope: Scope) -> Scope {
        let field_scope = self.sg.add_scope_default_with([RecordLabel::Definition]);
        self.sg
            .add_decl(scope, RecordLabel::TypeDefinition, RecordData::TypeDecl {
                name: struct_def.name.clone(),
                ty: field_scope,
            })
            .expect("already closed");
        self.sg.close(scope, &RecordLabel::Definition);

        field_scope
    }

    async fn typecheck_structdef(
        tc: Rc<TypeChecker<'a>>,
        struct_def: &StructDef,
        scope: Scope,
        field_scope: Scope,
    ) {
        let fld_decl_futures = struct_def.fields.iter().map(|(fld_name, fld_ty)| {
            let tc = tc.clone();
            async move {
                let ty = match fld_ty {
                    Type::StructRef(n) => {
                        let struct_scope = resolve_struct_ref(&tc.sg, scope, n).await;
                        PartialType::Struct {
                            name: n.clone(),
                            scope: struct_scope,
                        }
                    }
                    Type::Int => PartialType::Number,
                };

                tc.sg
                    .add_decl(
                        field_scope,
                        RecordLabel::Definition,
                        RecordData::VarDecl {
                            name: fld_name.clone(),
                            ty,
                        },
                    )
                    .expect("unexpected close");
            }
        });

        join_all(fld_decl_futures).await;
        tc.sg.close(field_scope, &RecordLabel::Definition);
    }

    fn with_unifier<T, F: FnOnce(&mut UnionFind) -> T>(&self, f: F) -> T {
        let mut uf = self.uf.borrow_mut();
        let res = f(&mut uf);
        drop(uf);
        res
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

fn typecheck(ast: &Ast) -> PartialType {
    let storage = Storage::new();
    let sg = RecordScopegraph::new(&storage, FutureCompleteness::default());
    let uf = RefCell::new(UnionFind::new());
    let local = LocalExecutor::new();

    let tc = Rc::new(TypeChecker {
        sg: sg,
        uf: uf,
        ex: local,
    });

    let _tc = tc.clone();

    let fut = async move {
        let global_scope = tc.sg.add_scope_default_with([RecordLabel::TypeDefinition]);

        for item in &ast.items {
            // synchronously init record decl
            let field_scope = tc.init_structdef(item, global_scope);
            tc.ex
                .spawn(TypeChecker::typecheck_structdef(
                    tc.clone(),
                    item,
                    global_scope,
                    field_scope,
                ))
                .detach();
        }

        // We can close for type definitions since the scopes for this are synchronously made
        // even before the future is returned and spawned.
        tc.sg.close(global_scope, &RecordLabel::TypeDefinition);

        let _tc = tc.clone();
        let main_task = tc.ex
            .spawn(TypeChecker::typecheck_expr(_tc.clone(), &ast.main, global_scope));

        while !tc.ex.is_empty() {
            tc.ex.tick().await;
        }

        // extract result from task
        main_task.into_future().await
    };

    let type_checker_result = smol::block_on(fut);
    type_checker_result
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

    println!("Type of example is: {:?}", typecheck(&example));
}
