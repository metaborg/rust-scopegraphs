use std::{cell::RefCell, future::Future, rc::Rc};

use async_recursion::async_recursion;
use scopegraphs::{
    add_scope,
    completeness::{FutureCompleteness, ScopeExtPerm},
    future_wrapper::FutureWrapper,
    label_order, query_regex,
    render::{RenderScopeData, RenderScopeLabel, RenderSettings},
    resolve::Resolve,
    Label, Scope, ScopeGraph, Storage,
};
use smol::LocalExecutor;

use crate::{union_find::UnionFind, Expr, Function, PartialType, Program, Type};

#[derive(Debug, Label, Copy, Clone, Hash, PartialEq, Eq)]
enum SgLabel {
    Fun,
    Var,
    Lex,
}

impl RenderScopeLabel for SgLabel {
    fn render(&self) -> String {
        match self {
            SgLabel::Fun => "FUN".into(),
            SgLabel::Var => "VAR".into(),
            SgLabel::Lex => "LEX".into(),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
struct FunType {
    args: Vec<PartialType>,
    return_type: PartialType,
}

#[derive(Debug, Default, Hash, Eq, PartialEq, Clone)]
enum SgData {
    VarDecl {
        name: String,
        ty: PartialType,
    },
    FunDecl {
        name: String,
        ty: FunType,
    },

    #[default]
    Nothing,
}

impl RenderScopeData for SgData {
    fn render_node(&self) -> Option<String> {
        match self {
            SgData::VarDecl { name, ty } => Some(format!("var {name}: {ty:?}")),
            SgData::FunDecl { name, ty } => {
                let args = &ty.args;
                let return_type = &ty.return_type;
                Some(format!("fun {name}({args:?}) : {return_type:?}"))
            }
            SgData::Nothing => None,
        }
    }
}

impl SgData {
    pub fn expect_var_decl(&self) -> &PartialType {
        match self {
            SgData::VarDecl { ty, .. } => ty,
            _ => panic!("expected var decl, got {:?}", &self),
        }
    }

    pub fn expect_fun_decl(&self) -> &FunType {
        match self {
            SgData::FunDecl { ty, .. } => ty,
            _ => panic!("expected type decl, got {:?}", &self),
        }
    }
}

type OverloadScopegraph<'sg> = ScopeGraph<'sg, SgLabel, SgData, FutureCompleteness<SgLabel>>;
type SgScopeExt<'sg> = ScopeExtPerm<'sg, SgLabel, SgData, FutureCompleteness<SgLabel>>;

struct TypeChecker<'sg, 'ex> {
    sg: &'sg OverloadScopegraph<'sg>,
    uf: RefCell<UnionFind>,
    ex: LocalExecutor<'ex>,
}

async fn resolve_var<'sg>(
    sg: &'sg OverloadScopegraph<'sg>,
    scope: Scope,
    ref_name: &str,
) -> PartialType {
    let ref_name: String = ref_name.into();
    let query = sg
        .query()
        .with_path_wellformedness(query_regex!(SgLabel: Lex* Var))
        .with_data_wellformedness(move |decl: &SgData| {
            let ref_name: String = ref_name.clone();
            let decl = decl.clone();
            FutureWrapper::new(async move {
                match decl {
                    SgData::VarDecl {
                        name: decl_name,
                        ty,
                    } => *decl_name == ref_name,
                    _ => false,
                };
                todo!()
            })
        })
        .with_label_order(label_order!(SgLabel: Var < Lex));

    let env = query.resolve(scope).await;
    // how to teach Rust that after the await, everything is released?

    let result = env
        .get_only_item()
        .expect("variable did not resolve properly")
        .data()
        .expect_var_decl()
        .clone();

    return result;
}

async fn resolve_fun<'sg>(
    sg: &'sg OverloadScopegraph<'sg>,
    scope: Scope,
    ref_name: &str,
    arg_types: Vec<PartialType>,  // used to select correct overload
    expected_return: PartialType, // used to select correct overload
) -> FunType {
    let ref_name: String = ref_name.into();
    let query = sg
        .query()
        .with_path_wellformedness(query_regex!(SgLabel: Lex* Fun))
        .with_data_wellformedness(move |decl: &SgData| {
            let ref_name: String = ref_name.clone();
            let arg_types = arg_types.clone();
            let decl = decl.clone();
            FutureWrapper::new(async move {
                let result: bool = match decl {
                    SgData::FunDecl {
                        name: decl_name,
                        ty,
                    } => *decl_name == ref_name && ty.args.len() == arg_types.len(),
                    _ => false,
                };
                result
            })
        })
        .with_data_equivalence(|d1: &SgData, d2: &SgData| {
            let d1_type = d1.expect_fun_decl();
            let d2_type = d2.expect_fun_decl();
            FutureWrapper::new(async move {
                // do all the proper scheduling
                // take care that incompatible types can occur, due to later refinements; those should have lowest priority
                false
            })
        })
        .with_label_order(label_order!(SgLabel: Var < Lex));

    let env = query.resolve(scope).await;
    // how to teach Rust that after the await, everything is released?

    let result = env
        .get_only_item()
        .expect("variable did not resolve properly")
        .data()
        .expect_fun_decl()
        .clone();

    return result;
}

impl<'sg, 'ex> TypeChecker<'sg, 'ex>
where
    'sg: 'ex,
{
    fn spawn<F, T>(self: &Rc<Self>, f: impl FnOnce(Rc<Self>) -> F)
    where
        F: Future<Output = T> + 'ex,
        T: 'ex,
    {
        self.ex.spawn(f(self.clone())).detach()
    }

    fn to_partial_type(self: &Rc<Self>, type_opt: Option<Type>) -> PartialType {
        type_opt
            .map(PartialType::Type)
            .unwrap_or_else(|| PartialType::Variable(self.uf.borrow_mut().fresh()))
    }

    async fn typecheck_fun(self: Rc<Self>, function: &Function, fun_ext: &SgScopeExt<'sg>) {
        let arg_vars = function
            .args
            .iter()
            .map(|arg| (arg.name.clone(), self.to_partial_type(arg.type_ann.clone())))
            .collect::<Vec<_>>();
        self.sg
            .ext_decl(
                fun_ext,
                SgData::FunDecl {
                    name: function.name.clone(),
                    ty: FunType {
                        args: arg_vars.iter().map(|(_, tp)| tp).cloned().collect(),
                        return_type: self.to_partial_type(function.return_type.clone()),
                    },
                },
            )
            .expect("why can this function even have an error??")
    }

    #[async_recursion(?Send)]
    async fn typecheck_expr(
        self: Rc<Self>,
        expr: &'ex Expr,
        scope: Scope,
        expected_type: PartialType,
    ) {
        match expr {
            Expr::IntLit(_) => self
                .uf
                .borrow_mut()
                .unify(expected_type, PartialType::Type(Type::IntT)),
            Expr::BoolLit(_) => self
                .uf
                .borrow_mut()
                .unify(expected_type, PartialType::Type(Type::BoolT)),
            Expr::Ident(name) => {
                let decl_type = resolve_var(self.sg, scope, &name).await;
                self.uf.borrow_mut().unify(expected_type, decl_type);
            }
            Expr::Plus(lhs, rhs) => {
                self.uf
                    .borrow_mut()
                    .unify(expected_type, PartialType::Type(Type::IntT));
                self.clone()
                    .typecheck_expr(lhs, scope, PartialType::Type(Type::IntT));
                self.typecheck_expr(rhs, scope, PartialType::Type(Type::IntT));
            }
            Expr::Lt(lhs, rhs) => {
                self.uf
                    .borrow_mut()
                    .unify(expected_type, PartialType::Type(Type::BoolT));
                self.clone()
                    .typecheck_expr(lhs, scope, PartialType::Type(Type::IntT));
                self.typecheck_expr(rhs, scope, PartialType::Type(Type::IntT));
            }
            Expr::IfThenElse(condition, if_branch, then_branch) => {
                self.clone()
                    .typecheck_expr(condition, scope, PartialType::Type(Type::BoolT));
                self.clone()
                    .typecheck_expr(if_branch, scope, expected_type.clone());
                self.typecheck_expr(then_branch, scope, expected_type);
            }
            Expr::FunCall(name, args) => {
                // we do not yet know which direction inference will flow, so create vars for all arguments
                let vars = args
                    .iter()
                    .map(|_| PartialType::Variable(self.uf.borrow_mut().fresh()))
                    .collect::<Vec<_>>();

                // type check all arguments
                args.iter().zip(vars.clone()).for_each(|(arg, ty)| {
                    self.spawn(|this| this.typecheck_expr(arg, scope, ty));
                });

                // spawn task that resolves function
                self.spawn(|this| {
                    FutureWrapper::new(async move {
                        // resolve function
                        let fun =
                            resolve_fun(this.sg, scope, &name, vars.clone(), expected_type.clone())
                                .await;

                        // check return type
                        this.uf.borrow_mut().unify(expected_type, fun.return_type);

                        // check argument types
                        fun.args.into_iter().zip(vars).for_each(|(formal, actual)| {
                            this.uf.borrow_mut().unify(formal, actual);
                        });
                    })
                });
            }
            Expr::Ascribe(expr, ty) => {
                let ty = PartialType::Type(ty.clone());
                self.uf.borrow_mut().unify(expected_type, ty.clone());
                self.clone().typecheck_expr(expr, scope, ty);
            }
        }
    }
}

fn typecheck(ast: &Program) -> Option<Type> {
    let storage = Storage::new();
    let sg = OverloadScopegraph::new(&storage, FutureCompleteness::default());
    let uf = RefCell::new(UnionFind::default());
    let local = LocalExecutor::new();

    let tc = Rc::new(TypeChecker {
        sg: &sg,
        uf,
        ex: local,
    });

    // INLINED add_scope!(...) macro for debugging purposes
    let (global_scope, ext_fun) = add_scope!(&tc.sg, [SgLabel::Fun]);

    // typecheck all the type definitions somewhere in the future
    for item in &ast.functions {
        // TODO: spawn task for type checking functions
    }

    // We can close for function definitions since the scopes for this are synchronously
    // made even before the future is returned and spawned. so, at this point,
    // no new type definitions are made.
    // FIXME: does this work when type-checking functions is spawned in a task?
    ext_fun.close(); // required, otherwise it will be dropped only after the type checking process is finished, which is too late

    // init variable for main type
    let main_ty = PartialType::Variable(tc.uf.borrow_mut().fresh());

    // typecheck the main expression
    let res = tc.ex.spawn(
        tc.clone()
            .typecheck_expr(&ast.main, global_scope, main_ty.clone()),
    );

    // extract result from task
    smol::block_on(async {
        while !tc.ex.is_empty() {
            tc.ex.tick().await;
        }

        res.await
    });

    tc.sg
        .render_to(
            "overload-sg.dot",
            RenderSettings::default().with_name("example with overloading"),
        )
        .unwrap();
    println!("Unifier: {:?}", tc.uf.borrow());

    let resolved_main_ty = tc.uf.borrow_mut().type_of_partial_type(main_ty);
    resolved_main_ty
}
