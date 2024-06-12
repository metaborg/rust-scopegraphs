// {tutor name "records-example-mainrs"}
use crate::ast::{Expr, Program, RecordDef, Type};
use crate::queries::{resolve_lexical_ref, resolve_member_ref, resolve_record_ref};
use async_recursion::async_recursion;
use futures::future::{join, join_all};
use scopegraphs::completeness::FutureCompleteness;
use scopegraphs::render::{EdgeStyle, EdgeTo, RenderScopeData, RenderScopeLabel, RenderSettings};
use scopegraphs::{Scope, ScopeGraph, Storage};
use scopegraphs_macros::Label;
use smol::LocalExecutor;
use std::cell::RefCell;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::future::Future;
use std::rc::Rc;
use union_find::UnionFind;

mod ast;
mod parse;
mod queries;
mod union_find;

#[derive(Debug, Label, Copy, Clone, Hash, PartialEq, Eq)]
enum SgLabel {
    TypeDefinition,
    Definition,
    Lexical,
}

#[derive(Debug, Default, Hash, Eq, PartialEq, Clone)]
enum SgData {
    VarDecl {
        name: String,
        ty: PartialType,
    },
    TypeDecl {
        name: String,
        scope: Scope,
    },

    #[default]
    Nothing,
}

impl RenderScopeData for SgData {
    fn render_node(&self) -> Option<String> {
        match self {
            SgData::VarDecl { name, .. } | SgData::TypeDecl { name, .. } => Some(name.to_string()),
            SgData::Nothing => None,
        }
    }

    fn render_node_label(&self) -> Option<String> {
        match self {
            SgData::VarDecl { ty, .. } => {
                if matches!(ty, PartialType::Variable(_)) {
                    None
                } else {
                    Some(ty.to_string())
                }
            }
            SgData::TypeDecl { .. } => None,
            SgData::Nothing => None,
        }
    }

    fn extra_edges(&self) -> Vec<EdgeTo> {
        if let SgData::TypeDecl { scope, .. } = self {
            vec![EdgeTo {
                to: *scope,
                edge_style: EdgeStyle {},
                label_text: "fields".to_string(),
            }]
        } else {
            Vec::new()
        }
    }
}

impl RenderScopeLabel for SgLabel {
    fn render(&self) -> String {
        match self {
            SgLabel::TypeDefinition => "typ",
            SgLabel::Definition => "def",
            SgLabel::Lexical => "lex",
        }
        .to_string()
    }
}

impl SgData {
    pub fn expect_var_decl(&self) -> &PartialType {
        match self {
            SgData::VarDecl { ty, .. } => ty,
            _ => panic!("expected var decl, got {:?}", &self),
        }
    }

    pub fn expect_type_decl(&self) -> &Scope {
        match self {
            SgData::TypeDecl { scope: ty, .. } => ty,
            _ => panic!("expected type decl, got {:?}", &self),
        }
    }
}

/// A type variable, a placeholder for a type
#[derive(Clone, Debug, Default, Hash, Eq, PartialEq, Copy)]
pub struct TypeVar(usize);

/// A partial type can either still be a type variable, or a concrete type
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
enum PartialType {
    /// A variable
    Variable(TypeVar),
    /// A record named `name` with a scope.
    /// The scope contains the field of the record.
    /// See "scopes as types"
    Record { name: String, scope: Scope },
    /// A number type
    Int,
}

impl Display for PartialType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PartialType::Variable(v) => write!(f, "#{}", v.0),
            PartialType::Record { name, .. } => write!(f, "record {name}"),
            PartialType::Int => write!(f, "int"),
        }
    }
}

type RecordScopegraph<'sg> = ScopeGraph<'sg, SgLabel, SgData, FutureCompleteness<SgLabel>>;

struct TypeChecker<'sg, 'ex> {
    sg: RecordScopegraph<'sg>,
    uf: RefCell<UnionFind>,
    ex: LocalExecutor<'ex>,
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

    #[async_recursion(?Send)]
    async fn typecheck_expr<'a>(self: Rc<Self>, ast: &'ex Expr, scope: Scope) -> PartialType {
        match ast {
            Expr::StructInit { name, fields } => {
                let record_scope = resolve_record_ref(&self.sg, scope, name).await;

                // defer typechecking of all the fields..
                for (field_name, field_initializer) in fields {
                    self.spawn(|this| async move {
                        let (decl_type, init_type) = join(
                            resolve_member_ref(&this.sg, record_scope, field_name),
                            this.clone().typecheck_expr(field_initializer, scope),
                        )
                        .await;

                        this.uf.borrow_mut().union(decl_type, init_type);
                    });
                }

                // FIXME: field init exhaustiveness check omitted

                // .. but eagerly return the record type
                PartialType::Record {
                    name: name.clone(),
                    scope: record_scope,
                }
            }
            Expr::Add(l, r) => {
                // type check left-hand-side asynchronously
                self.spawn(|this| async move {
                    let l_ty = this.clone().typecheck_expr(l, scope).await;
                    this.uf.borrow_mut().union(l_ty, PartialType::Int);
                });
                // and type-check the right-hand-side asynchronously
                self.spawn(|this| async move {
                    let r_ty = this.clone().typecheck_expr(r, scope).await;
                    this.uf.borrow_mut().union(r_ty, PartialType::Int);
                });

                // ... but immediately return the current type
                PartialType::Int
            }
            Expr::Number(_) => PartialType::Int,
            Expr::Ident(var_name) => resolve_lexical_ref(&self.sg, scope, var_name).await,
            Expr::FieldAccess(inner, field) => {
                let res = self.clone().typecheck_expr(inner, scope).await;
                let inner_expr_type = self.uf.borrow_mut().find_partial_type(res);
                self.type_check_field_access(inner_expr_type, field).await
            }
            Expr::Let {
                name,
                value,
                in_expr,
            } => {
                let new_scope = self
                    .sg
                    .add_scope_default_with([SgLabel::Lexical, SgLabel::Definition]);
                self.sg
                    .add_edge(new_scope, SgLabel::Lexical, scope)
                    .expect("already closed");
                self.sg.close(new_scope, &SgLabel::Lexical);

                let ty_var = PartialType::Variable(self.uf.borrow_mut().fresh());
                self.sg
                    .add_decl(
                        new_scope,
                        SgLabel::Definition,
                        SgData::VarDecl {
                            name: name.clone(),
                            ty: ty_var.clone(),
                        },
                    )
                    .expect("already closed");

                self.sg.close(new_scope, &SgLabel::Definition);

                self.spawn(|this| async move {
                    let ty = this.clone().typecheck_expr(value, scope).await;
                    this.uf.borrow_mut().union(ty_var, ty);
                });

                // compute type of the result expression
                self.clone().typecheck_expr(in_expr, new_scope).await
            }
            Expr::LetRec { values, in_expr } => {
                let new_scope = self
                    .sg
                    .add_scope_default_with([SgLabel::Lexical, SgLabel::Definition]);
                self.sg
                    .add_edge(new_scope, SgLabel::Lexical, scope)
                    .expect("already closed");
                self.sg.close(new_scope, &SgLabel::Lexical);

                for (name, initializer_expr) in values {
                    let ty = PartialType::Variable(self.uf.borrow_mut().fresh());
                    self.sg
                        .add_decl(
                            new_scope,
                            SgLabel::Definition,
                            SgData::VarDecl {
                                name: name.clone(),
                                ty: ty.clone(),
                            },
                        )
                        .expect("already closed");

                    self.spawn(|this| async move {
                        let init_ty = this
                            .clone()
                            .typecheck_expr(initializer_expr, new_scope)
                            .await;
                        this.uf.borrow_mut().union(ty, init_ty)
                    });
                }
                self.sg.close(new_scope, &SgLabel::Definition);

                // compute type of the result expression
                self.typecheck_expr(in_expr, new_scope).await
            }
        }
    }

    async fn type_check_field_access(
        self: Rc<Self>,
        mut inner_expr_type: PartialType,
        field: &str,
    ) -> PartialType {
        loop {
            match inner_expr_type {
                PartialType::Variable(tv) => {
                    let fut = self.uf.borrow_mut().wait_for_unification(tv);
                    inner_expr_type = fut.await;
                }
                PartialType::Record { scope, .. } => {
                    break resolve_member_ref(&self.sg, scope, field).await
                }
                PartialType::Int => panic!("number has no field {field}"),
            }
        }
    }

    fn init_record_def(&self, record_def: &RecordDef, scope: Scope) -> Scope {
        let field_scope = self.sg.add_scope_default_with([SgLabel::Definition]);
        self.sg
            .add_decl(
                scope,
                SgLabel::TypeDefinition,
                SgData::TypeDecl {
                    name: record_def.name.clone(),
                    scope: field_scope,
                },
            )
            .expect("already closed");
        self.sg.close(scope, &SgLabel::Definition);

        field_scope
    }

    async fn typecheck_record_def(
        self: Rc<Self>,
        record_def: &RecordDef,
        scope: Scope,
        field_scope: Scope,
    ) {
        let fld_decl_futures = record_def.fields.iter().map(|(fld_name, fld_ty)| {
            let this = self.clone();
            async move {
                let ty = match fld_ty {
                    Type::StructRef(n) => {
                        let record_scope = resolve_record_ref(&this.sg, scope, n).await;
                        PartialType::Record {
                            name: n.clone(),
                            scope: record_scope,
                        }
                    }
                    Type::Int => PartialType::Int,
                };

                this.sg
                    .add_decl(
                        field_scope,
                        SgLabel::Definition,
                        SgData::VarDecl {
                            name: fld_name.clone(),
                            ty,
                        },
                    )
                    .expect("unexpected close");
            }
        });

        join_all(fld_decl_futures).await;
        self.sg.close(field_scope, &SgLabel::Definition);
    }
}

fn typecheck(ast: &Program) -> Option<Type> {
    let storage = Storage::new();
    let sg = RecordScopegraph::new(&storage, FutureCompleteness::default());
    let uf = RefCell::new(UnionFind::default());
    let local = LocalExecutor::new();

    let tc = Rc::new(TypeChecker { sg, uf, ex: local });

    let global_scope = tc.sg.add_scope_default_with([SgLabel::TypeDefinition]);

    // typecheck all the type definitions somewhere in the future
    for item in &ast.record_types {
        // synchronously init record decl
        let field_scope = tc.init_record_def(item, global_scope);
        tc.spawn(|this| this.typecheck_record_def(item, global_scope, field_scope));
    }

    // We can close for type definitions since the scopes for this are synchronously
    // made even before the future is returned and spawned. so, at this point,
    // no new type definitions are made.
    tc.sg.close(global_scope, &SgLabel::TypeDefinition);

    // typecheck the main expression
    let res = tc
        .ex
        .spawn(tc.clone().typecheck_expr(&ast.main, global_scope));

    // extract result from task
    let main_ty = smol::block_on(async {
        while !tc.ex.is_empty() {
            tc.ex.tick().await;
        }

        res.await
    });

    tc.sg
        .render_to(
            "sg.dot",
            RenderSettings::default().with_name("example with records"),
        )
        .unwrap();
    println!("{:?}", tc.uf.borrow());

    let resolved_main_ty = tc.uf.borrow_mut().type_of_partial_type(main_ty);
    resolved_main_ty
}

fn main() -> Result<(), Box<dyn Error>> {
    let example = parse::parse(
        "
    ",
    )
    .map_err(|i| i.to_string())?;

    println!("Type of example is: {:?}", typecheck(&example));

    Ok(())
}

#[cfg(test)]
mod tests {
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
    fn test_shadow() {
        test_example(
            "record A {} main = let a = new A {}; in let a = 42; in a;",
            ast::Type::Int,
        )
    }

    #[test]
    #[should_panic = "variable did not resolve uniquely: OnlyElementError::Multiple {..}"]
    fn test_letrec_shadow() {
        test_example(
            "record A {} main = letrec a = new A {}; a = 42; in a;",
            ast::Type::Int,
        )
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
}
// {tutor end name }
