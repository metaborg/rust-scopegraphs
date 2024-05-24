use self::completable_future::{CompletableFuture, CompletableFutureSignal};
use async_recursion::async_recursion;
use futures::future::{join, join_all};
use scopegraphs::completeness::FutureCompleteness;
use scopegraphs::{Scope, ScopeGraph, Storage};
use scopegraphs_macros::{Label};
use smol::LocalExecutor;
use std::cell::RefCell;
use std::error::Error;
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::future::Future;
use std::rc::Rc;
use crate::ast::{Expr, Program, StructDef, Type};
use crate::resolve::{resolve_lexical_ref, resolve_member_ref, resolve_record_ref};
use scopegraphs::RenderScopeData;

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
    fn render(&self) -> Option<String> {
        match self {
            SgData::VarDecl { name, ty} => Some(format!("var {name}: {ty:?}")),
            SgData::TypeDecl { name, scope} => Some(format!("record {name} -> {scope:?}")),
            SgData::Nothing => None
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
    /// A struct named `name` with a scope.
    /// The scope contains the field of the struct.
    /// See "scopes as types"
    Record { name: String, scope: Scope },
    /// A number type
    Int,
}

#[derive(Default)]
pub struct UnionFind {
    /// Records the parent of each type variable.
    /// Kind of assumes type variables are assigned linearly.
    /// The "parent" of type variable 0 is stored at index 0
    parent: Vec<PartialType>,
    /// Keep track of type variables we've given out
    vars: usize,
    /// A vec of signals for each type variable.
    /// Whenever type variable 0 is unified with anything, we go through
    /// the list at index 0 and notify each.
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
    /// Create a new type variable
    /// (which happens to be one bigger than the previous fresh type variable)
    fn fresh(&mut self) -> TypeVar {
        let old = self.vars;
        self.vars += 1;

        TypeVar(old)
    }

    /// Unify two partial types, asserting they are equal to each other.
    ///
    /// If one of left or right is a concrete type, and the other is a type variable,
    /// we've essentially resolved what type the type variable is now, and we update the
    /// data structure to represent that. The next [`find`](Self::find) of this type variable
    /// will return the concrete type after this unification.
    ///
    /// Sometimes, two type variables are unified. In that case, one of the two is chosen by
    /// a fair (trust me) dice roll and is made the representative of both input type variables.
    /// Whenever one of the two is now unified with a concrete type, both input type variables
    /// become equal to that concrete type.
    fn unify(&mut self, left: PartialType, right: PartialType) {
        let left = self.find_partial_type(left);
        let right = self.find_partial_type(right);

        match (left, right) {
            (PartialType::Variable(left), right) | (right, PartialType::Variable(left)) => {
                // FIXME: use rank heuristic in case right is a variable?
                *self.get(left) = right.clone();
                if self.callbacks.len() > left.0 {
                    for fut in std::mem::take(&mut self.callbacks[left.0]) {
                        fut.complete(right.clone());
                    }
                }
            }
            (left, right) if left != right => {
                panic!("type error: cannot unify {left:?} and {right:?}");
            }
            _ => {}
        }
    }

    /// Find the representative for a given type variable.
    /// In the best case, this is a concrete type this type variable is equal to.
    /// That's nice, because now we know what that type variable was supposed to be.
    ///
    /// However, it's possible we find another type variable instead (wrapped in a [`PartialType`]).
    /// Now we know that this new type variable has the same type of the given type variable,
    /// we just don't know yet which type that is. More unifications are needed.
    fn find(&mut self, ty: TypeVar) -> PartialType {
        let res = self.get(ty);
        if let PartialType::Variable(v) = *res {
            if v == ty {
                return PartialType::Variable(ty);
            }

            // do path compression
            let root = self.find(v);
            *self.get(v) = root.clone();
            root
        } else {
            res.clone()
        }
    }

    /// [find](Self::find), but for a parial type
    fn find_partial_type(&mut self, ty: PartialType) -> PartialType {
        if let PartialType::Variable(v) = ty {
            self.find(v)
        } else {
            ty
        }
    }

    /// Get a mutable reference to parent of a given type variable.
    /// Used in the implementation of [`find`](Self::find) and [`union`](Self::union)
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
            PartialType::Record { name, .. } => Some(Type::StructRef(name)),
            PartialType::Int => Some(Type::Int),
        }
    }

    fn type_of_partial_type(&mut self, var: PartialType) -> Option<Type> {
        match self.find_partial_type(var) {
            PartialType::Variable(_) => None,
            PartialType::Record { name, .. } => Some(Type::StructRef(name)),
            PartialType::Int => Some(Type::Int),
        }
    }

    /// Wait for when tv is unified with something.
    fn wait_for_unification(&mut self, tv: TypeVar) -> impl Future<Output=PartialType> {
        let future = CompletableFuture::<PartialType>::new();
        let callbacks = &mut self.callbacks;
        for _ in callbacks.len()..=tv.0 {
            callbacks.push(vec![]);
        }

        callbacks[tv.0].push(future.signal());
        
        future
    }
}

mod ast {
    use std::collections::HashMap;

    #[derive(Debug, Clone)]
    pub enum Type {
        StructRef(String),
        Int,
    }

    #[derive(Debug)]
    pub struct StructDef {
        pub name: String,
        pub fields: HashMap<String, Type>,
    }

    #[derive(Debug, Clone)]
    pub enum Expr {
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
        LetRec {
            values: HashMap<String, Expr>,
            in_expr: Box<Expr>,
        },
    }

    #[derive(Debug)]
    pub struct Program {
        /// Items can occur in any order. Like in Rust!
        pub items: Vec<StructDef>,
        pub main: Expr,
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
                let struct_scope = resolve_record_ref(&self.sg, scope, name).await;

                // defer typechecking of all the fields..
                for (field_name, field_initializer) in fields {
                    self.spawn(|this| async move {
                        let (decl_type, init_type) = join(
                            resolve_member_ref(&this.sg, struct_scope, field_name),
                            this.clone().typecheck_expr(field_initializer, scope),
                        )
                        .await;

                        this.uf.borrow_mut().unify(decl_type, init_type);
                    });
                }

                // FIXME: field init exhaustiveness check omitted

                // .. but eagerly return the struct type
                PartialType::Record {
                    name: name.clone(),
                    scope: struct_scope,
                }
            }
            Expr::Add(l, r) => {
                // type check left-hand-side asynchronously
                self.spawn(|this| async move {
                    let l_ty = this.clone().typecheck_expr(l, scope).await;
                    this.uf.borrow_mut().unify(l_ty, PartialType::Int);
                });
                // and type-check the right-hand-side asynchronously
                self.spawn(|this| async move {
                    let r_ty = this.clone().typecheck_expr(r, scope).await;
                    this.uf.borrow_mut().unify(r_ty, PartialType::Int);
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
                    this.uf.borrow_mut().unify(ty_var, ty);
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
                        this.uf.borrow_mut().unify(ty, init_ty)
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

    fn init_struct_def(&self, struct_def: &StructDef, scope: Scope) -> Scope {
        let field_scope = self.sg.add_scope_default_with([SgLabel::Definition]);
        self.sg
            .add_decl(
                scope,
                SgLabel::TypeDefinition,
                SgData::TypeDecl {
                    name: struct_def.name.clone(),
                    scope: field_scope,
                },
            )
            .expect("already closed");
        self.sg.close(scope, &SgLabel::Definition);

        field_scope
    }

    async fn typecheck_struct_def(
        self: Rc<Self>,
        struct_def: &StructDef,
        scope: Scope,
        field_scope: Scope,
    ) {
        let fld_decl_futures = struct_def.fields.iter().map(|(fld_name, fld_ty)| {
            let this = self.clone();
            async move {
                let ty = match fld_ty {
                    Type::StructRef(n) => {
                        let struct_scope = resolve_record_ref(&this.sg, scope, n).await;
                        PartialType::Record {
                            name: n.clone(),
                            scope: struct_scope,
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

mod resolve {
    use scopegraphs::{query_regex, Scope};
    use scopegraphs::resolve::Resolve;
    use scopegraphs_macros::label_order;
    use crate::{PartialType, RecordScopegraph, SgData, SgLabel};

    pub async fn resolve_record_ref(sg: &RecordScopegraph<'_>, scope: Scope, ref_name: &str) -> Scope {
        let env = sg
            .query()
            .with_path_wellformedness(query_regex!(SgLabel: Lexical* TypeDefinition))
            .with_data_wellformedness(|record_data: &SgData| match record_data {
                SgData::TypeDecl {
                    name: decl_name, ..
                } => decl_name == ref_name,
                _ => false,
            })
            .with_label_order(label_order!(SgLabel: Definition < Lexical))
            .resolve(scope)
            .await;

        *env.get_only_item()
            .expect("record name did not resolve properly")
            .data()
            .expect_type_decl()
    }

    pub async fn resolve_lexical_ref(
        sg: &RecordScopegraph<'_>,
        scope: Scope,
        var_name: &str,
    ) -> PartialType {
        let env = sg
            .query()
            .with_path_wellformedness(query_regex!(SgLabel: Lexical* Definition))
            .with_label_order(label_order!(SgLabel: Lexical < Definition))
            .with_data_wellformedness(|record_data: &SgData| -> bool {
                matches!(record_data, SgData::VarDecl { name, .. } if name == var_name)
            })
            .resolve(scope)
            .await;

        env.get_only_item()
            .expect("variable did not resolve uniquely")
            .data()
            .expect_var_decl()
            .clone()
    }

    pub async fn resolve_member_ref(
        sg: &RecordScopegraph<'_>,
        struct_scope: Scope,
        ref_name: &str,
    ) -> PartialType {
        let env = sg
            .query()
            .with_path_wellformedness(query_regex!(SgLabel: Definition))
            .with_data_wellformedness(|record_data: &SgData| match record_data {
                SgData::VarDecl {
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
}

fn typecheck(ast: &Program) -> Option<Type> {
    let storage = Storage::new();
    let sg = RecordScopegraph::new(&storage, FutureCompleteness::default());
    let uf = RefCell::new(UnionFind::default());
    let local = LocalExecutor::new();

    let tc = Rc::new(TypeChecker { sg, uf, ex: local });

    let global_scope = tc.sg.add_scope_default_with([SgLabel::TypeDefinition]);

    // typecheck all the type definitions somewhere in the future
    for item in &ast.items {
        // synchronously init record decl
        let field_scope = tc.init_struct_def(item, global_scope);
        tc.spawn(|this| this.typecheck_struct_def(item, global_scope, field_scope));
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

    tc.sg.render(&mut File::create("sg.dot").unwrap(), "sg").unwrap();
    println!("{:?}", tc.uf.borrow());

    let resolved_main_ty = tc.uf.borrow_mut().type_of_partial_type(main_ty);
    resolved_main_ty
}

mod parse {
    use std::collections::HashMap;
    use winnow::ascii::multispace0;
    use winnow::combinator::{alt, delimited, opt, preceded, repeat, separated, terminated};
    use winnow::error::{ParserError, StrContext};

    use winnow::prelude::*;
    use winnow::seq;
    use winnow::stream::AsChar;
    use winnow::token::{one_of, take_while};
    use crate::ast::{Program, Expr, StructDef, Type};

    fn ws<'a, F, O, E: ParserError<&'a str>>(inner: F) -> impl Parser<&'a str, O, E>
        where
            F: Parser<&'a str, O, E>,
    {
        delimited(
            multispace0,
            inner,
            multispace0
        )
    }

    fn parse_ident(input: &mut &'_ str) -> PResult<String> {
        ws((
            one_of(|c: char| c.is_alpha() || c == '_'),
            take_while(0.., |c: char| c.is_alphanum() || c == '_'),
        )
            .recognize()
            .verify(|i: &str| {
                i != "in" && i != "new" && i != "letrec" && i != "record"
            })
        )
            .parse_next(input)
            .map(|i| i.to_string())
    }

    fn parse_int(input: &mut &'_ str) -> PResult<u64> {
        repeat(
            1..,
            terminated(one_of('0'..='9'), repeat(0.., '_').map(|()| ())),
        )
            .map(|()| ())
            .recognize()
            .parse_next(input)
            .map(|i| i.parse().expect("not an integer"))
    }

    fn parse_type(input: &mut &'_ str) -> PResult<Type> {
        ws(alt((
            "int".value(Type::Int),
            parse_ident.map(Type::StructRef),
        ))).parse_next(input)
    }

    fn parse_field_def(input: &mut &'_ str) -> PResult<(String, Type)> {
        seq!(
        _: multispace0,
        parse_ident,
        _: ws(":"),
        parse_type,
        _: multispace0,
    ).parse_next(input)
    }

    fn parse_field_defs(input: &mut &'_ str) -> PResult<HashMap<String, Type>> {
        terminated(separated(0.., ws(parse_field_def), ws(",")), opt(ws(","))).parse_next(input)
    }

    fn parse_field(input: &mut &'_ str) -> PResult<(String, Expr)> {
        seq!(
        _: multispace0,
        parse_ident,
        _: ws(":"),
        parse_expr,
        _: multispace0,
    ).parse_next(input)
    }

    fn parse_fields(input: &mut &'_ str) -> PResult<HashMap<String, Expr>> {
        terminated(separated(0.., ws(parse_field), ws(",")), opt(ws(","))).parse_next(input)
    }

    fn parse_item(input: &mut &'_ str) -> PResult<StructDef> {
        seq! {StructDef {
        name: parse_ident,
        // `_` fields are ignored when building the struct
        _: ws("{"),
        fields: parse_field_defs,
        _: ws("}"),
    }}.parse_next(input)
    }

    fn parse_value(input: &mut &'_ str) -> PResult<(String, Expr)> {
        seq!(
        parse_ident,
        _: ws("="),
        parse_expr,
        _: ws(";")
    ).parse_next(input)
    }

    fn parse_values(input: &mut &'_ str) -> PResult<HashMap<String, Expr>> {
        repeat(0.., parse_value).parse_next(input)
    }

    fn parse_basic_expr(input: &mut &'_ str) -> PResult<Expr> {
        alt((
            parse_int.map(Expr::Number),
            parse_ident.map(Expr::Ident),
            seq! {
            _: ws("new"),
            parse_ident,
            // `_` fields are ignored when building the struct
            _: ws("{"),
            parse_fields,
            _: ws("}"),
        }.map(|(name, fields)| Expr::StructInit { name, fields }),
            seq! {
            _: ws("letrec"),
            parse_values,
            _: ws("in"),
            parse_expr,
        }.map(|(values, in_expr)| Expr::LetRec { values, in_expr: Box::new(in_expr) }),
            seq! {
            _: ws("("),
            parse_expr,
            _: ws(")"),
        }.map(|(i, )| i),
        )).context(StrContext::Label("parse expr"))
            .parse_next(input)
    }

    fn parse_expr(input: &mut &'_ str) -> PResult<Expr> {
        let first = ws(parse_basic_expr).parse_next(input)?;
        let mut res = repeat(0.., (ws("."), parse_ident).map(|(_, i)| i)).fold(|| first.clone(), |acc, val| {
            Expr::FieldAccess(Box::new(acc), val)
        });

        res.parse_next(input)
    }

    enum ItemOrExpr {
        Item(StructDef),
        Expr(Expr),
    }

    pub(crate) fn parse(mut input: &str) -> PResult<Program> {
        let mut items = Vec::new();
        let mut main = None;

        while !input.is_empty() {
            match ws(
                alt((
                    ws(preceded(ws("record"), parse_item.map(ItemOrExpr::Item))),
                    seq!(
                    _: ws("main"),
                    _: ws("="),
                    ws(parse_expr.map(ItemOrExpr::Expr)),
                    _: ws(";"),
                ).map(|(i, )| i),
                )).context(StrContext::Label("parse item")),
            )
                .parse_next(&mut input)?
            {
                ItemOrExpr::Expr(e) => main = Some(e),
                ItemOrExpr::Item(i) => items.push(i),
            }
        }

        Ok(Program {
            items,
            main: main.expect("no main"),
        })
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let example = parse::parse(
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
    )
    .map_err(|i| i.to_string())?;

    println!("Type of example is: {:?}", typecheck(&example));

    Ok(())
}

#[allow(unused)]
mod completable_future {
    //! Copied and adapted from https://crates.io/crates/completable_future (due to dependency mismatch)
    //!
    //! # Completable Future
    //!
    //! Similar to Java's CompletableFuture, this crate provides a simple
    //! future that can be completed and properly notified from elsewhere other
    //! than the executor of the future. It is sutable for some blocking
    //! tasks that could block the executor if we use a future directly in
    //! an executor.
    //!
    //! A CompletableFuture is still a future and has all the combinators that
    //! you can use to chain logic working on the result or the error. Also,
    //! unlike Java and inherited from Rust's poll model future, some executor
    //! needs to execute the CompletableFuture in order to get the result; the
    //! thread or code that completes (or errors) the future will not execute
    //! the logic chained after the future.
    //!
    //! The CompletableFuture uses Arc and Mutex to synchronize poll and completion,
    //! so there's overhead for using it.
    //!
    //! # Example
    //! ```
    //! extern crate futures;
    //! extern crate completable_future;
    //!
    //! use futures::prelude::*;
    //! use futures::executor::block_on;
    //! use std::thread::spawn;
    //! use std::thread::sleep;
    //! use std::time::Duration;
    //! use completable_future::CompletableFuture;
    //!
    //! fn main() {
    //!     let fut1 = CompletableFuture::<String, ()>::new();
    //!     // we will give the signal to some worker for it to complete
    //!     let mut signal = fut1.signal();
    //!     let fut2 = fut1.and_then(|s| {
    //!         // this will come from whoever completes the future
    //!         println!("in fut2: {}", s);
    //!         Ok("this comes from fut2".to_string())
    //!     });
    //!     
    //!     let j = spawn(move || {
    //!         println!("waiter thread: I'm going to block on fut2");
    //!         let ret = block_on(fut2).unwrap();
    //!         println!("waiter thread: fut2 completed with message -- {}", ret);
    //!     });
    //!     
    //!     spawn(move || {
    //!         println!("worker thread: going to block for 1000 ms");
    //!         sleep(Duration::from_millis(1000));
    //!         signal.complete("this comes from fut1".to_string());
    //!         println!("worker thread: completed fut1");
    //!     });
    //!     
    //!     j.join().unwrap();
    //! }
    //! ```

    use futures::future::Future;
    use futures::task::{AtomicWaker, Context, Waker};
    use std::mem;
    use std::pin::Pin;
    use std::sync::{Arc, Mutex};
    use std::task::Poll;

    enum WakerWrapper {
        Registered(AtomicWaker),
        NotRegistered,
    }

    impl WakerWrapper {
        fn register(&mut self, waker: &Waker) {
            match self {
                WakerWrapper::Registered(_dont_care) => (),
                WakerWrapper::NotRegistered => {
                    let w = AtomicWaker::new();
                    w.register(waker);
                    *self = WakerWrapper::Registered(w)
                }
            }
        }

        fn wake(&self) {
            match *self {
                WakerWrapper::Registered(ref w) => w.wake(),
                WakerWrapper::NotRegistered => (),
            };
        }
    }

    enum FutureState<V> {
        Pending,
        Completed(V),
        Taken,
    }

    impl<V> FutureState<V> {
        fn swap(&mut self, new_val: FutureState<V>) -> FutureState<V> {
            mem::replace(self, new_val)
        }

        fn unwrap_val(&mut self) -> V {
            match self.swap(FutureState::Taken) {
                FutureState::Completed(val) => val,
                _ => panic!("cannot unwrap because my state is not completed"),
            }
        }
    }

    /// the state of the future; reference counted
    struct SignalInternal<V> {
        waker: WakerWrapper,
        state: FutureState<V>,
    }

    /// A handle to the future state. When you create a completable future,
    /// you should also create a signal that somebody can use to complete
    /// the future.
    #[derive(Clone)]
    pub struct CompletableFutureSignal<V> {
        internal: Arc<Mutex<SignalInternal<V>>>,
    }

    impl<V> CompletableFutureSignal<V> {
        fn mutate_self(&self, new_state: FutureState<V>) -> bool {
            let mut internal = self.internal.lock().unwrap();
            match internal.state {
                FutureState::Pending => {
                    internal.state.swap(new_state);
                    internal.waker.wake();
                    true
                }
                _ => false,
            }
        }

        /// Complete the associated CompletableFuture. This method
        /// can be called safely across multiple threads multiple times,
        /// but only the winning call would mutate the future; other calls
        /// will be rendered noop.
        ///
        /// Returns whether the call successfully mutates the future.
        pub fn complete(&self, value: V) -> bool {
            self.mutate_self(FutureState::Completed(value))
        }
    }

    /// A CompletableFuture is a future that you can expect a result (or error)
    /// from and chain logic on. You will need some executor to actively poll
    /// the result. Executors provided by the futures crate are usually good
    /// enough for common situations.
    ///
    /// If you use a custom executor, be careful that don't poll the CompletableFuture
    /// after it has already completed (or errored) in previous polls. Doing so
    /// will panic your executor.
    pub struct CompletableFuture<V> {
        internal: Arc<Mutex<SignalInternal<V>>>,
    }

    impl<V> CompletableFuture<V> {
        /// Construct a CompletableFuture.
        pub fn new() -> CompletableFuture<V> {
            CompletableFuture {
                internal: Arc::new(Mutex::new(SignalInternal {
                    waker: WakerWrapper::NotRegistered,
                    state: FutureState::Pending,
                })),
            }
        }

        /// Construct a CompletableFuture that's already completed
        /// with the value provided.
        pub fn completed(val: V) -> CompletableFuture<V> {
            CompletableFuture {
                internal: Arc::new(Mutex::new(SignalInternal {
                    waker: WakerWrapper::NotRegistered,
                    state: FutureState::Completed(val),
                })),
            }
        }

        /// Get a CompletableFutureSignal that can be used to complete
        /// or error this CompletableFuture.
        pub fn signal(&self) -> CompletableFutureSignal<V> {
            CompletableFutureSignal {
                internal: self.internal.clone(),
            }
        }
    }

    impl<V> Future for CompletableFuture<V> {
        type Output = V;

        fn poll(self: Pin<&mut Self>, ctx: &mut Context) -> Poll<Self::Output> {
            let mut signal = self.internal.lock().unwrap();
            signal.waker.register(ctx.waker());

            let state = &mut signal.state;
            match state {
                FutureState::Pending => Poll::Pending,
                FutureState::Taken => {
                    panic!("bug: the value has been taken, yet I'm still polled again")
                }
                FutureState::Completed(_) => Poll::Ready(state.unwrap_val()),
            }
        }
    }
}
