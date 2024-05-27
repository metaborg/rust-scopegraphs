use std::fmt::{Debug, Formatter};

use futures::Future;
use smol::channel::{bounded, Sender};

use crate::{ast::Type, PartialType, TypeVar};

#[derive(Default)]
pub struct UnionFind {
    /// Records the parent of each type variable.
    /// Kind of assumes type variables are assigned linearly.
    ///
    /// For example the "parent" of type variable 0 is stored at index 0
    parent: Vec<PartialType>,
    /// Keep track of type variables we've given out
    vars: usize,
    /// A vec of signals for each type variable.
    ///
    /// For example, whenever type variable 0 is unified with anything,
    /// we go through the list at index 0 and notify each.
    callbacks: Vec<Vec<Sender<PartialType>>>,
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
    pub fn fresh(&mut self) -> TypeVar {
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
    pub fn union(&mut self, left: PartialType, right: PartialType) {
        let left = self.find_partial_type(left);
        let right = self.find_partial_type(right);

        match (left, right) {
            (PartialType::Variable(left), right) | (right, PartialType::Variable(left)) => {
                // FIXME: use rank heuristic in case right is a variable?
                *self.get(left) = right.clone();
                if self.callbacks.len() > left.0 {
                    for fut in self.callbacks[left.0].drain(..) {
                        let _ = fut.send_blocking(right.clone());
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
    pub fn find_partial_type(&mut self, ty: PartialType) -> PartialType {
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

    #[allow(unused)]
    fn type_of(&mut self, var: TypeVar) -> Option<Type> {
        match self.find(var) {
            PartialType::Variable(_) => None,
            PartialType::Record { name, .. } => Some(Type::StructRef(name)),
            PartialType::Int => Some(Type::Int),
        }
    }

    pub fn type_of_partial_type(&mut self, var: PartialType) -> Option<Type> {
        match self.find_partial_type(var) {
            PartialType::Variable(_) => None,
            PartialType::Record { name, .. } => Some(Type::StructRef(name)),
            PartialType::Int => Some(Type::Int),
        }
    }

    /// Wait for when tv is unified with something.
    pub fn wait_for_unification(&mut self, tv: TypeVar) -> impl Future<Output = PartialType> {
        let callbacks = &mut self.callbacks;
        for _ in callbacks.len()..=tv.0 {
            callbacks.push(vec![]);
        }

        let (tx, rx) = bounded(1);
        callbacks[tv.0].push(tx);

        // not an async function, cause when we await this we don't want to hold on to a &mut self.
        // This future can complete on its own.
        async move { rx.recv().await.expect("sender dropped") }
    }
}
