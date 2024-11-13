#![allow(unused)]

use crate::ast::*;
use crate::parse::parse;

mod ast;
mod parse;
mod union_find;

pub fn main() {
    let program = "
        fun tt() = true;
        fun not(b) = if b { false } else { true };
        fun and(b1: bool, b2): bool = 
            if b1 {
                if b2 {
                    true
                } else {
                    false
                }
            } else {
                false
            };

        $ and(not(false), tt())
    ";

    assert!(parse(program).is_ok())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PartialType {
    // Types are not recursive, so no need to have separate constructors for each variant
    Type(Type),
    Variable(TypeVar),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeVar(usize);

pub struct FunType {
    return_type: PartialType,
    arg_types: Vec<(String, PartialType)>,
}
