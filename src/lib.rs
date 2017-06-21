
//! The Aleph Programming Langauge

// temp
#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]

#![cfg_attr(feature = "bench", feature(test))]
#![cfg_attr(feature = "bench", allow(unused_features))]

#[macro_use]
extern crate itertools;
extern crate petgraph as pg;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

use itertools::*;

macro_rules! scope {
    ($var:expr => $val:expr; $($code:tt)+) => {
        {
            use std::mem;
            let new = $val;
            let old = mem::replace(&mut $var, new);
            let value = {$($code)+};
            mem::replace(&mut $var, old);
            value
        }
    };
}

pub mod read;
pub mod types;
pub mod module;
pub mod interpreter;
pub mod macros;
#[cfg(test)]
mod tests;
#[cfg(all(test, feature = "bench"))]
mod bench;

use module::{Extern, ExternFnPtr, Module, ModuleBuilder};
use interpreter::{Value, Interpreter};
use types::*;
use read::Sexp;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::iter;


#[derive(Debug)]
pub struct VarInfo {
    sym: String,
    ty: Type,
}

#[derive(Debug, Clone)]
pub struct Var {
    id: usize,
    info: Rc<VarInfo>,
}

#[derive(Debug)]
pub struct FnInfo {
    params: Vec<Var>,
    body: Vec<TExp>,
}

#[derive(Debug, Clone)]
pub struct Fn {
    id: usize,
    info: Rc<FnInfo>,
}

#[derive(Debug)]
pub enum TExp {
    Unit,
    Bool(bool),
    Int(i64),
    Quote(Sexp),
    Let(Vec<(Var, TExp)>, Vec<TExp>),
    Var(Var),
    // callee is included
    App(Vec<TExp>),
    IfElse(Box<(TExp, TExp, TExp)>),
    If(Box<(TExp, TExp)>),
    Add(Vec<TExp>),
    Fn(Fn),
}

impl TExp {
    pub fn ty(&self) -> Type {
        match *self {
            TExp::Unit => Type::Unit,
            TExp::Bool(_) => Type::Bool,
            TExp::Int(_) => Type::Int,
            TExp::Quote(ref texp) => unimplemented!(),
            TExp::Let(_, ref body) => body.last().unwrap().ty(),
            TExp::Var(ref var) => var.info.ty.clone(),
            TExp::App(ref exps) => {
                match exps[0].ty() {
                    Type::Fun(_, ret) => *ret,
                    _ => unreachable!(),
                }
            }
            TExp::IfElse(ref exps) => exps.1.ty(),
            TExp::If(ref exps) => exps.1.ty(),
            TExp::Add(_) => Type::Int,
            TExp::Fn(ref f) => f.info.body.last().unwrap().ty(),
        }
    }
}
