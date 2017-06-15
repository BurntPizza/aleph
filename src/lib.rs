
//! The Aleph Programming Langauge

// temp
#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]


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

use module::{Extern, ExternFnPtr, Module, ModuleBuilder};
use interpreter::{Value, Interpreter};
use types::*;

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
    Let(Vec<(Var, TExp)>, Vec<TExp>),
    Var(Var),
    // callee is included
    App(Vec<TExp>),
    IfElse(Box<(TExp, TExp, TExp)>),
    If(Box<(TExp, TExp)>),
    Add(Vec<TExp>),
    Fn(Fn),
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_eq {
        ($name:ident; $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let input: &str = $input;
                let expected: Value = $expected;

                let module = ModuleBuilder::default().parse_mod(input);
                let mut interp = Interpreter::default();

                assert_eq!(interp.exec_module(&module), expected);
            }
        };
    }

    #[test]
    fn test_extern() {
        fn five_fn_ptr(args: &[Value]) -> Value {
            assert!(args.is_empty());
            Value::Int(5)
        }

        let five = Extern::Fn {
            ty: Type::Fun(vec![], Box::new(Type::Int)),
            ptr: ExternFnPtr(five_fn_ptr),
        };

        let input = "5";
        let expected = Value::Int(5);

        let mut builder = ModuleBuilder::default();
        builder.register_extern("five", five);

        let module = builder.parse_mod(input);
        let mut interp = Interpreter::default();

        assert_eq!(interp.exec_module(&module), expected);
    }

    test_eq!(test_higher_order; "(let (f (fn (x) (x 4)) \
                                       a (fn (b) b))    \
                                   (f a))", Value::Int(4));

    test_eq!(test_closure; "(let (x 1
                                  a (fn (b) (+ x b)))
                              (a 1))", Value::Int(2));

    test_eq!(test_module_def; "(def foo 5) foo", Value::Int(5));

    #[test]
    fn test_module_import() {
        let m1 = "(pub foo 5)";
        let m2 = "(use m1 *) foo";
        let expected = Value::Int(5);

        let m1 = ModuleBuilder::default().parse_mod(m1);
        let mut m2b = ModuleBuilder::default();
        m2b.link_module("m1", &m1);

        let m2 = m2b.parse_mod(m2);
        let mut interp = Interpreter::default();

        assert_eq!(interp.exec_module(&m2), expected);
    }

    #[test]
    fn test_if() {
        fn print_fn_ptr(args: &[Value]) -> Value {
            assert!(args.is_empty());
            println!("Hello from 'print'");
            Value::Unit
        }

        let print = Extern::Fn {
            ty: Type::Fun(vec![], Box::new(Type::Unit)),
            ptr: ExternFnPtr(print_fn_ptr),
        };

        let mut builder = ModuleBuilder::default();
        builder.register_extern("print", print);

        let input = "(let (b true) (if b (print)))";
        let expected = Value::Unit;

        let module = builder.parse_mod(input);
        let mut interp = Interpreter::default();

        assert_eq!(interp.exec_module(&module), expected);
    }
}
