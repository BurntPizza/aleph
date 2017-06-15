
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

use module::{Extern, ExternFnPtr, Module, ModuleBuilder};

use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::iter;
use std::fmt::{self, Debug, Formatter};

use types::*;

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
    If(Box<(TExp, TExp, TExp)>),
    Add(Vec<TExp>),
    Fn(Fn),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Int(i64),
    Fn(usize),
    ExternFn(ExternFnPtr),
}

#[derive(Default)]
struct Interpreter {
    vars: HashMap<usize, Value>,
    fns: HashMap<usize, Rc<FnInfo>>,
    externs: HashMap<String, Value>,
}

impl Interpreter {
    fn exec_module(&mut self, m: &Module) -> Value {
        for (sym, e) in &m.externs {
            match *e {
                Extern::Fn { ptr, .. } => {
                    self.externs.insert(sym.clone(), Value::ExternFn(ptr));
                }
            }
        }

        // fix this?
        for (sym, rc) in &m.defs {
            let &(ref texp, _) = &**rc;
            let val = self.eval(texp);
            self.externs.insert(sym.clone(), val);
        }

        if let Some((last, rest)) = m.top_level.split_last() {
            for texp in rest {
                self.eval(texp);
            }
            self.eval(last)
        } else {
            Value::Unit
        }
    }

    fn eval(&mut self, exp: &TExp) -> Value {
        match *exp {
            TExp::Unit => Value::Unit,
            TExp::Bool(val) => Value::Bool(val),
            TExp::Int(val) => Value::Int(val),
            TExp::Add(ref args) => {
                Value::Int(args.into_iter()
                               .map(|arg| match self.eval(arg) {
                                        Value::Int(val) => val,
                                        _ => unreachable!(),
                                    })
                               .fold(0, |acc, e| acc + e))
            }
            TExp::Var(ref var) => {
                if let Some(&val) = self.vars.get(&var.id) {
                    val
                } else {
                    self.externs[&var.info.sym]
                }
            }
            TExp::If(ref exps) => {
                let (ref cond, ref b1, ref b2) = **exps;
                match self.eval(cond) {
                    Value::Bool(val) => if val { self.eval(b1) } else { self.eval(b2) },
                    _ => unreachable!(),
                }
            }
            TExp::App(ref exps) => {
                let (callee, args) = exps.split_first().unwrap_or_else(|| unreachable!());
                match self.eval(callee) {
                    Value::Fn(id) => {
                        let FnInfo {
                            ref params,
                            ref body,
                        } = *(self.fns[&id].clone());

                        scope! {
                            self.vars => self.vars.clone();

                            for (var, arg) in zip(params, args) {
                                let value = self.eval(arg);
                                self.vars.insert(var.id, value);
                            }

                            let (last, rest) = body.split_last().unwrap_or_else(|| unreachable!());

                            for exp in rest {
                                self.eval(exp);
                            }

                            let value = self.eval(last);

                            value
                        }
                    }
                    Value::ExternFn(ptr) => {
                        let args = args.into_iter().map(|e| self.eval(e)).collect_vec();
                        ptr.0(&*args)
                    }
                    _ => unreachable!(),
                }
            }
            TExp::Let(ref bindings, ref body) => {
                scope! {
                    self.vars => self.vars.clone();

                    for &(ref var, ref exp) in bindings {
                        let value = self.eval(exp);
                        self.vars.insert(var.id, value);
                    }

                    let (last, rest) = body.split_last().unwrap_or_else(|| unreachable!());

                    for exp in rest {
                        self.eval(exp);
                    }

                    let value = self.eval(last);

                    value
                }
            }
            TExp::Fn(Fn { id, ref info }) => {
                self.fns.insert(id, info.clone());
                Value::Fn(id)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use types::*;
    use super::*;

    #[test]
    fn test_interpret() {
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

    #[test]
    fn test_higher_order() {
        fn five_fn_ptr(args: &[Value]) -> Value {
            assert!(args.is_empty());
            Value::Int(5)
        }

        let five = Extern::Fn {
            ty: Type::Fun(vec![], Box::new(Type::Int)),
            ptr: ExternFnPtr(five_fn_ptr),
        };

        let input = "(let (f (fn (x) (x 4)) \
                           a (fn (b) b)) \
                      (f a))";
        let expected = Value::Int(4);

        let builder = ModuleBuilder::default();

        let module = builder.parse_mod(input);
        let mut interp = Interpreter::default();

        assert_eq!(interp.exec_module(&module), expected);
    }

    #[test]
    fn test_closure() {
        fn five_fn_ptr(args: &[Value]) -> Value {
            assert!(args.is_empty());
            Value::Int(5)
        }

        let five = Extern::Fn {
            ty: Type::Fun(vec![], Box::new(Type::Int)),
            ptr: ExternFnPtr(five_fn_ptr),
        };

        let input = "(let (x 1
                           a (fn (b) (+ x b)))
                      (a 1))";
        let expected = Value::Int(2);

        let builder = ModuleBuilder::default();

        let module = builder.parse_mod(input);
        let mut interp = Interpreter::default();

        assert_eq!(interp.exec_module(&module), expected);
    }

    #[test]
    fn test_module_def() {
        let input = "(def foo 5) foo";
        let expected = Value::Int(5);

        let module = ModuleBuilder::default().parse_mod(input);
        let mut interp = Interpreter::default();

        assert_eq!(interp.exec_module(&module), expected);
    }

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
    fn test_slice_swap() {
        let mut v = vec![0, 1, 2];
        v.swap(0, 1);
        assert_equal(&v, &[1, 0, 2]);
        v.swap(1, 1);
        assert_equal(&v, &[1, 0, 2]);
    }
}
