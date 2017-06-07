
//! The Aleph Programming Langauge

// temp
#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]


#[macro_use]
extern crate itertools;
extern crate petgraph;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

use itertools::*;

// pub mod _lang;
pub mod read;

pub mod types;

use std::collections::HashMap;
use std::rc::Rc;
use std::iter;
use std::mem;

use types::*;

#[derive(Debug)]
struct Module {
    top_level: Vec<TExp>,
}

type Env = HashMap<String, Type>;

fn parse_mod<S: Into<String>>(input: S) -> Module {
    use read::Sexp;

    let mut env = Env::new();
    let mut input = read::InputStream::new(input.into());
    let mut reader_env = read::Env::default();
    let sexps = read::read(&mut reader_env, &mut input).unwrap();

    fn sexp_to_exp(s: Sexp) -> Exp {
        match s {
            Sexp::Atom(_, _, sym) => {
                match sym.parse() {
                    Ok(val) => Exp::Int(val),
                    _ => {
                        match &*sym {
                            "true" => Exp::Bool(true),
                            "false" => Exp::Bool(false),
                            _ => Exp::Var(sym),
                        }
                    }
                }
            }
            Sexp::List(_, _, mut children) => {
                if children.is_empty() {
                    Exp::Unit
                } else {
                    let callee = sexp_to_exp(children.remove(0));
                    match callee {
                        Exp::Var(sym) => {
                            match &*sym {
                                "+" => Exp::Add(sexps_to_exps(children)),
                                "let" => {
                                    assert!(children.len() > 1);
                                    let body = sexps_to_exps(children.split_off(1));
                                    let params = match children.remove(0) {
                                        Sexp::List(_, _, params) => {
                                            assert!(params.len() % 2 == 0);
                                            // (sym, typ, exp)
                                            let mut args = Vec::with_capacity(params.len() / 2);

                                            for (sym_sexp, exp_sexp) in
                                                params.into_iter().tuples() {
                                                let sym = match sym_sexp {
                                                    Sexp::Atom(_, _, sym) => sym,
                                                    _ => panic!(),
                                                };

                                                let ty = gentyp();
                                                let exp = sexp_to_exp(exp_sexp);

                                                args.push((sym, ty, exp));
                                            }

                                            args
                                        }
                                        _ => panic!("first arg to let must be a list"),
                                    };

                                    Exp::Let(params, body)
                                }
                                "if" => {
                                    assert_eq!(children.len(), 3);
                                    let cond = sexp_to_exp(children.remove(0));
                                    let b1 = sexp_to_exp(children.remove(0));
                                    let b2 = sexp_to_exp(children.remove(0));
                                    Exp::If(Box::new(cond), Box::new(b1), Box::new(b2))
                                }
                                _ => Exp::App(Box::new(Exp::Var(sym)), sexps_to_exps(children)),
                            }
                        }
                        _ => Exp::App(Box::new(callee), sexps_to_exps(children)),
                    }
                }
            }
        }
    }

    fn sexps_to_exps(s: Vec<Sexp>) -> Vec<Exp> {
        s.into_iter().map(sexp_to_exp).collect()
    }

    fn exp_to_texp(env: &mut CEnv, e: Exp) -> TExp {
        match e {
            Exp::Unit => TExp::Unit,
            Exp::Bool(val) => TExp::Bool(val),
            Exp::Int(val) => TExp::Int(val),
            Exp::Let(bindings, body) => {
                let new_env = env.vars.clone();
                // push scope
                let old_env = mem::replace(&mut env.vars, new_env);

                let bindings = bindings
                    .into_iter()
                    .map(|(sym, ty, exp)| {
                             let var = env.new_var(sym, ty);
                             let texp = exp_to_texp(env, exp);
                             (var, texp)
                         })
                    .collect();
                let body = body.into_iter().map(|e| exp_to_texp(env, e)).collect();

                // pop scope
                mem::replace(&mut env.vars, old_env);

                TExp::Let(bindings, body)
            }
            Exp::Var(sym) => {
                match env.lookup(sym) {
                    Some(ref var) => TExp::Var((*var).clone()),
                    _ => unreachable!(),
                }
            }
            Exp::App(callee, args) => {
                let texps = iter::once(*callee)
                    .chain(args)
                    .map(|e| exp_to_texp(env, e))
                    .collect();
                TExp::App(texps)
            }
            Exp::If(cond, b1, b2) => {
                let cond = exp_to_texp(env, *cond);
                let b1 = exp_to_texp(env, *b1);
                let b2 = exp_to_texp(env, *b2);
                TExp::If(Box::new((cond, b1, b2)))
            }
            Exp::Add(args) => TExp::Add(args.into_iter().map(|e| exp_to_texp(env, e)).collect()),
        }
    }

    struct CEnv {
        counter: usize,
        vars: HashMap<String, Var>,
    }

    impl CEnv {
        fn new_var(&mut self, sym: String, ty: Type) -> Var {
            let id = self.counter;
            self.counter += 1;
            let info = Rc::new(VarInfo {
                                   sym: sym.clone(),
                                   ty,
                               });
            let var = Var { id, info };
            self.vars.insert(sym, var.clone());
            var
        }
        fn lookup<S: AsRef<str>>(&self, sym: S) -> Option<&Var> {
            self.vars.get(sym.as_ref())
        }
    }

    let mut texps = vec![];
    let mut conv_env = CEnv {
        counter: 0,
        vars: HashMap::new(),
    };

    for sexp in sexps {
        let exp = f(&mut env, sexp_to_exp(sexp));
        let texp = exp_to_texp(&mut conv_env, exp);
        texps.push(texp);
    }

    Module { top_level: texps }
}

#[derive(Debug)]
struct VarInfo {
    sym: String,
    ty: Type,
}

#[derive(Debug, Clone)]
struct Var {
    id: usize,
    info: Rc<VarInfo>,
}

#[derive(Debug)]
enum TExp {
    Unit,
    Bool(bool),
    Int(i64),
    Let(Vec<(Var, TExp)>, Vec<TExp>),
    Var(Var),
    // callee is included
    App(Vec<TExp>),
    If(Box<(TExp, TExp, TExp)>),
    Add(Vec<TExp>),
    // Extern/ExternApp
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Value {
    Unit,
    Bool(bool),
    Int(i64),
}

struct Interpreter {
    vars: HashMap<usize, Value>,
}

impl Interpreter {
    fn new() -> Self {
        Interpreter { vars: HashMap::new() }
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
            TExp::Var(ref var) => self.vars[&var.id],
            TExp::If(ref exps) => {
                match **exps {
                    (ref cond, ref b1, ref b2) => {
                        match self.eval(cond) {
                            Value::Bool(val) => if val { self.eval(b1) } else { self.eval(b2) },
                            _ => unreachable!(),
                        }
                    }
                }
            }
            TExp::App(ref exps) => {
                let (callee, args) = exps.split_first().unwrap_or_else(|| unreachable!());
                unimplemented!()
            }
            TExp::Let(ref bindings, ref body) => {
                let new_vars = self.vars.clone();
                let vars = mem::replace(&mut self.vars, new_vars);

                for &(ref var, ref exp) in bindings {
                    let value = self.eval(exp);
                    self.vars.insert(var.id, value);
                }

                let (last, rest) = body.split_last().unwrap_or_else(|| unreachable!());

                for exp in rest {
                    self.eval(exp);
                }

                let value = self.eval(last);

                mem::replace(&mut self.vars, vars);
                value
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
        let input = "(let (x (let (x 4) x)) (let (x 5) x))";
        let expected = Value::Int(5);

        let module = parse_mod(input);
        let exp = &module.top_level[0];
        let mut interp = Interpreter::new();
        assert_eq!(interp.eval(exp), expected);
    }
}


// pub mod ast;
// pub mod lang;
// mod typecheck;

// mod census;
// mod compile;
// mod print_table;

// pub mod reader;
// pub mod analyzer;
// pub mod symbol_table;
// pub mod interpreter; // this will eventually be entirely replaced by vm
// pub mod vm;
