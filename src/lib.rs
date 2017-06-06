
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
                env.push_scope();
                let bindings = bindings.into_iter().map(|(sym, ty, exp)| {
                    let var = env.new_var(sym, ty);
                    let texp = exp_to_texp(env, exp);
                    (var, texp)
                }).collect();
                let body = body.into_iter().map(|e|exp_to_texp(env, e)).collect();
                env.pop_scope();
                TExp::Let(bindings, body)
            }
            Exp::Var(sym) => {
                match env.lookup(sym) {
                    Some(ref var) => TExp::Var((*var).clone()),
                    _ => unreachable!(),
                }
            }
            Exp::App(callee, args) => {
                let texps = iter::once(*callee).chain(args).map(|e| exp_to_texp(env, e)).collect();
                TExp::App(texps)
            }
            Exp::If(cond, b1, b2) => {
                let cond = exp_to_texp(env, *cond);
                let b1 = exp_to_texp(env, *b1);
                let b2 = exp_to_texp(env, *b2);
                TExp::If(Box::new((cond, b1, b2)))
            }
            Exp::Add(args) => {
                TExp::Add(args.into_iter().map(|e| exp_to_texp(env, e)).collect())
            }
        }
    }

    struct CEnv {
        counter: usize,
        envs: Vec<HashMap<String, Rc<Var>>>,
    }

    impl CEnv {
        fn push_scope(&mut self) {
            let new_env = self.envs.last().unwrap().clone();
            self.envs.push(new_env);
        }
        fn pop_scope(&mut self) {
            self.envs.pop();
        }

        fn new_var(&mut self, sym: String, ty: Type) -> Rc<Var> {
            let id = self.counter;
            self.counter += 1;
            let var = Rc::new(Var{id, sym: sym.clone(), ty});
            self.envs.last_mut().unwrap().insert(sym, var.clone());
            var
        }
        fn lookup<S: AsRef<str>>(&self, sym: S) -> Option<&Rc<Var>> {
            self.envs.last().unwrap().get(sym.as_ref())
        }
    }

    let mut texps = vec![];
    let mut conv_env = CEnv{counter:0, envs: vec![HashMap::new()]};

    for sexp in sexps {
        let exp = f(&mut env, sexp_to_exp(sexp));
        let texp = exp_to_texp(&mut conv_env, exp);
        texps.push(texp);
    }

    Module { top_level: texps }
}

#[derive(Debug)]
struct Var {
    id: usize,
    ty: Type,
    sym: String,
}

#[derive(Debug)]
enum TExp {
    Unit,
    Bool(bool),
    Int(i64),
    Let(Vec<(Rc<Var>, TExp)>, Vec<TExp>),
    Var(Rc<Var>),
    // callee is included
    App(Vec<TExp>),
    If(Box<(TExp, TExp, TExp)>),
    Add(Vec<TExp>),
}



#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use types::*;
    use super::*;

    #[test]
    fn test_parse_mod() {
        let input = "(let (x 4) x)";
        let module = parse_mod(input);

        println!("{:#?}", module);
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
