
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

use std::collections::HashMap;
use std::rc::Rc;
use std::iter;

use types::*;

#[derive(Debug)]
struct Extern {
    ty: Type,
    val: Value,
}

#[derive(Debug)]
struct Module {
    top_level: Vec<TExp>,
    externs: HashMap<String, Extern>,
    linked: HashMap<usize, Rc<FnInfo>>,
}

#[derive(Default)]
struct ModuleBuilder {
    externs: HashMap<String, Extern>,
    linked_fns: HashMap<usize, Rc<FnInfo>>,
}

impl ModuleBuilder {
    fn register_extern<S: Into<String>>(&mut self, sym: S, e: Extern) {
        self.externs.insert(sym.into(), e);
    }

    // FIXME
    fn link_fn(&mut self, f: Fn) {
        let Fn { id, info } = f;
        self.linked_fns.insert(id, info);
    }

    fn parse_mod<S: Into<String>>(self, input: S) -> Module {
        use read::Sexp;

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
                                                let mut args = Vec::with_capacity(params.len() /
                                                                                  2);

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
                                    "fn" => {
                                        assert!(children.len() > 1);
                                        let body = sexps_to_exps(children.split_off(1));
                                        let params = match children.remove(0) {
                                            Sexp::List(_, _, params) => {
                                                // (sym, typ)
                                                let mut args = Vec::with_capacity(params.len());

                                                for sym_sexp in params.into_iter() {
                                                    let sym = match sym_sexp {
                                                        Sexp::Atom(_, _, sym) => sym,
                                                        _ => panic!(),
                                                    };

                                                    let ty = gentyp();
                                                    args.push((sym, ty));
                                                }

                                                args
                                            }
                                            _ => panic!("first arg to fn must be a list"),
                                        };
                                        let ty = Type::Fun(repeat_call(gentyp)
                                                               .take(params.len())
                                                               .collect(),
                                                           Box::new(gentyp()));
                                        Exp::Fn(ty, params, body)
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
                    scope! {
                    env.vars => env.vars.clone();

                    let bindings = bindings
                        .into_iter()
                        .map(|(sym, ty, exp)| {
                            let var = env.new_var(sym, ty);
                            let texp = exp_to_texp(env, exp);
                            (var, texp)
                        })
                        .collect();
                    let body = body.into_iter().map(|e| exp_to_texp(env, e)).collect();

                    TExp::Let(bindings, body)
                }
                }
                Exp::Fn(ty, params, body) => {
                    scope! {
                    env.vars => env.vars.clone();

                    let params = params
                        .into_iter()
                        .map(|(sym, ty)| env.new_var(sym, ty))
                        .collect();

                    let body = body.into_iter().map(|e| exp_to_texp(env, e)).collect();

                    TExp::Fn(env.new_fn(params, body))
                }
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
                Exp::Add(args) => {
                    TExp::Add(args.into_iter().map(|e| exp_to_texp(env, e)).collect())
                }
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
            fn new_fn(&mut self, params: Vec<Var>, body: Vec<TExp>) -> Fn {
                let id = self.counter;
                self.counter += 1;
                let info = Rc::new(FnInfo { params, body });
                Fn { id, info }
            }
            fn lookup<S: AsRef<str>>(&self, sym: S) -> Option<&Var> {
                self.vars.get(sym.as_ref())
            }
        }

        let mut env = InferenceEnv::default();
        let mut input = read::InputStream::new(input.into());
        let mut reader_env = read::Env::default();
        let mut conv_env = CEnv {
            counter: 0,
            vars: HashMap::new(),
        };

        for (sym, &Extern { ref ty, .. }) in &self.externs {
            env.vars.insert(sym.clone(), ty.clone());
        }

        for (var, ty) in &env.vars {
            conv_env.new_var(var.clone(), ty.clone());
        }

        let sexps = read::read(&mut reader_env, &mut input).unwrap();
        let exps = sexps_to_exps(sexps);
        // println!("Before typing: {:#?}", exps);
        let exps = exps.into_iter().map(|e| f(&mut env, e)).collect_vec();
        // println!("After typing: {:#?}", exps);
        let texps = exps.into_iter()
            .map(|e| exp_to_texp(&mut conv_env, e))
            .collect();
        // println!("After conversion: {:#?}", texps);

        Module {
            externs: self.externs,
            top_level: texps,
            linked: self.linked_fns,
        }
    }
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
struct FnInfo {
    params: Vec<Var>,
    body: Vec<TExp>,
}

#[derive(Debug, Clone)]
struct Fn {
    id: usize,
    info: Rc<FnInfo>,
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
    Fn(Fn),
    // Extern/ExternApp
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Value {
    Unit,
    Bool(bool),
    Int(i64),
    Fn(usize),
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
            self.externs.insert(sym.clone(), e.val);
        }

        for (&id, info) in &m.linked {
            self.fns.insert(id, info.clone());
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
        let five = Extern {
            ty: Type::Fun(vec![], Box::new(Type::Int)),
            val: Value::Fn(10),
        };
        let five_fn = Fn {
            id: 10,
            info: Rc::new(FnInfo {
                              params: vec![],
                              body: vec![TExp::Int(5)],
                          }),
        };

        let input = "(five)";
        let expected = Value::Int(5);

        let mut builder = ModuleBuilder::default();
        builder.register_extern("five", five);
        builder.link_fn(five_fn);

        let module = builder.parse_mod(input);
        let mut interp = Interpreter::default();

        assert_eq!(interp.exec_module(&module), expected);
    }
}
