
use std::rc::Rc;
use std::cell::{RefCell, Ref};
use std::collections::HashMap;

#[derive(PartialEq, Clone, Debug)]
pub struct TRef(Rc<RefCell<Option<Type>>>);

impl TRef {
    fn new() -> Self {
        TRef(Rc::new(RefCell::new(None)))
    }

    fn get(&self) -> Ref<Option<Type>> {
        self.0.borrow()
    }

    fn set(&self, v: Type) {
        *self.0.borrow_mut() = Some(v);
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Fun(Vec<Type>, Box<Type>),
    Var(TRef),
}

pub fn gentyp() -> Type {
    Type::Var(TRef::new())
}

#[derive(Debug, Clone)]
pub enum Exp {
    Unit,
    Bool(bool),
    Int(i64),
    Let(Vec<(String, Type, Exp)>, Vec<Exp>),
    Var(String),
    App(Box<Exp>, Vec<Exp>),
    If(Box<Exp>, Box<Exp>, Box<Exp>),
    Add(Vec<Exp>),
}

fn deref_typ(t: Type) -> Type {
    match t {
        Type::Fun(t1s, t2) => {
            Type::Fun(t1s.into_iter().map(deref_typ).collect(),
                      Box::new(deref_typ(*t2)))
        }
        Type::Var(r) => {
            let o = (*r.get()).clone();
            match o {
                Some(t) => {
                    let tp = deref_typ(t);
                    r.set(tp.clone());
                    tp
                }
                _ => panic!("Unable to infer type"),
            }
        }
        t => t,
    }
}

fn deref_term(e: Exp) -> Exp {
    use self::Exp::*;

    fn map(e: Box<Exp>) -> Box<Exp> {
        // could use a map-in-place version
        Box::new(deref_term(*e))
    }

    match e {
        Add(es) => Add(es.into_iter().map(deref_term).collect()),
        If(e1, e2, e3) => If(map(e1), map(e2), map(e3)),
        Let(params, body) => {
            let params = params
                .into_iter()
                .map(|(sym, ty, e)| (sym, deref_typ(ty), deref_term(e)))
                .collect();
            Let(params, body.into_iter().map(deref_term).collect())
        }
        App(e, es) => App(map(e), es.into_iter().map(deref_term).collect()),
        e => e,
    }
}

fn occur(r1: &TRef, t: &Type) -> bool {
    match *t {
        Type::Fun(ref t2s, ref t2) => t2s.into_iter().any(|t2| occur(r1, t2)) || occur(r1, &*t2),
        Type::Var(ref r2) => {
            if r1 == r2 {
                true
            } else if let None = *r2.get() {
                false
            } else if let Some(ref t2) = *r2.get() {
                occur(r1, t2)
            } else {
                unreachable!()
            }
        }
        _ => false,
    }
}

fn unify(t1: &Type, t2: &Type) -> Result<(), (Type, Type)> {
    match (t1, t2) {
        (&Type::Unit, &Type::Unit) |
        (&Type::Bool, &Type::Bool) |
        (&Type::Int, &Type::Int) => Ok(()),
        (&Type::Fun(ref t1s, ref t1p), &Type::Fun(ref t2s, ref t2p)) => {
            for (t1, t2) in t1s.into_iter().zip(t2s) {
                unify(t1, t2)?;
            }
            unify(t1p, t2p)
        }
        (&Type::Var(ref r1), &Type::Var(ref r2)) if r1 == r2 => Ok(()),
        (&Type::Var(ref r1), _) if r1.get().is_some() => {
            let t1p = r1.get().clone().unwrap();
            unify(&t1p, t2)
        }
        (_, &Type::Var(ref r2)) if r2.get().is_some() => {
            let t2p = r2.get().clone().unwrap();
            unify(t1, &t2p)
        }
        (&Type::Var(ref r1), _) if r1.get().is_none() => {
            if occur(r1, t2) {
                return Err((t1.clone(), t2.clone()));
            }
            r1.set(t2.clone());
            Ok(())
        }
        (_, &Type::Var(ref r2)) if r2.get().is_none() => {
            if occur(r2, t1) {
                return Err((t1.clone(), t2.clone()));
            }
            r2.set(t1.clone());
            Ok(())
        }
        _ => Err((t1.clone(), t2.clone())),
    }
}

pub fn g(env: &mut HashMap<String, Type>, e: &Exp) -> Type {
    use self::Exp::*;

    match *e {
        Unit => Type::Unit,
        Bool(_) => Type::Bool,
        Int(_) => Type::Int,
        Add(ref es) => {
            for e in es {
                unify(&Type::Int, &g(env, e)).unwrap();
            }
            Type::Int
        }
        Let(ref params, ref e2) => {
            let mut env = env.clone();

            for &(ref x, ref t, ref e1) in params {
                unify(t, &g(&mut env, e1)).unwrap();
                env.insert(x.clone(), t.clone());
            }
            
            let (last, rest) = e2.split_last().unwrap();
            for e in rest {
                g(&mut env, e);
            }

            g(&mut env, last)
        }
        Var(ref x) => {
            if let Some(x) = env.get(x).cloned() {
                x
            } else {
                panic!("Unknown sym: {:?}", x);
            }
        }
        App(ref e, ref es) => {
            let t = gentyp();
            let tf = Type::Fun(es.into_iter().map(|e| g(env, e)).collect(),
                               Box::new(t.clone()));
            unify(&g(env, e), &tf).unwrap();
            t
        }
        If(ref e1, ref e2, ref e3) => {
            unify(&g(env, e1), &Type::Bool).unwrap();
            let t2 = g(env, e2);
            let t3 = g(env, e3);
            unify(&t2, &t3).unwrap();
            t2
        }
    }
}

pub fn f(env: &mut HashMap<String, Type>, e: Exp) -> Exp {
    g(env, &e);
    deref_term(e)
}
