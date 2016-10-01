use std::collections::HashMap;

struct Env;

struct TyEnv(HashMap<String, Type>);

impl TyEnv {
    fn lookup<T: AsRef<str>>(&self, name: T) -> Type {
        self.0[name.as_ref()].clone()
    }

    fn extend<T: Into<String>>(&self, name: T, ty: Type) -> Self {
        let mut env = self.0.clone();
        env.insert(name.into(), ty);
        TyEnv(env)
    }
}

#[derive(Clone)]
enum Expr {
    Num(i64),
    Plus(Box<Expr>, Box<Expr>),
    Id(String),
    App {
        fun: Box<Expr>,
        arg: Box<Expr>,
    },
    Lam {
        arg: String,
        body: Box<Expr>,
    },
}

enum TExpr {
    Num(i64),
    Id(String),
    App {
        fun: Box<TExpr>,
        arg: Box<TExpr>,
    },
    Plus(Box<TExpr>, Box<TExpr>),
    Lam {
        arg: String,
        argT: Type,
        retT: Type,
        body: Box<TExpr>,
    },
}

enum Value {
    Num(i64),
    Closure {
        arg: String,
        body: TExpr,
        env: Env,
    },
}

#[derive(PartialEq, Debug, Clone)]
enum Type {
    Num,
    Fun {
        arg: Box<Type>,
        ret: Box<Type>,
    },
}

fn tc<T: AsRef<TExpr>>(expr: T, tenv: &TyEnv) -> Type {
    match *expr.as_ref() {
        TExpr::Num(..) => Type::Num,
        TExpr::Id(ref s) => tenv.lookup(s),
        TExpr::Plus(ref l, ref r) => {
            let lt = tc(l, tenv);
            let rt = tc(r, tenv);

            assert_eq!(lt, Type::Num);
            assert_eq!(rt, Type::Num);

            Type::Num
        }
        TExpr::App { ref fun, ref arg } => {
            let ft = tc(fun, tenv);
            let at = tc(arg, tenv);

            match ft {
                Type::Fun { ref arg, ref ret } => {
                    assert_eq!(**arg, at);
                    assert_eq!(**ret, ft);

                    *ret.clone()
                }
                _ => panic!("`ft` not a function"),
            }
        }
        TExpr::Lam { ref arg, ref argT, ref retT, ref body } => {
            let tenv = tenv.extend(arg.clone(), argT.clone());
            assert_eq!(tc(body, &tenv), *retT);

            Type::Fun {
                arg: Box::new(argT.clone()),
                ret: Box::new(retT.clone()),
            }
        }
    }
}

struct Constraint(Term, Term);

enum Term {
    TExp(Expr),
    Var(String),
    Num,
    Arrow(Box<Term>, Box<Term>),
}

fn cg<T: AsRef<Expr>>(e: T) -> Vec<Constraint> {
    let e = e.as_ref();
    match *e {
        Expr::Num(..) => vec![Constraint(Term::TExp(e.clone()), Term::Num)],
        Expr::Id(ref s) => vec![Constraint(Term::TExp(e.clone()), Term::Var(s.clone()))],
        Expr::Plus(ref l, ref r) => {
            vec![Constraint(Term::TExp(*l.clone()), Term::Num),
                 Constraint(Term::TExp(*r.clone()), Term::Num),
                 Constraint(Term::TExp(e.clone()), Term::Num)]
                .into_iter()
                .chain(cg(l))
                .chain(cg(r))
                .collect()
        }
        Expr::Lam { ref arg, ref body } => {
            vec![Constraint(Term::TExp(e.clone()),
                            Term::Arrow(Box::new(Term::Var(arg.clone())),
                                        Box::new(Term::TExp(*body.clone()))))]
                .into_iter()
                .chain(cg(body))
                .collect()
        }
        Expr::App { ref fun, ref arg } => {
            vec![Constraint(Term::TExp(*fun.clone()),
                            Term::Arrow(Box::new(Term::TExp(*arg.clone())),
                                        Box::new(Term::TExp(e.clone()))))]
                .into_iter()
                .chain(cg(fun))
                .chain(cg(arg))
                .collect()
        }
    }
}
