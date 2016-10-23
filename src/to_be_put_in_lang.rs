
use std::error::Error;
use std::rc::Rc;
use std::collections::HashMap;

use itertools::*;
use disjoint_sets::*;

use read::Sexp;

// parse pub-defs, priv-defs, and non-defs
// create initial TypeEnv, DefEnv
// pub-defs, TypeEnv, DefEnv => TypedAsts, TypeEnv2
// type infer priv-defs using TypeEnv2 => TypedAsts, TypeEnv3
// add typed priv-def-asts to DefEnv -> DefEnv2
// type inder non-defs using TypeEnv3 => TypedAsts
//
// interpret typed-non-def-asts using DefEnv2
//
//


// TODO: try_match(&TypeAcceptor, &[TypeId]) -> Result<TypeMatch, PartialMatch>

pub type Result<T> = ::std::result::Result<T, Box<::std::error::Error>>;

macro_rules! newtype_id {
    ($name:ident) => {
        #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
        pub struct $name(usize);
        
        impl ::std::convert::From<usize> for $name {
            fn from(x: usize) -> Self {
                $name(x)
            }
        }

        impl ::std::convert::From<$name> for usize {
            fn from(x: $name) -> usize {
                x.0
            }
        }
    };
}


#[derive(Debug)]
pub struct Module {
    name: Option<String>,
    pub_defs: Vec<Rc<Def>>,
    prv_defs: Vec<Rc<Def>>,
    code: Vec<Ast>,
}

#[derive(Debug)]
pub struct Def {
    name: String,
    id: usize,
    doc_string: Option<String>,
    type_sig: Option<TypeAcceptor>,
    public: bool,
    body: Rc<Expr>,
}

impl Def {
    pub fn new_private(name: String, id: usize, body: Expr) -> Self {
        Def {
            name: name,
            id: id,
            doc_string: None,
            type_sig: None,
            body: Rc::new(body),
            public: false,
        }
    }

    pub fn name(&self) -> &str {
        &*self.name
    }

    pub fn body(&self) -> Rc<Expr> {
        self.body.clone()
    }
}

pub fn parse_module<S: Into<String>>(name: Option<S>, sexps: Vec<Sexp>) -> Result<Module> {
    let mut pub_defs = vec![];
    let mut prv_defs = vec![];
    let mut def_errs = vec![];
    let mut non_defs = vec![];

    for sexp in sexps {
        match sexp {
            // defs
            Sexp::List(id, span, mut s) => {
                if !s.is_empty() && s[0].is_atom_of("def") {
                    s.remove(0);
                    match parse_def(s) {
                        Ok(d) => {
                            let d = Rc::new(d);
                            if d.public {
                                pub_defs.push(d)
                            } else {
                                prv_defs.push(d)
                            }
                        }
                        Err(e) => def_errs.push(e),
                    }
                } else {
                    non_defs.push(Sexp::List(id, span, s));
                }
            }
            _ => non_defs.push(sexp),
        }
    }

    if !def_errs.is_empty() {
        return Err(format!("Error: Failed to parse module: {}",
                           def_errs.iter().format(", "))
                       .into());
    }

    let mut def_env = HashMap::new();
    let mut type_env = HashMap::new();

    for def in pub_defs.iter().cloned() {
        def_env.insert(def.name().to_owned(), def);
    }

    infer_prv_defs(&mut def_env, &mut type_env, &*prv_defs);
//    infer_non_defs(&mut def_env, &mut type_env, &*non_defs);

    Ok(Module {
        name: name.map(Into::into),
        pub_defs: pub_defs,
        prv_defs: prv_defs,
        code: vec![],
    })
}

#[derive(Debug)]
pub enum Expr {
    Unit,
    Num(i64),
    Id(usize, String),
    App {
        fun: Rc<Expr>,
        args: Vec<Rc<Expr>>,
    },
    Lam {
        name: usize,
        body: Vec<Rc<Expr>>,
    },
}

#[derive(Debug)]
enum Term {
    TExp(Rc<Expr>),
    Var(usize),
    Num,
    Unit,
    Arrow(Vec<Term>, Box<Term>),
}

#[derive(Debug)]
enum Constraint {
    Eq(Term, Term),
}

type Node = UnionFindNode<Option<Type>>;

fn infer_prv_defs(def_env: &mut HashMap<String, Rc<Def>>,
                  type_env: &mut HashMap<usize, Type>,
                  prv_defs: &[Rc<Def>]) {


    let mut constraints = vec![];
    let mut var_env = HashMap::new();

    for def in prv_defs {
        var_env.insert(def.id, Node::new(None));
        // constraints.push(Constraint::Eq(Term::Var(def.id), Term::TExp(def.body())));
        constraint_gen(&mut constraints, def.body());
        def_env.insert(def.name().to_owned(), def.clone());
    }

    println!("\n{:?}", constraints.iter().format(",\n"));

    let unify = |a: usize, b: usize| {
        if a != b {
            let mut an = var_env.remove(&a).unwrap();
            an.union_with(var_env.get_mut(&b).unwrap(), |a, b| {
                match (a, b) {
                    (a, None) => a,
                    (None, b) => b,
                    _ => unimplemented!(),
                }
            });
            var_env.insert(a, an);
        }
    };

    for c in constraints {
        match c {
            Constraint::Eq(a, b) => {
                match a {
                    Term::TExp(expr) => {
                        match b {
                            Term::Var(id) => {
                                
                            }
                            _ => unimplemented!(),
                        }
                    }
                    _ => unimplemented!(),
                }
            }
        }
    }

    // let var_names: HashMap<_, _> = def_env.iter().map(|(k, v)| (v.id, k.to_owned())).collect();

    // let var_env: HashMap<_, _> = var_env.into_iter().map(|(k, v)| {
    //     (var_names[&k].to_owned(), v.find().clone_data())
    // }).collect();
}

fn constraint_gen(constraints: &mut Vec<Constraint>, expr: Rc<Expr>) {
    use self::Constraint::*;

    match *expr {
        Expr::Unit => {} //constraints.push(Eq(Term::TExp(expr.clone()), Term::Unit)),
        Expr::Num(_) => {} // constraints.push(Eq(Term::TExp(expr.clone()), Term::Num)),
        Expr::Id(id, _) => constraints.push(Eq(Term::TExp(expr.clone()), Term::Var(id))),
        Expr::App { ref fun, ref args } => {
            constraints.push(Eq(Term::TExp(fun.clone()),
                                Term::Arrow(args.iter().cloned().map(Term::TExp).collect(),
                                            Box::new(Term::TExp(expr.clone())))));
            constraint_gen(constraints, fun.clone());
            for expr in args.iter().cloned() {
                constraint_gen(constraints, expr);
            }
        }
        Expr::Lam { name, ref body } => {
            constraints.push(Eq(Term::TExp(expr.clone()),
                                Term::Arrow(vec![Term::Var(name)],
                                            Box::new(Term::TExp(body.last()
                                                                    .expect("empty lambda \
                                                                             body")
                                                                    .clone())))));
            for expr in body.iter().cloned() {
                constraint_gen(constraints, expr);
            }
        }
    }
}

pub fn parse_def(mut sexps: Vec<Sexp>) -> Result<Def> {
    debug!("Attempting to parse_def input: {:?}", sexps);

    if sexps.len() != 2 {
        return Err("Defs must have exactly 2 arguments".into());
    }

    let (name, id) = if let Sexp::Atom(id, _, s) = sexps.remove(0) {
        (s, id)
    } else {
        return Err("First argument of Def must be a name".into());
    };
    // let name = try!(sexps.remove(0)
    //                      .extract_atom()
    //                      .map_err(|_| Box::<Error>::from("First argument of Def must be a name")));

    let body = try!(sexp_to_expr(sexps.remove(0)));

    Ok(Def::new_private(name, id, body))
}

fn sexp_to_expr(sexp: Sexp) -> Result<Expr> {
    match sexp {
        Sexp::Atom(id, _, s) => {
            match s.parse() {
                Ok(n) => Ok(Expr::Num(n)),
                _ => Ok(Expr::Id(id, s)),
            }
        }
        Sexp::List(_, _, mut c) => {
            if c.is_empty() {
                Ok(Expr::Unit)
            } else {
                if c[0].is_atom_of("fn") {
                    c.remove(0);

                    if c.len() != 2 {
                        return Err("Fn must have 2 args".into());
                    }

                    let name = if let Sexp::Atom(id, _, _) = c.remove(0) {
                        id
                    } else {
                        return Err("First arg of fn must be a name".into());
                    };
                    // let name = try!(c.remove(0)
                    //                  .extract_atom()
                    //                  .map_err(|_| {
                    //                      Box::<Error>::from("First arg of fn must be a name")
                    //                  }));

                    Ok(Expr::Lam {
                        name: name,
                        body: try!(sexps_to_exprs(c)).into_iter().map(Rc::new).collect(),
                    })
                } else {
                    Ok(Expr::App {
                        fun: Rc::new(try!(sexp_to_expr(c.remove(0)))),
                        args: try!(sexps_to_exprs(c)).into_iter().map(Rc::new).collect(),
                    })
                }
            }
        }
    }
}

fn sexps_to_exprs(sexps: Vec<Sexp>) -> Result<Vec<Expr>> {
    sexps.into_iter().map(sexp_to_expr).collect()
}

fn id<T>(t: T) -> T {
    t
}

// regular language of types (over alphabet of TypeIds)
#[derive(Debug)]
pub enum TypeAcceptor {
    Void, // the "empty string"
    Type(TypeId),
    Union(Vec<TypeAcceptor>),
    Concat(Vec<TypeAcceptor>),
    Star(Box<TypeAcceptor>),
}

// upgrade to actually compute match
impl TypeAcceptor {
    pub fn accepts(&self, t: &[TypeId]) -> bool {
        use self::TypeAcceptor::*;

        match *self {
            Void => t.is_empty(),
            Type(id) => t.len() == 1 && t[0] == id,
            Union(ref a) => t.len() == 1 && any(iproduct!(a, t).map(|(a, &t)| a.accepts(&[t])), id),
            Concat(ref a) => {
                t.len() == a.len() && all(zip(a, t).map(|(a, &t)| a.accepts(&[t])), id)
            }
            Star(ref a) => true, // right?
        }
    }
}

pub enum Type {
    Data(TypeId),
    Fn {
        id: TypeId,
        args: Vec<Type>,
        ret: Box<Type>,
    },
}

newtype_id!(TypeId);
newtype_id!(FnId);
newtype_id!(ExprId);
newtype_id!(VarId);
newtype_id!(DoNodeId);
newtype_id!(CallNodeId);
newtype_id!(LetNodeId);

enum TypeState {
    Unknown,
    Equiv(ExprId),
    Known(TypeId),
}

struct DoNode {
    body: Vec<Ast>,
}
struct CallNode {
    args: Vec<Ast>,
    fn_id: FnId,
}
type Label = String;
struct LetNode {
    labels: Vec<Label>,
    args: Vec<Ast>,
    body: Vec<Ast>,
}

type VarTable = HashMap<VarId, TypeState>;
type DoNodeTable = HashMap<DoNodeId, DoNode>;
type CallNodeTable = HashMap<CallNodeId, CallNode>;
type LetNodeTable = HashMap<LetNodeId, LetNode>;

#[derive(Debug, Copy, Clone)]
pub enum Ast {
    Unit,
    Num(i64),
    Bool(bool),
    FnPtr(FnId),
    Var(VarId),
    Do(DoNodeId),
    Call(CallNodeId),
    Let(LetNodeId),
}

pub fn interpret(asts: &[Ast]) -> String {
    
    #[derive(Debug, Copy, Clone)]
    enum Value {
        Unit,
        Num(i64),
        Bool(bool),
        FnPtr(FnId),
    }

    fn print_value(val: &Value) -> String {
        match *val {
            Value::Unit => String::from("()"),
            Value::Bool(v) => format!("{}", v),
            Value::Num(v) => format!("{}", v),
            Value::FnPtr(v) => format!("<fn {}>", v.0),
        }
    }

    fn run_do(env: &HashMap<VarId, Value>, body: &[Ast]) -> Value {
        let (last, rest) = body.split_last().unwrap();

        for ast in rest {
            interpret_(&env, ast);
        }

        interpret_(&env, last)
    }

    fn interpret_(env: &HashMap<VarId, Value>, ast: &Ast) -> Value {
        match *ast {
            Ast::Unit => Value::Unit,
            Ast::Num(val) => Value::Num(val),
            Ast::Bool(val) => Value::Bool(val),
            Ast::FnPtr(fn_id) => Value::FnPtr(fn_id),
            Ast::Var(var_id) => env[&var_id],
            Ast::Do(do_node_id) => unimplemented!(),
            Ast::Call(call_node_id) => unimplemented!(),
            Ast::Let(let_node_id) => unimplemented!(),
        }
    }

    debug!("Interpret: {:#?}", asts);

    if asts.is_empty() {
        return String::from("");
    }

    let mut var_env = HashMap::new(); // HashMap<VarId, Value>;
    
    let result  = run_do(&var_env, asts);
    print_value(&result)
}


#[cfg(test)]
mod tests {
    extern crate slog_envlogger;

    use lang;
    use super::TypeAcceptor::*;
    use super::TypeId;

    #[test]
    fn accept() {
        assert!(Void.accepts(&[]));
        assert!(!Void.accepts(&[TypeId(0)]));
        assert!(Type(TypeId(5)).accepts(&[TypeId(5)]));
        assert!(!Type(TypeId(5)).accepts(&[TypeId(0)]));
        assert!(!Type(TypeId(5)).accepts(&[TypeId(5), TypeId(0)]));
    }

    #[ignore]
    #[test]
    fn parse() {
        let _ = slog_envlogger::init();

        let input = "(def a 1) (def b 2) (def c b) (def d c) a b";
        let sexps = lang::read(input);

        panic!("{:#?}", super::parse_module(Some("test_mod"), sexps));
    }

    #[test]
    fn interpret() {
        let _ = slog_envlogger::init();

        let input = "1";
        let sexps = lang::read(input);
        let module = super::parse_module(Some("interpret_mod"), sexps).unwrap();
        let result = super::interpret(&*module.code);

        assert_eq!("1", result);
    }
}
