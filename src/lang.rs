
use std::error::Error;
use std::rc::Rc;
use std::collections::HashMap;

use itertools::*;
use disjoint_sets::*;

use tiered_map::*;

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

pub type AResult<T> = Result<T, Box<::std::error::Error>>;

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

// #[derive(Debug)]
// pub struct Module {
//     name: Option<String>,
//     pub_defs: Vec<Rc<Def>>,
//     prv_defs: Vec<Rc<Def>>,
//     code: Vec<Ast>,
// }

// #[derive(Debug)]
// pub struct Def {
//     name: String,
//     id: usize,
//     doc_string: Option<String>,
//     type_sig: Option<TypeAcceptor>,
//     public: bool,
//     body: Rc<Expr>,
// }

// impl Def {
//     pub fn new_private(name: String, id: usize, body: Expr) -> Self {
//         Def {
//             name: name,
//             id: id,
//             doc_string: None,
//             type_sig: None,
//             body: Rc::new(body),
//             public: false,
//         }
//     }

//     pub fn name(&self) -> &str {
//         &*self.name
//     }

//     pub fn body(&self) -> Rc<Expr> {
//         self.body.clone()
//     }
// }

// pub fn parse_module<S: Into<String>>(name: Option<S>, sexps: Vec<Sexp>) -> AResult<Module> {
//     let mut pub_defs = vec![];
//     let mut prv_defs = vec![];
//     let mut def_errs = vec![];
//     let mut non_defs = vec![];

//     for sexp in sexps {
//         match sexp {
//             // defs
//             Sexp::List(id, span, mut s) => {
//                 if !s.is_empty() && s[0].is_atom_of("def") {
//                     s.remove(0);
//                     match parse_def(s) {
//                         Ok(d) => {
//                             let d = Rc::new(d);
//                             if d.public {
//                                 pub_defs.push(d)
//                             } else {
//                                 prv_defs.push(d)
//                             }
//                         }
//                         Err(e) => def_errs.push(e),
//                     }
//                 } else {
//                     non_defs.push(Sexp::List(id, span, s));
//                 }
//             }
//             _ => non_defs.push(sexp),
//         }
//     }

//     if !def_errs.is_empty() {
//         return Err(format!("Error: Failed to parse module: {}",
//                            def_errs.iter().format(", "))
//                        .into());
//     }

//     let mut def_env = HashMap::new();
//     let mut type_env = HashMap::new();

//     for def in pub_defs.iter().cloned() {
//         def_env.insert(def.name().to_owned(), def);
//     }

//     infer_prv_defs(&mut def_env, &mut type_env, &*prv_defs);
//     // infer_non_defs(&mut def_env, &mut type_env, &*non_defs);

//     Ok(Module {
//         name: name.map(Into::into),
//         pub_defs: pub_defs,
//         prv_defs: prv_defs,
//         code: vec![],
//     })
// }

// #[derive(Debug)]
// pub enum Expr {
//     Unit,
//     Num(i64),
//     Id(usize, String),
//     App {
//         fun: Rc<Expr>,
//         args: Vec<Rc<Expr>>,
//     },
//     Lam {
//         name: usize,
//         body: Vec<Rc<Expr>>,
//     },
// }

// #[derive(Debug)]
// enum Term {
//     TExp(Rc<Expr>),
//     Var(usize),
//     Num,
//     Unit,
//     Arrow(Vec<Term>, Box<Term>),
// }

// #[derive(Debug)]
// enum Constraint {
//     Eq(Term, Term),
// }

// type Node = UnionFindNode<Option<Type>>;

// fn infer_prv_defs(def_env: &mut HashMap<String, Rc<Def>>,
//                   type_env: &mut HashMap<usize, Type>,
//                   prv_defs: &[Rc<Def>]) {


//     let mut constraints = vec![];
//     let mut var_env = HashMap::new();

//     for def in prv_defs {
//         var_env.insert(def.id, Node::new(None));
//         // constraints.push(Constraint::Eq(Term::Var(def.id), Term::TExp(def.body())));
//         constraint_gen(&mut constraints, def.body());
//         def_env.insert(def.name().to_owned(), def.clone());
//     }

// println!("\n{:?}", constraints.iter().format(",\n"));

//     let unify = |a: usize, b: usize| {
//         if a != b {
//             let mut an = var_env.remove(&a).unwrap();
//             an.union_with(var_env.get_mut(&b).unwrap(), |a, b| {
//                 match (a, b) {
//                     (a, None) => a,
//                     (None, b) => b,
//                     _ => unimplemented!(),
//                 }
//             });
//             var_env.insert(a, an);
//         }
//     };

//     for c in constraints {
//         match c {
//             Constraint::Eq(a, b) => {
//                 match a {
//                     Term::TExp(expr) => {
//                         match b {
//                             Term::Var(id) => {}
//                             _ => unimplemented!(),
//                         }
//                     }
//                     _ => unimplemented!(),
//                 }
//             }
//         }
//     }

// // let var_names: HashMap<_, _> = def_env.iter().map(|(k, v)| (v.id, k.to_owned())).collect();

//     // let var_env: HashMap<_, _> = var_env.into_iter().map(|(k, v)| {
//     //     (var_names[&k].to_owned(), v.find().clone_data())
//     // }).collect();
// }

// fn constraint_gen(constraints: &mut Vec<Constraint>, expr: Rc<Expr>) {
//     use self::Constraint::*;

//     match *expr {
//         Expr::Unit => {} //constraints.push(Eq(Term::TExp(expr.clone()), Term::Unit)),
//         Expr::Num(_) => {} // constraints.push(Eq(Term::TExp(expr.clone()), Term::Num)),
//         Expr::Id(id, _) => constraints.push(Eq(Term::TExp(expr.clone()), Term::Var(id))),
//         Expr::App { ref fun, ref args } => {
//             constraints.push(Eq(Term::TExp(fun.clone()),
//                                 Term::Arrow(args.iter().cloned().map(Term::TExp).collect(),
//                                             Box::new(Term::TExp(expr.clone())))));
//             constraint_gen(constraints, fun.clone());
//             for expr in args.iter().cloned() {
//                 constraint_gen(constraints, expr);
//             }
//         }
//         Expr::Lam { name, ref body } => {
//             constraints.push(Eq(Term::TExp(expr.clone()),
//                                 Term::Arrow(vec![Term::Var(name)],
//                                             Box::new(Term::TExp(body.last()
//                                                                     .expect("empty lambda \
//                                                                              body")
//                                                                     .clone())))));
//             for expr in body.iter().cloned() {
//                 constraint_gen(constraints, expr);
//             }
//         }
//     }
// }

// pub fn parse_def(sexp: &Sexp) -> AResult<Def> {
//     debug!("Attempting to parse_def input: {:?}", sexp);

//     match *sexp {
//         Sexp::List(id, span, ref sexps) => {
//             if sexps.len() != 2 {
//                 return Err("Defs must have exactly 2 arguments".into());
//             }

//             let (name, id) = if let Sexp::Atom(id, _, ref s) = sexps[1] {
//                 (s, id)
//             } else {
//                 return Err("First argument of Def must be a name".into());
//             };

// let body = try!(sexp_to_expr(&sexps[0]));

//             Ok(Def::new_private(name.to_owned(), id, body))
//         }
//         _ => Err(unimplemented!()),
//     }
// }

// fn sexp_to_expr(sexp: Sexp) -> AResult<Expr> {
//     match sexp {
//         Sexp::Atom(id, _, s) => {
//             match s.parse() {
//                 Ok(n) => Ok(Expr::Num(n)),
//                 _ => Ok(Expr::Id(id, s)),
//             }
//         }
//         Sexp::List(_, _, mut c) => {
//             if c.is_empty() {
//                 Ok(Expr::Unit)
//             } else {
//                 if c[0].is_atom_of("fn") {
//                     c.remove(0);

//                     if c.len() != 2 {
//                         return Err("Fn must have 2 args".into());
//                     }

//                     let name = if let Sexp::Atom(id, _, _) = c.remove(0) {
//                         id
//                     } else {
//                         return Err("First arg of fn must be a name".into());
//                     };
//                     // let name = try!(c.remove(0)
//                     //                  .extract_atom()
//                     //                  .map_err(|_| {
//                     //                      Box::<Error>::from("First arg of fn must be a name")
//                     //                  }));

//                     Ok(Expr::Lam {
//                         name: name,
//                         body: try!(sexps_to_exprs(c)).into_iter().map(Rc::new).collect(),
//                     })
//                 } else {
//                     Ok(Expr::App {
//                         fun: Rc::new(try!(sexp_to_expr(c.remove(0)))),
//                         args: try!(sexps_to_exprs(c)).into_iter().map(Rc::new).collect(),
//                     })
//                 }
//             }
//         }
//     }
// }

// fn sexps_to_exprs(sexps: Vec<Sexp>) -> AResult<Vec<Expr>> {
//     sexps.into_iter().map(sexp_to_expr).collect()
// }

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

// enum TypeState {
//     Unknown,
//     Equiv(ExprId),
//     Known(TypeId),
// }

struct Def {

}

pub struct DoNode {
    body: Vec<Ast>,
}
pub struct CallNode {
    args: Vec<Ast>,
    fn_id: FnId,
}
pub struct LetNode {
    args: Vec<(VarId, Ast)>,
    body: Vec<Ast>,
}
pub struct FnDef {

}

pub type VarTable<'a> = TieredMap<'a, VarId, Value>;
pub type DoNodeTable = HashMap<DoNodeId, DoNode>;
pub type CallNodeTable = HashMap<CallNodeId, CallNode>;
pub type LetNodeTable = HashMap<LetNodeId, LetNode>;
pub type FnTable = HashMap<FnId, FnDef>;

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

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Unit,
    Num(i64),
    Bool(bool),
    FnPtr(FnId),
}

pub struct ExecUnit<'a> {
    var_env: VarTable<'a>,
    asts: Vec<Ast>,
    do_nodes: DoNodeTable,
    call_nodes: CallNodeTable,
    let_nodes: LetNodeTable,
    fns: FnTable,
}

pub fn analyze(sexps: &[Sexp]) -> AResult<ExecUnit> {
    newtype_id!(ExpId);

    type VarTable = HashMap<String, VarId>;
    type DoNodeTable = HashMap<DoNodeId, DoNode>;
    type CallNodeTable = HashMap<CallNodeId, CallNode>;
    type LetNodeTable = HashMap<LetNodeId, LetNode>;
    type FnTable = HashMap<FnId, FnDef>;
    type FnEnv = HashMap<String, FnId>;


    // "pre-ast"
    enum Exp {
        Unit(ExpId),
        Num(ExpId, i64),
        Bool(ExpId, bool),
        Atom(ExpId, String),
        Fn(ExpId, FnId),
        Do(ExpId, DoNodeId),
        Call(ExpId, CallNodeId),
        Let(ExpId, LetNodeId),
    }

    impl Exp {
        fn id(&self) -> ExpId {
            match *self {
                Exp::Atom(id, ..) |
                Exp::Bool(id, ..) |
                Exp::Call(id, ..) |
                Exp::Do(id, ..) |
                Exp::Fn(id, ..) |
                Exp::Let(id, ..) |
                Exp::Num(id, ..) |
                Exp::Unit(id, ..) => id,
            }
        }
    }

    struct DoNode {
        body: Vec<Exp>,
    }
    struct CallNode {
        args: Vec<Exp>,
        fn_id: FnId,
    }
    impl CallNode {
        fn return_type_id(&self) -> TypeId {
            unimplemented!()
        }
    }
    struct LetNode(Vec<(String, Exp)>, Vec<Exp>);
    struct FnDef;
    impl FnDef {
        fn id(&self) -> FnId {
            unimplemented!()
        }
        fn arg_type_ids(&self) -> &[TypeId] {
            unimplemented!()
        }
    }

    fn sexp_to_exp(sexp: &Sexp,
                   do_nodes: &mut DoNodeTable,
                   call_nodes: &mut CallNodeTable,
                   let_nodes: &mut LetNodeTable,
                   fns: &mut FnTable,
                   fn_env: &mut FnEnv)
                   -> AResult<Exp> {

        use std::ops::Index;
        use std::hash::Hash;

        trait IdTable<K, V> {
            fn get(&self, key: K) -> Option<&V>;
            fn insert_node(&mut self, val: V) -> K;
        }

        impl<K, V> Index<K> for IdTable<K, V> {
            type Output = V;
            fn index(&self, idx: K) -> &V {
                self.get(idx).expect("Key not in table!")
            }
        }

        impl<K, V> IdTable<K, V> for HashMap<K, V>
            where K: Eq + Hash + Copy + From<usize>
        {
            fn get(&self, key: K) -> Option<&V> {
                self.get(&key)
            }

            fn insert_node(&mut self, val: V) -> K {
                let key = self.len().into();
                self.insert(key, val);
                key
            }
        }

        let id = sexp.id().into();

        match *sexp {
            Sexp::Atom(_, _, ref s) => {
                s.parse().map(|n| Exp::Num(id, n)).or_else(|_| Ok(Exp::Atom(id, s.to_owned())))
            }
            Sexp::List(_, _, ref sexps) => {
                match sexps.split_first() {
                    None => Ok(Exp::Unit(id)),
                    Some((&Sexp::List(_, _, ref children), args)) => {
                        Ok(Exp::Call(id,
                                     call_nodes.insert_node(CallNode {
                                         fn_id: unimplemented!(),
                                         args: unimplemented!(),
                                     })))
                    }
                    Some((&Sexp::Atom(_, _, ref callee), args)) => {
                        match &**callee {
                            "let" => {
                                // args = (k v k v k v) <body>
                                // let (params, body) = try!(args.split_first().ok_or_else(|| ));
                                // Ok(Exp::Let(insert_node(LetNode(params, body), let_nodes)))
                                unimplemented!()
                            }
                            "do" => {
                                let body = sexps_to_exps(args,
                                                         do_nodes,
                                                         call_nodes,
                                                         let_nodes,
                                                         fns,
                                                         fn_env)?;
                                let do_id = do_nodes.insert_node(DoNode { body: body });
                                Ok(Exp::Do(id, do_id))
                            }
                            "fn" => {
                                // perform lambda lifting here
                                let fn_def = unimplemented!();
                                let fn_id = fns.insert_node(fn_def);
                                Ok(Exp::Fn(id, fn_id))
                            }
                            callee => {
                                let fn_id = match fn_env.get(callee) {
                                    Some(&fn_id) => fn_id,
                                    _ => {
                                        return Err(format!("Callee not in scope: {}", callee)
                                            .into())
                                    }
                                };
                                let args = sexps_to_exps(args,
                                                         do_nodes,
                                                         call_nodes,
                                                         let_nodes,
                                                         fns,
                                                         fn_env)?;
                                let fn_def = &fns[&fn_id];
                                let call_id = call_nodes.insert_node(CallNode {
                                    fn_id: fn_id,
                                    args: args,
                                });
                                Ok(Exp::Call(id, call_id))
                            }
                        }
                    }
                }
            }
        }
    }

    fn sexps_to_exps(sexps: &[Sexp],
                     do_nodes: &mut DoNodeTable,
                     call_nodes: &mut CallNodeTable,
                     let_nodes: &mut LetNodeTable,
                     fns: &mut FnTable,
                     fn_env: &mut FnEnv)
                     -> AResult<Vec<Exp>> {
        sexps.iter()
            .map(|sexp| sexp_to_exp(sexp, do_nodes, call_nodes, let_nodes, fns, fn_env))
            .collect()
    }

    newtype_id!(VarId);

    fn infer(exps: &[Exp],
             do_nodes: &mut DoNodeTable,
             call_nodes: &mut CallNodeTable,
             let_nodes: &mut LetNodeTable,
             fns: &mut FnTable,
             vars: &mut VarTable)
             -> AResult<Vec<Ast>> {
        use ena::unify::*;

        newtype_id!(TypeKey);

        impl UnifyKey for TypeKey {
            type Value = TypeState;
            fn index(&self) -> u32 {
                self.0 as u32
            }
            fn from_index(u: u32) -> Self {
                (u as usize).into()
            }
            fn tag() -> &'static str {
                stringify!(TypeKey)
            }
        }

        #[derive(Clone, Copy, Debug)]
        pub enum TypeState {
            Unknown,
            Known(TypeId),
        }

        impl UnifyValue for TypeState {
            fn unify_values(&a: &Self, &b: &Self) -> Result<Self, (Self, Self)> {
                match (a, b) {
                    (TypeState::Unknown, b) => Ok(b),
                    (a, TypeState::Unknown) => Ok(a),
                    (TypeState::Known(a_id), TypeState::Known(b_id)) => {
                        if a_id == b_id {
                            Ok(a)
                        } else {
                            Err((a, b))
                        }
                    }
                }
            }
        }

        enum Constraint {
            NodeNode(ExpId, ExpId),
            NodeType(ExpId, TypeId),
            NodeVar(ExpId, VarId),
        }

        #[derive(Copy, Clone)]
        struct Context<'a> {
            do_nodes: &'a DoNodeTable,
            call_nodes: &'a CallNodeTable,
            let_nodes: &'a LetNodeTable,
            fns: &'a FnTable,
        }

        fn gen_constraints(exps: &[Exp], cxt: Context, vars: &mut VarTable) -> Vec<Constraint> {

            fn gen(exp: &Exp,
                   cxt: Context,
                   constraints: &mut Vec<Constraint>,
                   vars: &mut VarTable) {

                const UNIT_TYPEID: TypeId = TypeId(0); // Known(Unit)
                const BOOL_TYPEID: TypeId = TypeId(1); // Known(Bool)
                const NUM_TYPEID: TypeId = TypeId(2); // Known(Num)

                match *exp {
                    Exp::Unit(id) => constraints.push(Constraint::NodeType(id, UNIT_TYPEID)),
                    Exp::Bool(id, _) => constraints.push(Constraint::NodeType(id, BOOL_TYPEID)),
                    Exp::Num(id, _) => constraints.push(Constraint::NodeType(id, NUM_TYPEID)),
                    Exp::Atom(id, ref s) => {
                        constraints.push(Constraint::NodeVar(id, vars.get(s).cloned().unwrap()));
                    }

                    Exp::Do(id, node) => {
                        let node = &cxt.do_nodes[&node];
                        let constraint = match node.body.last() {
                            Some(exp) => Constraint::NodeNode(id, exp.id()),
                            _ => Constraint::NodeType(id, UNIT_TYPEID),
                        };

                        constraints.push(constraint);
                    }
                    Exp::Call(id, node) => {
                        let node = &cxt.call_nodes[&node];
                        let fn_def = &cxt.fns[&node.fn_id];

                        constraints.push(Constraint::NodeType(id, node.return_type_id()));
                        for (arg, &arg_type_id) in node.args.iter().zip(fn_def.arg_type_ids()) {
                            constraints.push(Constraint::NodeType(arg.id(), arg_type_id));
                        }
                    }
                    Exp::Let(id, node) => {
                        let node = &cxt.let_nodes[&node];
                        unimplemented!()
                    }
                    Exp::Fn(id, node) => {
                        //
                        unimplemented!()
                    }
                }
            }

            let mut constraints = vec![];

            for exp in exps {
                gen(exp, cxt, &mut constraints, vars);
            }

            constraints
        }
        // let mut utable = UnificationTable::<TypeKey>::new();
        //
        // let key = utable.new_key(TypeState::Unknown);
        //
        // println!("K: {:?}, V: {:?}", key, utable.probe_value(key));
        //
        // let do_nodes = ();
        //

        // generate constraint set
        let cxt = Context {
            do_nodes: do_nodes,
            call_nodes: call_nodes,
            let_nodes: let_nodes,
            fns: fns,
        };

        let constraints = gen_constraints(exps, cxt, vars);

        unimplemented!()
    }


    let mut do_nodes = DoNodeTable::new();
    let mut call_nodes = CallNodeTable::new();
    let mut let_nodes = LetNodeTable::new();
    let mut fns = FnTable::new();
    let mut fn_env = FnEnv::new();
    let mut vars = VarTable::new();

    let (defs, non_defs) = separate_defs(sexps)?;

    let exps: Vec<_> = try!(non_defs.into_iter()
        .map(|sexp| {
            sexp_to_exp(sexp,
                        &mut do_nodes,
                        &mut call_nodes,
                        &mut let_nodes,
                        &mut fns,
                        &mut fn_env)
        })
        .collect());


    let asts = infer(&*exps,
                     &mut do_nodes,
                     &mut call_nodes,
                     &mut let_nodes,
                     &mut fns,
                     &mut vars)?;

    Ok(unimplemented!())
    // Ok(ExecUnit {
    //     var_env: var_env,
    //     asts: asts,
    //     do_nodes: do_nodes,
    //     call_nodes: call_nodes,
    //     let_nodes: let_nodes,
    //     fns: fns,
    // })
}

fn sexp_to_ast(sexp: &Sexp) -> Ast {
    match *sexp {
        Sexp::Atom(id, span, ref s) => {
            match s.parse() {
                Ok(n) => Ast::Num(n),
                _ => Ast::Var(id.into()),
            }
        }
        Sexp::List(id, span, ref sexps) => unimplemented!(),
    }
}

fn separate_defs(sexps: &[Sexp]) -> AResult<(Vec<&Sexp>, Vec<&Sexp>)> {
    let mut defs = vec![];
    let mut non_defs = vec![];

    for sexp in sexps {
        match *sexp {
            Sexp::List(id, span, ref s) => {
                if !s.is_empty() && s[0].is_atom_of("def") {
                    defs.push(sexp);
                } else {
                    non_defs.push(sexp);
                }
            }
            _ => non_defs.push(sexp),
        }
    }

    Ok((defs, non_defs))
}

pub fn interpret(program: &ExecUnit) -> String {

    fn print_value(val: &Value) -> String {
        match *val {
            Value::Unit => String::from("()"),
            Value::Bool(v) => format!("{}", v),
            Value::Num(v) => format!("{}", v),
            Value::FnPtr(v) => format!("<fn {}>", v.0),
        }
    }

    #[derive(Clone, Copy)]
    struct Context<'a> {
        do_nodes: &'a DoNodeTable,
        call_nodes: &'a CallNodeTable,
        let_nodes: &'a LetNodeTable,
        fns: &'a FnTable,
    }

    fn run_do(cxt: Context, vars: &VarTable, body: &[Ast]) -> Value {
        let (last, rest) = body.split_last().unwrap();

        for ast in rest {
            interpret_(cxt, vars, ast);
        }

        interpret_(cxt, vars, last)
    }

    fn run_let(cxt: Context, vars: &VarTable, node: &LetNode) -> Value {

        let mut vars = vars.new_scope();

        for &(label, arg) in &node.args {
            let val = interpret_(cxt, &vars, &arg);
            vars.insert(label, val);
        }

        run_do(cxt, &vars, &*node.body)
    }

    fn run_call(cxt: Context, vars: &VarTable, node: &CallNode) -> Value {
        unimplemented!()
    }

    fn interpret_<'a>(cxt: Context, vars: &VarTable<'a>, ast: &'a Ast) -> Value {
        match *ast {
            Ast::Unit => Value::Unit,
            Ast::Num(val) => Value::Num(val),
            Ast::Bool(val) => Value::Bool(val),
            Ast::FnPtr(fn_id) => Value::FnPtr(fn_id),
            Ast::Var(ref var_id) => vars[var_id],
            Ast::Do(id) => run_do(cxt, vars, &*cxt.do_nodes[&id].body),
            Ast::Call(id) => run_call(cxt, vars, &cxt.call_nodes[&id]),
            Ast::Let(id) => run_let(cxt, vars, &cxt.let_nodes[&id]),
        }
    }

    let cxt = Context {
        do_nodes: &program.do_nodes,
        call_nodes: &program.call_nodes,
        let_nodes: &program.let_nodes,
        fns: &program.fns,
    };

    let asts = &*program.asts;

    debug!("Interpret: {:#?}", asts);

    if asts.is_empty() {
        return String::from("");
    }

    let result = run_do(cxt, &program.var_env, asts);

    print_value(&result)
}


#[cfg(test)]
mod tests {
    extern crate slog_envlogger;

    use read::{self, InputStream, Sexp};

    pub fn read(text: &str) -> Vec<Sexp> {
        let mut env = Default::default();
        let mut stream = InputStream::new(text.to_string());

        read::read(&mut env, &mut stream).unwrap()
    }

    #[test]
    fn accept() {
        use super::TypeId;
        use super::TypeAcceptor::*;

        assert!(Void.accepts(&[]));
        assert!(!Void.accepts(&[TypeId(0)]));
        assert!(Type(TypeId(5)).accepts(&[TypeId(5)]));
        assert!(!Type(TypeId(5)).accepts(&[TypeId(0)]));
        assert!(!Type(TypeId(5)).accepts(&[TypeId(5), TypeId(0)]));
    }

    // #[test]
    // fn parse() {
    //     use super::*;

    // let _ = slog_envlogger::init();

    //     let input = "(def a 1) (def b 2) (def c b) (def d c) a b";
    //     let sexps = lang::read(input);

    //     panic!("{:#?}", parse_module(Some("test_mod"), sexps));
    // }

    #[test]
    fn test_interpret() {
        use super::*;

        let _ = slog_envlogger::init();

        let mut program = ExecUnit {
            asts: vec![Ast::Let(0.into())],
            do_nodes: DoNodeTable::new(),
            call_nodes: CallNodeTable::new(),
            let_nodes: LetNodeTable::new(),
            var_env: VarTable::new(),
            fns: FnTable::new(),
        };

        program.let_nodes.insert(0.into(),
                                 LetNode {
                                     args: vec![(0.into(), Ast::Num(1))],
                                     body: vec![Ast::Var(0.into())],
                                 });

        let result = interpret(&program);

        assert_eq!("1", result);
    }

    #[ignore]
    #[test]
    fn test_interpret_full() {
        use super::*;

        let _ = slog_envlogger::init();

        let input = "(def inc (fn (a) (+ a 1)))

                     (inc 4)";

        let sexps = read(input);

        let mut program = analyze(&*sexps).unwrap();

        program.let_nodes.insert(0.into(),
                                 LetNode {
                                     args: vec![(0.into(), Ast::Num(1))],
                                     body: vec![Ast::Var(0.into())],
                                 });

        let result = interpret(&program);

        assert_eq!("5", result);
    }
}
