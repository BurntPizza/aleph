
use itertools::*;
use petgraph;
use disjoint_sets::UnionFindNode;

use std::rc::Rc;
use std::cell::RefCell;
use std::error::Error;
use std::convert::Into;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

use read::{Span, Sexp};

pub type Table<K, V> = HashMap<K, V>;
pub type Result<T> = ::std::result::Result<T, Box<Error>>;

macro_rules! def_id {
    ($name:ident, $ty:ty) => {
        #[derive(PartialEq, PartialOrd, Eq, Hash, Copy, Clone, Debug)]
        pub struct $name($ty);

        impl ::std::convert::From<$ty> for $name {
            fn from(val: $ty) -> Self {
                $name(val)
            }
        }

        impl ::std::ops::Deref for $name {
            type Target = $ty;
            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
    }
}

def_id!(BindingId, usize);
def_id!(ScopeId, u32);

pub struct Def(Span, String, Sexp);

fn new_binding_id() -> BindingId {
    use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};
    static COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;

    COUNTER.fetch_add(1, Ordering::SeqCst).into()
}

#[derive(Debug)]
pub struct Binding {
    name: String,
    span: Span,
    ast_id: usize,
    id: BindingId,
}

#[derive(Hash, Eq, Debug, Clone, PartialEq)]
pub enum Type {
    I64,
    Bool,
    Unit,
    Fn(Vec<Type>, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f,
               "{}",
               match *self {
                   Type::Bool => "Bool",
                   Type::I64 => "I64",
                   Type::Unit => "()",
                   _ => unimplemented!(),
               })
    }
}

#[derive(Debug, Default)]
pub struct Scope {
    parent: Option<Rc<Scope>>,
    by_name: RefCell<HashMap<String, Rc<Binding>>>,
    by_id: RefCell<HashMap<BindingId, Rc<Binding>>>,
}

impl Scope {
    fn child_of(parent: Rc<Scope>) -> Rc<Self> {
        Rc::new(Scope { parent: Some(parent.clone()), ..Default::default() })
    }

    fn add_binding(&self, name: String, span: Span, ast_id: usize) -> BindingId {
        let id = new_binding_id();
        let binding = Rc::new(Binding {
            name: name.clone(),
            span: span,
            ast_id: ast_id,
            id: id,
        });

        self.by_name.borrow_mut().insert(name, binding.clone());
        self.by_id.borrow_mut().insert(id, binding);

        id
    }

    fn get(&self, id: BindingId) -> Rc<Binding> {
        self.by_id.borrow()[&id].clone()
    }

    fn lookup(&self, name: &str) -> Option<Rc<Binding>> {
        self.by_name
            .borrow()
            .get(name)
            .cloned()
            .or_else(|| self.parent.clone().and_then(|parent| parent.lookup(name)))
    }
}

#[derive(Clone, Debug)]
pub enum Ast {
    EmptyList(usize, Rc<Scope>, Span),

    I64Literal(usize, Rc<Scope>, Span, i64),
    BoolLiteral(usize, Rc<Scope>, Span, bool),

    Atom(usize, Rc<Scope>, Span, String),
    Inv(usize, Rc<Scope>, Span, Box<Ast>, Vec<Ast>),

    Do(usize, Rc<Scope>, Span, Vec<Ast>),
    // params, body
    Fn(usize, Rc<Scope>, Span, Vec<(usize, Rc<Binding>)>, Vec<Ast>),
    // (params, values), body
    Let(usize, Rc<Scope>, Span, Vec<(usize, String, Ast)>, Vec<Ast>),
    // condition, then-expr, else-expr
    If(usize, Rc<Scope>, Span, Box<Ast>, Box<Ast>, Box<Ast>),
}

impl Ast {
    fn id(&self) -> usize {
        match *self {
            Ast::EmptyList(id, _, _) => id,
            Ast::I64Literal(id, _, _, _) => id,
            Ast::BoolLiteral(id, _, _, _) => id,
            Ast::Atom(id, _, _, _) => id,
            Ast::Inv(id, _, _, _, _) => id,
            Ast::Do(id, _, _, _) => id,
            Ast::Fn(id, _, _, _, _) => id,
            Ast::Let(id, _, _, _, _) => id,
            Ast::If(id, _, _, _, _, _) => id,
        }
    }
}

pub fn read(text: &str) -> Vec<Sexp> {
    use read::{self, InputStream};

    let mut env = Default::default();
    let mut stream = InputStream::new(text.to_string());

    read::read(&mut env, &mut stream).unwrap()
}

pub fn separate_defs(sexps: Vec<Sexp>) -> Result<(Vec<Def>, Vec<Sexp>)> {
    let (defs_, non_defs): (Vec<_>, Vec<_>) = sexps.into_iter().partition_map(|sexp| {
        match sexp {
            Sexp::List(id, span, mut sexps) => {
                if if let Some(&Sexp::Atom(_, _, ref name)) = sexps.first() {
                    name == "def"
                } else {
                    false
                } {
                    assert_eq!(sexps.len(), 3);
                    let _def_symbol = sexps.remove(0);
                    let name = sexps.remove(0);
                    let rhs = sexps.remove(0);
                    return Partition::Left((span, name, rhs));
                } else {
                    Partition::Right(Sexp::List(id, span, sexps))
                }
            }
            _ => Partition::Right(sexp),
        }
    });

    let mut defs = Vec::with_capacity(defs_.len());

    for (span, name_sexp, rhs_sexp) in defs_ {
        let name = match name_sexp {
            Sexp::Atom(_, _, string) => string,
            _ => return Err(format!("First arg of `def` must be a symbol: {:?}", name_sexp).into()),
        };

        defs.push(Def(span, name, rhs_sexp));
    }

    Ok((defs, non_defs))
}

pub fn process_defs(global_scope: Rc<Scope>, defs: Vec<Def>) -> (Env, Table<BindingId, TypedAst>) {
    use petgraph::Graph;

    fn get_deps(ast: &Ast) -> Vec<BindingId> {
        match *ast {
            Ast::Atom(_, ref scope, _, ref string) => {
                vec![scope.lookup(&**string).expect("oops").id]
            }
            Ast::BoolLiteral(..) |
            Ast::EmptyList(..) |
            Ast::I64Literal(..) => vec![],
            _ => panic!("unimpl: {:?}", ast),
        }
    }

    let mut env = Env::new();
    let mut binding_id_to_node_idx = Table::new();
    let mut def_graph = Graph::<_, ()>::new();

    for Def(span, name, sexp) in defs {
        let ast = sexp_to_ast(global_scope.clone(), sexp).unwrap();
        let (id, ty) = match ast {
            Ast::I64Literal(id, _, _, _) => (id, TypeTerm::Known(Type::I64)),
            Ast::BoolLiteral(id, _, _, _) => (id, TypeTerm::Known(Type::Bool)),
            Ast::Atom(id, _, _, _) => (id, TypeTerm::Unknown),
            _ => unimplemented!(),
        };

        let binding_id = global_scope.add_binding(name.clone(), span, id);
        let node_idx = def_graph.add_node((binding_id, ast));

        binding_id_to_node_idx.insert(binding_id, node_idx);
        env.insert(id, Var::new(ty));
    }

    for idx in def_graph.node_indices() {
        for id in get_deps(&def_graph[idx].1) {
            let dep_idx = binding_id_to_node_idx[&id];
            def_graph.add_edge(idx, dep_idx, ());
        }
    }

    for scc in petgraph::algo::scc(&def_graph) {
        if scc.len() > 1 {
            panic!("Cyclic dependency: {:?}",
                   scc.into_iter()
                      .map(|idx| global_scope.get(def_graph[idx].0).name.clone())
                      .collect_vec());
        }
    }

    let (nodes, _) = def_graph.into_nodes_edges();
    let (ids, asts): (Vec<_>, _) = nodes.into_iter().map(|node| node.weight).unzip();
    let asts = type_infer(&mut env, asts);

    (env, ids.into_iter().zip(asts).collect())
}

pub fn sexp_to_ast(scope: Rc<Scope>, sexp: Sexp) -> Result<Ast> {
    match sexp {
        Sexp::Atom(id, span, string) => {
            match string.parse::<i64>() {
                Ok(val) => Ok(Ast::I64Literal(id, scope.clone(), span, val)),
                _ => {
                    if string == "true" {
                        Ok(Ast::BoolLiteral(id, scope.clone(), span, true))
                    } else if string == "false" {
                        Ok(Ast::BoolLiteral(id, scope.clone(), span, false))
                    } else {
                        Ok(Ast::Atom(id, scope.clone(), span, string))
                    }
                }
            }
        }
        Sexp::List(id, span, mut sexps) => {
            match sexps.len() {
                0 => Ok(Ast::EmptyList(id, scope.clone(), span)),
                _ => {
                    let first = sexps.remove(0);

                    match first {
                        Sexp::Atom(_, _, ref s) if s == "ns" => {
                            // set current ns
                            // Ok(None)
                            unimplemented!()
                        }
                        Sexp::Atom(_, _, ref s) if s == "use" => {
                            // import names
                            // Ok(None)
                            unimplemented!()
                        }
                        Sexp::Atom(_, _, ref s) if s == "do" => {
                            assert!(sexps.len() > 0);

                            let body = try!(sexps.into_iter()
                                                 .map(|sexp| sexp_to_ast(scope.clone(), sexp))
                                                 .fold_results(vec![], vec_collector));

                            Ok(Ast::Do(id, scope.clone(), span, body))
                        }
                        Sexp::Atom(_, _, ref s) if s == "if" => {
                            assert_eq!(sexps.len(), 3);

                            let cond_expr = try!(sexp_to_ast(scope.clone(), sexps.remove(0)));
                            let then_expr = try!(sexp_to_ast(scope.clone(), sexps.remove(0)));
                            let else_expr = try!(sexp_to_ast(scope.clone(), sexps.remove(0)));

                            Ok(Ast::If(id,
                                       scope.clone(),
                                       span,
                                       Box::new(cond_expr),
                                       Box::new(then_expr),
                                       Box::new(else_expr)))
                        }
                        Sexp::Atom(_, _, ref s) if s == "let" => {
                            assert!(sexps.len() >= 2);

                            let inner_scope = Scope::child_of(scope.clone());
                            let binding_list_src = match sexps.remove(0) {
                                Sexp::List(_, _, sexps) => {
                                    assert!(sexps.len() % 2 == 0);

                                    sexps.into_iter()
                                         .chunks_lazy(2)
                                         .into_iter()
                                         .map(|mut chunk| {
                                             let (param_id, param_span, param_name) = {
                                                 match chunk.next().unwrap() {
                                                     Sexp::Atom(id, span, string) => {
                                                         (id, span, string)
                                                     }
                                                     _ => panic!("param must be Atom"),
                                                 }
                                             };
                                             let value = chunk.next().unwrap();

                                             (param_id, param_span, param_name, value)
                                         })
                                         .collect_vec()
                                }
                                _ => panic!("First arg of `let` must be a list"),
                            };

                            let mut binding_list = vec![];

                            for (id, span, name, value_sexp) in binding_list_src {
                                let value = try!(sexp_to_ast(inner_scope.clone(), value_sexp));
                                scope.add_binding(name.clone(), span, id);
                                binding_list.push((id, name, value));
                            }

                            let body_asts = try!(sexps.into_iter()
                                                      .map(|sexp| {
                                                          sexp_to_ast(inner_scope.clone(), sexp)
                                                      })
                                                      .fold_results(vec![], vec_collector));

                            Ok(Ast::Let(id, scope.clone(), span, binding_list, body_asts))
                        }
                        Sexp::Atom(_, _, ref s) if s == "fn" => {
                            assert!(sexps.len() >= 2);

                            let inner_scope = Scope::child_of(scope.clone());
                            let params_list = match sexps.remove(0) {
                                Sexp::List(_, _, sexps) => {
                                    sexps.into_iter()
                                         .map(|sexp| match sexp {
                                             Sexp::Atom(id, span, string) => {
                                                 let binding_id = inner_scope.add_binding(string,
                                                                                          span,
                                                                                          id);

                                                 (id, inner_scope.get(binding_id))
                                             }
                                             _ => panic!("param must be Atom"),
                                         })
                                         .collect_vec()
                                }
                                _ => panic!("First arg of `fn` must be a list"),
                            };

                            let body_asts = try!(sexps.into_iter()
                                                      .map(|sexp| {
                                                          sexp_to_ast(inner_scope.clone(), sexp)
                                                      })
                                                      .fold_results(vec![], vec_collector));

                            Ok(Ast::Fn(id, scope.clone(), span, params_list, body_asts))
                        }
                        Sexp::Atom(_, _, ref s) if s == "def" => unreachable!(),
                        Sexp::Atom(_, _, ref s) if s == "defreader" => unreachable!(),
                        _ => {
                            Ok(Ast::Inv(id,
                                        scope.clone(),
                                        span,
                                        Box::new(try!(sexp_to_ast(scope.clone(), first))),
                                        try!(sexps.into_iter()
                                                  .map(|sexp| sexp_to_ast(scope.clone(), sexp))
                                                  .fold_results(vec![], vec_collector))))
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum TypedAst {
    EmptyList,
    I64Literal(i64),
    BoolLiteral(bool),

    Atom(Type, BindingId),
    Inv(Type, Box<TypedAst>, Vec<TypedAst>),

    Do(Type, Vec<TypedAst>),
    // params, body
    Fn(Type, Vec<Rc<Binding>>, Vec<TypedAst>),
    // (params, values), body
    Let(Type, Vec<(BindingId, TypedAst)>, Vec<TypedAst>),
    // condition, then-expr, else-expr
    If(Type, Box<TypedAst>, Box<TypedAst>, Box<TypedAst>),
}

#[derive(Debug, Clone, PartialEq)]
enum TypeTerm {
    Unknown,
    Known(Type),
    FnInProgress(Vec<usize>, usize),
}

type Var = UnionFindNode<TypeTerm>;

#[derive(Debug)]
pub struct Env {
    id_to_var: Table<usize, Var>,
    string_to_id: Table<String, usize>,
}

impl Env {
    fn new() -> Self {
        Env {
            id_to_var: Table::new(),
            string_to_id: Table::new(),
        }
    }

    fn get(&self, id: usize) -> &Var {
        &self.id_to_var[&id]
    }

    fn insert(&mut self, id: usize, var: Var) {
        self.id_to_var.insert(id, var);
    }

    fn remove(&mut self, id: usize) -> Var {
        self.id_to_var.remove(&id).unwrap()
    }

    fn map_values<F>(&mut self, a: usize, b: usize, f: F)
        where F: Fn(Var, Var) -> (Var, Var)
    {
        assert!(a != b);
        let a_var = self.remove(a);
        let b_var = self.remove(b);
        let (a_var, b_var) = f(a_var, b_var);
        self.insert(a, a_var);
        self.insert(b, b_var);
    }

    fn lookup_type(&self, id: usize) -> Option<Type> {
        fn resolve(env: &Env, id: usize, v: &Var) -> Option<Type> {
            v.with_data(|term| {
                let ty = match *term {
                    TypeTerm::Known(ref ty) => Some(ty.clone()),
                    TypeTerm::FnInProgress(ref args, ret) => {
                        let args = args.iter()
                                       .map(|&arg| {
                                           let arg = env.get(arg);
                                           resolve(env, id, arg).unwrap()
                                       })
                                       .collect();

                        let ret = env.get(ret);
                        let ret = resolve(env, id, ret).unwrap();
                        Some(Type::Fn(args, Box::new(ret)))
                    }
                    TypeTerm::Unknown => None,
                };

                if ty.is_some() {
                    *term = TypeTerm::Known(ty.clone().unwrap());
                }

                ty
            })
        }

        resolve(self, id, self.get(id))
    }
}

pub fn type_infer(env: &mut Env, asts: Vec<Ast>) -> Vec<TypedAst> {

    fn fresh() -> Var {
        UnionFindNode::new(TypeTerm::Unknown)
    }

    fn known(ty: Type) -> Var {
        UnionFindNode::new(TypeTerm::Known(ty))
    }

    fn inner_unif(a: TypeTerm, b: TypeTerm) -> TypeTerm {
        match (a, b) {
            (TypeTerm::Unknown, b) => b,
            (a, TypeTerm::Unknown) => a,
            (TypeTerm::FnInProgress(_arg_types, _ret_type), TypeTerm::Known(_b)) => unimplemented!(),
            (TypeTerm::Known(_a), TypeTerm::FnInProgress(_arg_types, _ret_type)) => unimplemented!(),
            (a, b) => {
                if a != b {
                    panic!("Mismatched types: {:?} and {:?}", a, b);
                }
                a
            }
        }
    }

    fn unify(env: &mut Env, a: usize, b: usize) {
        env.map_values(a, b, |mut av, mut bv| {
            av.union_with(&mut bv, inner_unif);
            (av, bv)
        });
    }

    enum WalkId {
        Simple(usize),
        Fn(Vec<usize>, usize),
    }

    fn walk(env: &mut Env, ast: &Ast) -> WalkId {
        match *ast {
            Ast::Atom(id, ref scope, _span, ref string) => {
                match scope.lookup(&**string) {
                    Some(binding) => {
                        let term = env.lookup_type(binding.ast_id).map(known).unwrap_or_else(fresh);
                        env.insert(binding.ast_id, term);
                        env.insert(id, fresh());
                        unify(env, binding.ast_id, id);
                    }
                    _ => {
                        env.insert(id, fresh());
                    }
                }

                WalkId::Simple(id)
            }
            Ast::BoolLiteral(id, ref _scope, _span, _val) => {
                env.insert(id, known(Type::Bool));
                WalkId::Simple(id)
            }
            Ast::Do(id, ref _scope, _span, ref children) => {
                let (last, rest) = children.split_last().unwrap();

                for ast in rest {
                    walk(env, ast);
                }

                walk(env, last);

                env.insert(id, fresh());
                unify(env, id, last.id());
                WalkId::Simple(id)
            }
            Ast::EmptyList(id, ref _scope, _span) => {
                env.insert(id, known(Type::Unit));
                WalkId::Simple(id)
            }
            Ast::Fn(id, ref _scope, _span, ref params, ref body) => {
                let param_ids = params.into_iter().map(|&(id, _)| id).collect_vec();

                for &id in &*param_ids {
                    env.insert(id, fresh());
                }

                let (last, rest) = body.split_last().unwrap();

                for ast in rest {
                    walk(env, ast);
                }

                walk(env, last);

                env.insert(id,
                           Var::new(TypeTerm::FnInProgress(param_ids.clone(), last.id())));

                WalkId::Fn(param_ids, last.id())
            }
            Ast::I64Literal(id, ref _scope, _span, _val) => {
                env.insert(id, known(Type::I64));
                WalkId::Simple(id)
            }
            Ast::If(id, ref _scope, _span, ref cond_ast, ref then_ast, ref else_ast) => {
                walk(env, cond_ast);

                if env.lookup_type(cond_ast.id()).unwrap() != Type::Bool {
                    panic!("Condition of `if` expression must be of type Bool");
                }

                walk(env, then_ast);
                walk(env, else_ast);

                if env.lookup_type(then_ast.id()) != env.lookup_type(else_ast.id()) {
                    panic!("Branches of `if` expression must have same type");
                }

                env.insert(id, fresh());
                unify(env, id, then_ast.id());
                unify(env, id, else_ast.id());
                WalkId::Simple(id)
            }
            Ast::Inv(id, ref _scope, _span, ref callee, ref args) => {
                let (arg_ids, ret_id) = match walk(env, callee) {
                    WalkId::Fn(arg_ids, ret_id) => (arg_ids, ret_id),
                    _ => panic!("Invocation of non-fn: {:?}", callee),
                };

                for (arg, arg_receptor_id) in args.into_iter().zip(arg_ids.into_iter()) {
                    walk(env, arg);
                    unify(env, arg.id(), arg_receptor_id);
                }

                env.insert(id, fresh());
                unify(env, id, ret_id);
                WalkId::Simple(id)
            }
            Ast::Let(id, ref _scope, _span, ref bindings, ref body) => {
                for &(id, ref _name, ref ast) in bindings {
                    walk(env, ast);

                    env.insert(id, fresh());
                    unify(env, id, ast.id());
                }

                let (last, rest) = body.split_last().unwrap();

                for ast in rest {
                    walk(env, ast);
                }

                walk(env, last);

                env.insert(id, fresh());
                unify(env, id, last.id());
                WalkId::Simple(id)
            }
        }
    }

    fn to_typed(env: &Env, ast: Ast) -> TypedAst {
        match ast {
            Ast::Atom(id, scope, _, string) => {
                TypedAst::Atom(env.lookup_type(id).unwrap(),
                               scope.lookup(&*string).unwrap().id)
            }
            Ast::BoolLiteral(_, _, _, val) => TypedAst::BoolLiteral(val),
            Ast::Do(id, _, _, children) => {
                TypedAst::Do(env.lookup_type(id).unwrap(),
                             children.into_iter().map(|ast| to_typed(env, ast)).collect())
            }
            Ast::EmptyList(..) => TypedAst::EmptyList,
            Ast::Fn(id, _scope, _span, params, body) => {
                TypedAst::Fn(env.lookup_type(id).unwrap(),
                             params.into_iter()
                                   .map(|(_, binding)| binding)
                                   .collect(),
                             body.into_iter().map(|ast| to_typed(env, ast)).collect())
            }
            Ast::I64Literal(_, _, _, val) => TypedAst::I64Literal(val),
            Ast::If(id, _scope, _span, cond_ast, then_ast, else_ast) => {
                let cond_ast = Box::new(to_typed(env, *cond_ast));
                let then_ast = Box::new(to_typed(env, *then_ast));
                let else_ast = Box::new(to_typed(env, *else_ast));

                TypedAst::If(env.lookup_type(id).unwrap(), cond_ast, then_ast, else_ast)
            }
            Ast::Inv(id, _scope, _span, callee, args) => {
                TypedAst::Inv(env.lookup_type(id).unwrap(),
                              Box::new(to_typed(env, *callee)),
                              args.into_iter().map(|arg| to_typed(env, arg)).collect())
            }
            Ast::Let(id, scope, _, bindings, body) => {
                TypedAst::Let(env.lookup_type(id).unwrap(),
                              bindings.into_iter()
                                      .map(|(_, string, ast)| {
                                          (scope.lookup(&*string).unwrap().id, to_typed(env, ast))
                                      })
                                      .collect(),
                              body.into_iter().map(|ast| to_typed(env, ast)).collect())
            }
        }
    }

    for ast in &asts {
        walk(env, ast);
    }

    asts.into_iter().map(|ast| to_typed(env, ast)).collect()
}

pub fn interpret(consts: &Table<BindingId, TypedAst>, asts: &[TypedAst]) -> String {
    use std::collections::VecDeque;

    #[derive(Clone, Debug)]
    enum Value<'a> {
        Unit,
        I64(i64),
        Bool(bool),
        Fn(&'a [Rc<Binding>], &'a [TypedAst]),
    }

    impl<'a> Display for Value<'a> {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            match *self {
                Value::Unit => write!(f, "()"),
                Value::I64(val) => write!(f, "{}", val),
                Value::Bool(val) => write!(f, "{}", val),
                Value::Fn(..) => write!(f, "<fn>"),
            }
        }
    }

    fn init_env(consts: &Table<BindingId, TypedAst>) -> Table<BindingId, Value> {
        let mut env = Table::new();
        let mut open_set = consts.into_iter().collect::<VecDeque<_>>();

        while let Some((id, elem)) = open_set.pop_front() {
            match *elem {
                TypedAst::Atom(_, inner_id) => {
                    if env.contains_key(&inner_id) {
                        let val = interpret_(&env, elem);
                        env.insert(*id, val);
                    } else {
                        open_set.push_back((id, elem));
                    }
                }
                _ => {
                    let val = interpret_(&env, elem);
                    env.insert(*id, val);
                }
            }
        }

        env
    }

    fn run_do<'a>(env: &Table<BindingId, Value<'a>>, body: &'a [TypedAst]) -> Value<'a> {
        let (last, rest) = body.split_last().unwrap();

        for ast in rest {
            interpret_(&env, ast);
        }

        interpret_(&env, last)
    }

    fn interpret_<'a>(env: &Table<BindingId, Value<'a>>, ast: &'a TypedAst) -> Value<'a> {
        match *ast {
            TypedAst::Atom(_, id) => env[&id].clone(),
            TypedAst::BoolLiteral(val) => Value::Bool(val),
            TypedAst::Do(_, ref body) => run_do(env, &**body),
            TypedAst::EmptyList => Value::Unit,
            TypedAst::Fn(_, ref param_bindings, ref body) => Value::Fn(&**param_bindings, &**body),
            TypedAst::I64Literal(val) => Value::I64(val),
            TypedAst::If(_, ref cond_ast, ref then_ast, ref else_ast) => {
                match interpret_(env, &*cond_ast) {
                    Value::Bool(true) => interpret_(env, &*then_ast),
                    Value::Bool(false) => interpret_(env, &*else_ast),
                    _ => unreachable!(),
                }
            }
            TypedAst::Inv(_, ref callee, ref args) => {
                match interpret_(env, callee) {
                    Value::Fn(param_bindings, body) => {
                        let mut env = env.clone();

                        for (binding, arg) in param_bindings.into_iter().zip(args.into_iter()) {
                            let id = binding.id;
                            let val = interpret_(&env, arg);
                            env.insert(id, val);
                        }

                        run_do(&env, &*body)
                    }
                    _ => panic!(),
                }
            }
            TypedAst::Let(_, ref bindings, ref body) => {
                let mut env = env.clone();

                for &(id, ref ast) in bindings {
                    let val = interpret_(&env, &ast);
                    env.insert(id, val);
                }

                run_do(&env, &*body)
            }
        }
    }

    let env = init_env(consts);
    let result = run_do(&env, asts);
    format!("{}", result)
}

fn vec_collector<T>(mut b: Vec<T>, a: T) -> Vec<T> {
    b.push(a);
    b
}

#[cfg(test)]
mod test {
    use itertools::*;
    use std::rc::Rc;

    use lang;

    macro_rules! test1 {
        ($name:ident, $input:expr, $expected:expr) => {
            test1!($name, $input, $expected,);
        };
        ($name:ident, $input:expr, $expected:expr, $($extra:meta),*) => {
            #[test]
            $(#[$extra])*
            fn $name() {                
                let input: &'static str = $input;
                let expected: &'static str = $expected;
                
                let sexps = lang::read(input);
                let (defs, sexps) = lang::separate_defs(sexps).unwrap();
                let global_scope = Rc::new(lang::Scope::default());
                let (mut type_env, consts) = lang::process_defs(global_scope.clone(), defs);
                let asts = sexps.into_iter()
                    .map(|sexp| lang::sexp_to_ast(global_scope.clone(), sexp))
                    .fold_results(vec![], lang::vec_collector)
                    .unwrap();

                let typed_asts = lang::type_infer(&mut type_env, asts);

                println!("{:#?}", type_env);
                println!("{:#?}", typed_asts);

                let result = lang::interpret(&consts, &*typed_asts);
                
                assert_eq!(expected, result);
            }
        }
    }

    test1!(eval_i64, "10", "10");
    test1!(def_i64, "(def x 10) x", "10");
    test1!(forward_def_i64, "x (def x 10)", "10");
    test1!(eval_boolean_t, "true", "true");
    test1!(eval_boolean_f, "false", "false");
    test1!(eval_let, "(let (a 4) a)", "4");
    test1!(alias_i64, "(def x 10) (def y x) y", "10");
    test1!(forward_alias_i64, "y (def y x) (def x 10)", "10");
    test1!(let_alias, "(def a 3) (let (a 4) a)", "4");
    test1!(eval_if_t, "(if true 0 1)", "0");
    test1!(eval_if_f, "(if false 0 1)", "1");
    test1!(eval_if_def, "(def a false) (if a 0 1)", "1");
    test1!(eval_if_let, "(let (a false) (if a 0 1))", "1");
    test1!(eval_if_in_if, "(if (if true false true) 0 1)", "1");
    test1!(fn_eval, "((fn (a) a) 1)", "1");
    test1!(eval_do, "(do 1 2 ())", "()");

    test1!(non_bool_if_cond,
           "(if 0 0 1)",
           "",
           should_panic(expected = "Condition of `if` expression must be of type Bool"));

    test1!(if_branch_mismatch,
           "(if true 1 false)",
           "",
           should_panic(expected = "Branches of `if` expression must have same type"));

    test1!(cyclic_dep,
           "(def x y) (def y z) (def z x)",
           "",
           should_panic(expected = "Cyclic dependency: [\"x\", \"y\", \"z\"]"));
}
