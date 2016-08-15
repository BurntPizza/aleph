
use itertools::*;

use std::rc::Rc;
use std::error::Error;
use std::convert::Into;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{self, Display, Debug, Formatter};

use read::{Span, Sexp, InputStream};
use print_table;


// (syntactic) Forms:
// statement
// Expr vs Value
// Obj vs Ref

// Special forms:
// defreader  ??
// ns
// use
// def
// macro
// fn
//
// let
// if
// match? or is this a complicated macro? (maybe easier to impl as a special?)

// builtin macros/instrinsics:
// do
// defn
// defmacro
// cond

// '1' expression producing a value (ref to obj) (constant)
// '(+ 1 3)' expression producing a value (ref to obj) (constant)
// '(+ 1 a)' expression producing a value (ref to obj)
// '+' expr -> ref to obj (constant)
// '(fn [...] ...)' expr -> ref to obj (constant)
// '(def x (vec 1 2 3))' statement: modifies env
// '(vec 1 2 3)' expr -> ref to obj (constant)
// '(vec a b 2)' expr -> ref to obj (constant only if a, b are constant)
// 'x' expr -> ref to obj (constant if x is a non-local binding)

pub type Table<K, V> = HashMap<K, V>;
pub type Result<T> = ::std::result::Result<T, Box<Error>>;

const SPECIALS: [&'static str; 9] = ["let",
                                     "if",
                                     "fn",
                                     "do",
                                     "def",
                                     "ns",
                                     "use",
                                     "macro",
                                     "defreader"];

const DIRECTIVES: [&'static str; 4] = ["def", "ns", "use", "defreader"];

pub fn is_special<T>(symbol: T) -> bool
    where T: AsRef<str>
{
    SPECIALS.contains(&symbol.as_ref())
}

fn is_directive<T>(symbol: T) -> bool
    where T: AsRef<str>
{
    DIRECTIVES.contains(&symbol.as_ref())
}
pub struct FnDef {
    ast: Vec<Ast>,
    id: u32,
}

impl FnDef {
    pub fn id(&self) -> u32 {
        self.id
    }

    pub fn ast(&self) -> &[Ast] {
        &*self.ast
    }
}

//         for &name in &SPECIALS {
//             env.insert(name,
//                        // AtomKind::Macro,
//                        Binding::ExternalConst,
//                        root_scope_id,
//                        invalid_source_pos);
//         }

//         env.insert("true",
//                    Binding::Const(ConstType::Bool(true)),
//                    root_scope_id,
//                    invalid_source_pos);
//         env.insert("false",
//                    Binding::Const(ConstType::Bool(false)),
//                    root_scope_id,
//                    invalid_source_pos);

// env

#[derive(Clone, Debug)]
pub enum ConstType {
    Unit,
    I64(i64),
    Bool(bool),
}

// (def x 10) installs 'x' into current namespace (TopLevel)
// (use hello/x) installs 'x' into current namespace (Module)
// (use hello/y) '' 'y' ''
// (use hello) installs (lazily) 'hello/x' and 'hello/y' into current namespace (Module)
// (let [x 10] x) 'x' is Local in it's scope (no associated namespace)
// #[derive(Clone, Debug)]
// pub enum Binding {
//     Const(ConstType),
//     ExternalConst,
// }
// TopLevel(ConstKind), // member current of namespace
// Module(ConstKind), // member of external namespace
// Local(AtomKind), // not member of namespace (e.g. function args)

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

pub struct MacroEnv;

pub struct Def(Span, String, Sexp);

use std::cell::RefCell;

fn new_binding_id() -> BindingId {
    use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};
    static COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;

    COUNTER.fetch_add(1, Ordering::SeqCst).into()
}

#[derive(Debug)]
struct Binding {
    name: String,
    span: Span,
    ast_id: usize,
    // type_: Type,
    id: BindingId,
}

#[derive(Debug, Clone,PartialEq)]
enum Type {
    I64,
    Bool,
    Unit,
    Fn(Vec<Type>, Box<Type>),
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

    fn add_binding(&self,
                   name: String,
                   span: Span,
                   ast_id: usize /* , btype: Type */)
                   -> BindingId {
        let id = new_binding_id();
        let binding = Rc::new(Binding {
            name: name.clone(),
            span: span,
            ast_id: ast_id,
            // type_: btype,
            id: id,
        });

        self.by_name.borrow_mut().insert(name, binding.clone());
        self.by_id.borrow_mut().insert(id, binding);

        id
    }

    fn lookup(&self, name: &str) -> Option<Rc<Binding>> {
        self.by_name
            .borrow()
            .get(name)
            .cloned()
            .or_else(|| self.parent.clone().and_then(|parent| parent.lookup(name)))
    }
}


#[derive(Debug)]
pub enum Ast {
    EmptyList(usize, Rc<Scope>, Span),

    I64Literal(usize, Rc<Scope>, Span, i64),
    BoolLiteral(usize, Rc<Scope>, Span, bool),

    Atom(usize, Rc<Scope>, Span, String),
    Inv(usize, Rc<Scope>, Span, Box<Ast>, Vec<Ast>),

    Do(usize, Rc<Scope>, Span, Vec<Ast>),
    // params, body
    Fn(usize, Rc<Scope>, Span, Vec<usize>, Vec<Ast>),
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

// read: Vec<Sexp>
// sep_defs: (Vec<Def>, Vec<Sexp>)
// process_defs(Vec<Def>)
// sexp_to_ast: Vec<Ast>

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
    let mut env = Env::new();

    let mut asts = vec![];
    let mut binding_ids = vec![];

    for Def(span, name, sexp) in defs {
        let ast = sexp_to_ast(global_scope.clone(), sexp).unwrap();
        let (id, ty) = match ast {
            Ast::I64Literal(id, _, _, _) => (id, TypeTerm::Known(Type::I64)),
            Ast::Atom(id, _, _, _) => (id, TypeTerm::Unknown),
            _ => unimplemented!(),
        };

        asts.push(ast);

        let binding_id = global_scope.add_binding(name.clone(), span, id);
        binding_ids.push(binding_id);
        env.insert(id, Var::new(ty));
    }

    let asts = type_infer(&mut env, asts);

    let consts = asts.into_iter()
                     .enumerate()
                     .map(|(idx, ast)| (binding_ids[idx], ast))
                     .collect();

    (env, consts)
}

// sig?
pub fn macroexpand(env: &MacroEnv, sexp: Sexp) -> Sexp {
    unimplemented!()
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
                        Sexp::Atom(atom_id, _, ref s) if s == "let" => {
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
                                binding_list.push((id, name, value));
                            }

                            let body_asts = try!(sexps.into_iter()
                                                      .map(|sexp| {
                                                          sexp_to_ast(inner_scope.clone(), sexp)
                                                      })
                                                      .fold_results(vec![], vec_collector));

                            Ok(Ast::Let(id, scope.clone(), span, binding_list, body_asts))
                        }
                        Sexp::Atom(atom_id, _, ref s) if s == "fn" => {
                            // assert!(sexps.len() >= 2);
                            // env.push();

                            // let param_list_src = match sexps.remove(0) {
                            //     Sexp::List(_, sexps) => {
                            //         sexps.into_iter()
                            //              .map(|sexp| {
                            //                  match sexp {
                            //                      Sexp::Atom(span, string) => (span, string),
                            //                      _ => panic!("param must be Atom"),
                            //                  }
                            //              })
                            //              .collect_vec()
                            //     }
                            //     _ => panic!("First arg of `fn` must be a list"),
                            // };

                            // let mut param_list = vec![];

                            // for (span, name) in param_list_src {
                            //     let value = Ast::I64Literal(span, -1);
                            //     let record = env.add_record_ast(span, name, &value);

                            //     param_list.push(record.id());
                            // }

                            // let body_asts = try!(sexps_to_asts(env, sexps));

                            // env.pop();

                            // Ok(Ast::Fn(span, param_list, body_asts))
                            unimplemented!()
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

pub fn process_decls(asts: &[Ast]) {

    // TODO: incorperate decls into type inferrence

    fn type_of_ast(ast: &Ast) -> Type {
        match *ast {
            Ast::EmptyList(..) => Type::Unit,
            Ast::BoolLiteral(..) => Type::Bool,
            Ast::I64Literal(..) => Type::I64,
            _ => unimplemented!(),
        }
    }

    for ast in asts {
        match *ast {
            Ast::Let(id, ref scope, span, ref bindings, _) => {
                for &(id, ref string, ref ast) in bindings {
                    scope.add_binding(string.clone(), span, id /* , type_of_ast(ast) */);
                }
            }
            Ast::Fn(..) => unimplemented!(),
            _ => {}
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

impl TypedAst {
    fn get_type(&self) -> Type {
        match *self {
            TypedAst::EmptyList => Type::Unit,
            TypedAst::I64Literal(_) => Type::I64,
            TypedAst::BoolLiteral(_) => Type::Bool,
            TypedAst::Atom(ref ty, _) => ty.clone(),
            TypedAst::Inv(ref ty, _, _) => ty.clone(),
            TypedAst::Do(ref ty, _) => ty.clone(),
            TypedAst::Fn(ref ty, _, _) => ty.clone(),
            TypedAst::Let(ref ty, _, _) => ty.clone(),
            TypedAst::If(ref ty, _, _, _) => ty.clone(),
        }
    }
}

use disjoint_sets::UnionFindNode;

#[derive(Debug, Clone, PartialEq)]
enum TypeTerm {
    Unknown,
    Known(Type),
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

    fn lookup_type(&self, id: usize) -> Type {
        self.get(id).find().with_data(|term| match *term {
            TypeTerm::Known(ref ty) => ty.clone(),
            _ => panic!("Type could not be inferred"),
        })
    }
}

pub fn type_infer(env: &mut Env, asts: Vec<Ast>) -> Vec<TypedAst> {

    fn fresh() -> Var {
        UnionFindNode::new(TypeTerm::Unknown)
    }

    fn known(ty: Type) -> Var {
        UnionFindNode::new(TypeTerm::Known(ty))
    }

    fn unify(env: &mut Env, a: usize, b: usize) {
        env.map_values(a, b, |mut av, mut bv| {
            av.union_with(&mut bv, |a, b| {
                match (a, b) {
                    (TypeTerm::Unknown, b) => b,
                    (a, TypeTerm::Unknown) => a,
                    (a, b) => {
                        assert_eq!(a, b);
                        a
                    }
                }
            });
            (av, bv)
        });
    }

    fn walk(env: &mut Env, ast: &Ast) {
        match *ast {
            Ast::Atom(id, ref scope, span, ref string) => {
                let term = match scope.lookup(&**string) {
                    Some(binding) => known(env.lookup_type(binding.ast_id)),
                    _ => fresh(),
                };

                env.insert(id, term);
            }
            Ast::BoolLiteral(id, ref scope, span, val) => {
                env.insert(id, known(Type::Bool));
            }
            Ast::Do(id, ref scope, span, ref children) => {
                let (last, rest) = children.split_last().unwrap();

                for ast in rest {
                    walk(env, ast);
                }

                walk(env, last);

                env.insert(id, fresh());
                unify(env, id, last.id());
            }
            Ast::EmptyList(id, ref scope, span) => {
                env.insert(id, known(Type::Unit));
            }
            Ast::Fn(..) => unimplemented!(),
            Ast::I64Literal(id, ref scope, span, val) => {
                env.insert(id, known(Type::I64));
            }
            Ast::If(..) => unimplemented!(),
            Ast::Inv(..) => unimplemented!(),
            Ast::Let(id, ref scope, span, ref bindings, ref body) => {
                for &(id, ref name, ref ast) in bindings {
                    walk(env, ast);

                    env.insert(id, fresh());
                    unify(env, id, ast.id());
                }
                // need to unify params with occurances of params
                let (last, rest) = body.split_last().unwrap();

                for ast in rest {
                    walk(env, ast);
                }

                walk(env, last);

                env.insert(id, fresh());
                unify(env, id, last.id());
            }
        }
    }

    fn to_typed(env: &Env, ast: Ast) -> TypedAst {
        match ast {
            Ast::Atom(id, scope, _, string) => {
                TypedAst::Atom(env.lookup_type(id), scope.lookup(&*string).unwrap().id)
            }
            Ast::BoolLiteral(_, _, _, val) => TypedAst::BoolLiteral(val),
            Ast::Do(id, _, _, children) => {
                TypedAst::Do(env.lookup_type(id),
                             children.into_iter().map(|ast| to_typed(env, ast)).collect())
            }
            Ast::EmptyList(..) => TypedAst::EmptyList,
            Ast::Fn(..) => unimplemented!(),
            Ast::I64Literal(_, _, _, val) => TypedAst::I64Literal(val),
            Ast::If(..) => unimplemented!(),
            Ast::Inv(..) => unimplemented!(),
            Ast::Let(id, scope, _, bindings, body) => {
                TypedAst::Let(env.lookup_type(id),
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


pub fn interpret(consts: &Table<BindingId, TypedAst>, asts: &[TypedAst]) -> Box<Display> {

    #[derive(Clone, Debug)]
    enum Value {
        I64(i64),
        Bool(bool),
    }

    impl Display for Value {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            match *self {
                Value::I64(val) => write!(f, "{}", val),
                Value::Bool(val) => write!(f, "{}", val),
            }
        }
    }

    type Env = Table<BindingId, Value>;


    // #[derive(Debug)]
    // pub enum TypedAst {
    //     EmptyList,
    //     I64Literal(i64),
    //     BoolLiteral(bool),

    //     Atom(Type, BindingId),
    //     Inv(Type, Box<TypedAst>, Vec<TypedAst>),

    //     Do(Type, Vec<TypedAst>),
    //     // params, body
    //     Fn(Type, Vec<Rc<Binding>>, Vec<TypedAst>),
    //     // (params, values), body
    //     Let(Type, Vec<(BindingId, TypedAst)>, Vec<TypedAst>),
    //     // condition, then-expr, else-expr
    //     If(Type, Box<TypedAst>, Box<TypedAst>, Box<TypedAst>),
    // }


    fn interpret_(env: &Env, ast: &TypedAst) -> Value {
        match *ast {
            TypedAst::Atom(_, id) => env[&id].clone(),
            TypedAst::BoolLiteral(val) => Value::Bool(val),
            TypedAst::Do(..) => unimplemented!(),
            TypedAst::EmptyList => unimplemented!(),
            TypedAst::Fn(..) => unimplemented!(),
            TypedAst::I64Literal(val) => Value::I64(val),
            TypedAst::If(..) => unimplemented!(),
            TypedAst::Inv(..) => unimplemented!(),
            TypedAst::Let(_, ref bindings, ref body) => {
                let mut env = env.clone();

                for &(id, ref ast) in bindings {
                    let val = interpret_(&env, &ast);
                    env.insert(id, val);
                }

                let (last, rest) = body.split_last().unwrap();

                for ast in rest {
                    interpret_(&env, ast);
                }

                interpret_(&env, last)
            }
        }
    }

    let init_env = Env::new();

    let env = consts.into_iter()
                    .map(|(&id, ast)| (id, interpret_(&init_env, &ast)))
                    .collect();

    let (last, rest) = asts.split_last().unwrap();

    for ast in rest {
        interpret_(&env, ast);
    }

    Box::new(interpret_(&env, last))
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
    // use census;
    // use compile;

    macro_rules! test1 {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
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

                lang::process_decls(&*asts);

                let typed_asts = lang::type_infer(&mut type_env, asts);

                println!("{:#?}", type_env);
                println!("{:#?}", typed_asts);
                let result = lang::interpret(&consts, &*typed_asts).to_string();
                
                assert_eq!(expected, result);
            }
        }
    }

    test1!(eval_i64, "10", "10");
    test1!(def_i64, "(def x 10) x", "10");
    test1!(forward_def_i64, "x (def x 10)", "10");
    // test1!(alias_i64, "(def x 10) (def y x) y", "10");
    // test1!(forward_alias_i64, "y (def y x) (def x 10)", "10");

    test1!(eval_boolean_t, "true", "true");
    test1!(eval_boolean_f, "false", "false");

    // test1!(eval_if_t, "(if true 0 1)", "0");
    // test1!(eval_if_f, "(if false 0 1)", "1");
    // test1!(eval_if_in_if, "(if (if true false true) 0 1)", "1");

    test1!(eval_let, "(let (a 4) a)", "4");
    // test1!(let_alias, "(def a 3) (let (a 4) a)", "4");

    // test1!(fn_eval, "(fn (a) a)", "");
}
