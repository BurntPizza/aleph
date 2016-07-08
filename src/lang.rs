use itertools::*;

use std::rc::Rc;
use std::error::Error;
use std::convert::Into;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{self, Debug, Formatter};

use read::{self, Span, Sexp, InputStream};
use compile;
use print_table;


// fn callsite:
// vm_push_ret_addr()
// vm_jmp(fn_ptr)

// fn def:
// expands to:
// (let [arg1 (__vm_pop)
//       arg2 (__vm_pop)
//       ...]
//   <body>)
// (__vm_ret)





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

// reader: Text -> Form(s)
// census + macroexpand: Form -> Expr

pub type Table<K, V> = HashMap<K, V>;
pub type Result<T> = ::std::result::Result<T, Box<Error>>;

fn read_in_default_ns_and_env<T>(input: T) -> Vec<Sexp>
    where T: Into<String>
{
    let mut env = Default::default();
    let mut stream = InputStream::new(input.into());

    read::read(&mut env, &mut stream).unwrap()
}

fn macroexpand(env: &Env, forms: &mut [Sexp]) -> Result<()> {
    // TODO
    unimplemented!()
}

fn exec(env: &Env, asts: &[Ast]) -> Result<String> {
    let program = try!(compile::compile(env, asts));
    Ok(program.exec())
}

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

// TODO: better name
#[derive(Clone, Debug)]
pub enum BindingKey {
    String(String),
    Id(BindingId),
}

#[derive(Debug)]
pub enum Ast {
    EmptyList(Span),

    I64Literal(Span, i64),
    BoolLiteral(Span, bool),

    Atom(Span, BindingKey),
    Inv(Span, Box<Ast>, Vec<Ast>),

    Do(Span, Vec<Ast>),
    // params, body
    Fn(Span, Vec<Ast>, Vec<Ast>),
    // params, values, body
    Let(Span, Vec<Ast>, Vec<Ast>, Vec<Ast>),
    // condition, then-expr, else-expr
    If(Span, Box<Ast>, Box<Ast>, Box<Ast>),
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

pub struct Env {
    scope: ScopeId,
    fn_defs: Vec<FnDef>,
    id_table: Vec<Rc<BindingRecord>>,
    forward_decls: HashSet<String>,
    // yet-to-be parsed
    work_list: VecDeque<(Span, String, Rc<Sexp>)>,
}

impl Env {
    // TODO: figure out and clean up add_record stuff + parse_sexp/create_binding

    pub fn add_record(&mut self, span: Span, name: String, rhs: Sexp) {
        self.forward_decls.insert(name.clone());
        self.work_list.push_back((span, name, Rc::new(rhs)));
    }

    pub fn add_record_ast(&mut self, span: Span, name: String, rhs: &Ast) {
        self.forward_decls.insert(name.clone());
        let binding = match create_binding(self, rhs) {
            BindResult::Ok(binding) => binding,
            _ => unimplemented!(),
        };

        let scope = self.current_scope();
        self.insert(name, binding, scope, span);
    }

    pub fn lookup_by_id(&self, id: BindingId) -> Rc<BindingRecord> {
        match self.id_table.get(*id as usize).cloned() {
            Some(record) => record,
            None => panic!("`{:?}` not present in {:#?}", id, self),
        }
    }

    pub fn lookup_by_name<T>(&self, symbol: T) -> Option<Rc<BindingRecord>>
        where T: AsRef<str>
    {
        let symbol = symbol.as_ref();
        self.id_table.iter().find(|record| record.symbol == symbol).cloned()
    }

    pub fn contains_name<T>(&self, symbol: T) -> bool
        where T: AsRef<str>
    {
        self.forward_decls.contains(symbol.as_ref())
    }

    pub fn fn_defs(&self) -> &[FnDef] {
        &*self.fn_defs
    }

    pub fn finish_work_list(&mut self) -> Result<()> {
        while !self.work_list.is_empty() {
            let (span, name, def) = self.work_list.pop_front().unwrap();

            try!(self.test_name_collision(&name));

            match parse_sexp(self, &def) {
                ParseResult::Ok(ast) => {
                    match create_binding(self, &ast) {
                        BindResult::Ok(binding) => {
                            let scope = self.current_scope();
                            self.insert(name, binding, scope, span);
                        }
                        BindResult::Unfinished => {
                            self.work_list.push_back((span, name, def));
                        }
                        BindResult::Err(e) => return Err(e),
                    }
                }
                ParseResult::Unfishished => {
                    self.work_list.push_back((span, name, def));
                    continue;
                }
                ParseResult::Err(e) => return Err(e),
            }


        }

        Ok(())
    }

    pub fn push(&self) -> Self {
        let new_env = Env {
            scope: ScopeId::from(self.scope.0 + 1),
            fn_defs: vec![],
            id_table: self.id_table.clone(),
            forward_decls: self.forward_decls.clone(),
            work_list: VecDeque::new(),
        };

        new_env
    }

    fn test_name_collision(&self, name: &str) -> Result<()> {
        match self.lookup_by_name(name) {
            Some(_) => Err(format!("Name collision: {}", name).into()),
            _ => Ok(()),
        }
    }

    fn new<T: Into<String>>(ns: T) -> Self {
        let invalid_source_pos = Span::new(0, 0);
        let root_scope_id = ScopeId::from(0);

        let mut env = Env {
            scope: ScopeId::from(0),
            fn_defs: vec![],
            // root_ns: ns,
            id_table: vec![],
            forward_decls: HashSet::new(),
            work_list: VecDeque::new(),
        };



        // define the macro / special dichotomy (and if it needs to exist)
        // "Special Forms" are just builtin macros, they have custom AST nodes to do their codegen

        // fn -- custom AST node for codegen
        // macro -- (directive) macro that expands to ??? (regular invocation, to be delt with in macroexpansion)
        // let -- how is this going to be implemented? (custom AST node for codegen)
        // if -- ditto
        // do -- same


        for &name in &SPECIALS {
            env.insert(name,
                       // AtomKind::Macro,
                       Binding::ExternalConst,
                       root_scope_id,
                       invalid_source_pos);
        }

        env.insert("true",
                   Binding::Const(ConstType::Bool(true)),
                   root_scope_id,
                   invalid_source_pos);
        env.insert("false",
                   Binding::Const(ConstType::Bool(false)),
                   root_scope_id,
                   invalid_source_pos);

        env
    }

    fn current_scope(&self) -> ScopeId {
        self.scope
    }

    fn insert<T>(&mut self,
                 symbol: T,
                 // atom_kind: AtomKind,
                 binding: Binding,
                 scope: ScopeId,
                 pos: Span)
        where T: Into<String>
    {
        let symbol = symbol.into();
        self.forward_decls.insert(symbol.clone());
        let id = BindingId::from(self.id_table.len() as u32);
        let record = BindingRecord {
            id: id,
            pos: pos,
            symbol: symbol,
            scope: scope,
            binding: binding,
        };
        self.id_table.push(Rc::new(record));
    }
}

enum ParseResult {
    Ok(Ast),
    Unfishished,
    Err(Box<Error>),
}

fn parse_sexp(env: &Env, sexp: &Sexp) -> ParseResult {
    match *sexp {
        Sexp::Atom(span, ref string) => {
            match string.parse::<i64>() {
                Ok(val) => ParseResult::Ok(Ast::I64Literal(span, val)),
                _ => ParseResult::Ok(Ast::Atom(span, BindingKey::String(string.clone()))),
            }
        }
        Sexp::List(_, ref sexps) => unimplemented!(),
    }
}

#[derive(Debug)]
pub struct BindingRecord {
    pos: Span,
    symbol: String,
    scope: ScopeId,
    id: BindingId,
    binding: Binding,
}

impl BindingRecord {
    pub fn id(&self) -> BindingId {
        self.id
    }

    pub fn symbol(&self) -> &str {
        &*self.symbol
    }

    pub fn binding(&self) -> &Binding {
        &self.binding
    }
}

enum BindResult {
    Ok(Binding),
    Unfinished,
    Err(Box<Error>),
}

fn create_binding<'a>(env: &Env, ast: &'a Ast) -> BindResult {
    match *ast {
        Ast::I64Literal(_, val) => BindResult::Ok(Binding::Const(ConstType::I64(val))),
        // alias
        Ast::Atom(_, ref binding_key) => {
            let record = match *binding_key {
                BindingKey::Id(id) => env.lookup_by_id(id),
                BindingKey::String(ref string) => {
                    match env.lookup_by_name(&string) {
                        Some(record) => record,
                        _ => {
                            return if !env.contains_name(&string) {
                                BindResult::Err(format!("Unknown symbol: `{}`", string).into())
                            } else {
                                BindResult::Unfinished
                            }
                        }
                    }
                }
            };

            BindResult::Ok(record.binding().clone())
        }
        _ => unimplemented!(),
    }
}

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
#[derive(Clone, Debug)]
pub enum Binding {
    Const(ConstType),
    ExternalConst,
}
// TopLevel(ConstKind), // member current of namespace
// Module(ConstKind), // member of external namespace
// Local(AtomKind), // not member of namespace (e.g. function args)

macro_rules! def_id {
    ($name:ident, $ty:ty) => {
        #[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
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

def_id!(BindingId, u32);
def_id!(ScopeId, u32);

impl Debug for Env {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use print_table::Alignment::Right;

        let rows = self.id_table
                       .iter()
                       .map(|br| {
                           vec![br.id().to_string(),
                                br.symbol.clone(),
                                format!("{:?}", br.binding()),
                                br.scope.to_string(),
                                format!("{:?}", br.pos)]
                       });

        writeln!(f,
                 "{:#?}",
                 print_table::debug_table("Env",
                                          vec!["id", "symbol", "kind", "scope", "pos"],
                                          vec![Right, Right, Right, Right, Right],
                                          rows))
    }
}



#[cfg(test)]
mod test {
    use itertools::*;

    use lang;
    use census;
    use compile;

    macro_rules! test1 {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                
                let input: &'static str = $input;
                let expected: &'static str = $expected;
                
                let sexps = lang::read_in_default_ns_and_env(input);
                let mut env = lang::Env::new("testing");
                let asts = census::census(&mut env, sexps).unwrap();
                println!("{:?}", input);
                println!("{:#?}", env);
                println!("{:#?}", asts);

                assert_eq!(lang::exec(&env, &*asts).unwrap(), expected);
                // typecheck(&env, &ast).unwrap();
                //let program = compile::compile(&env, &ast).unwrap();
                //assert_eq!(program.exec(), expected);
            }
        }
    }

    test1!(eval_i64, "10", "10");
    test1!(def_i64, "(def x 10) x", "10");
    test1!(forward_def_i64, "x (def x 10)", "10");
    test1!(alias_i64, "(def x 10) (def y x) y", "10");
    test1!(forward_alias_i64, "y (def y x) (def x 10)", "10");

    test1!(eval_boolean_t, "true", "true");
    test1!(eval_boolean_f, "false", "false");

    test1!(eval_if_t, "(if true 0 1)", "0");
    test1!(eval_if_f, "(if false 0 1)", "1");
    test1!(eval_if_in_if, "(if (if true false true) 0 1)", "1");

    test1!(eval_let, "(let (a 4) a)", "4");
}
