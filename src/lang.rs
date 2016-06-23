
use itertools::*;

use std::rc::Rc;
use std::error::Error;
use std::convert::Into;
use std::collections::HashMap;
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



// fn fully_parsed(env: &Env, form: &Sexp) -> bool {
//     match *form {
//         Sexp::Atom(_, ref string) => {
//             env.lookup_by_name(&**string).map(|b| b.binding().0 == AtomKind::Var).unwrap()
//         }
//         Sexp::List(_, ref children) => {
//             children.is_empty() || children.iter().all(|s| fully_parsed(env, s))
//         }
//     }
// }

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


// TODO: unchecked
pub fn form_to_expr(form: Form) -> Expr {
    match form {
        Form::Expr(e) => e,
        _ => unreachable!(),
    }
}

pub fn forms_to_exprs<T>(forms: T) -> Vec<Expr>
    where T: IntoIterator<Item = Form>
{
    forms.into_iter()
         .map(form_to_expr)
         .collect()
}

#[derive(Clone, Debug)]
pub enum Form {
    Directive(Directive),
    Expr(Expr),
}

impl Form {
    pub fn atom(id: BindingId) -> Self {
        Form::Expr(Expr::Atom(id))
    }

    pub fn list<T>(env: &mut Env, pos: Span, items: T) -> Self
        where T: IntoIterator<Item = Form>
    {
        let mut items = items.into_iter();

        match items.next() {
            Some(form) => {
                match form {
                    Form::Expr(expr) => {
                        let args = forms_to_exprs(items);
                        Form::Expr(Expr::Inv(Box::new(expr), args))
                    }
                    Form::Directive(sta) => {
                        //
                        unimplemented!()
                    }
                }
            }
            None => {
                // empty list (TODO: is root scope ok?)
                let id = env.insert("()",
                                    // AtomKind::Var,
                                    Binding::Const(ConstType::Unit),
                                    ScopeId::from(0),
                                    pos);
                Form::Expr(Expr::Atom(id))
            }
        }
    }
}

// TODO: better name
#[derive(Debug)]
pub enum BindingKey {
    String(String),
    Id(BindingId),
}

#[derive(Debug)]
pub enum Ast {
    EmptyList(Span),

    I64Literal(Span, i64),

    Atom(Span, BindingKey),
    Inv(Span, Box<Ast>, Vec<Ast>),

    Do(Span, Vec<Expr>),
    // params, body
    Fn(Span, Vec<Expr>, Vec<Expr>),
    // params, values, body
    Let(Span, Vec<Expr>, Vec<Expr>, Vec<Expr>),
    // condition, then-expr, else-expr
    If(Span, Box<Expr>, Box<Expr>, Box<Expr>),
}

impl Ast {
    fn is_directive(&self) -> bool {
        // TODO
        unimplemented!()
    }
}

#[derive(Clone, Debug)]
pub enum Directive {
    Namespace {
        name: String,
    },
    Define {
        id: BindingId,
        to_value: Expr,
    },
}

#[derive(Clone, Debug)]
pub enum Expr {
    Atom(BindingId),
    DoExpr(Vec<Expr>),
    // params, body
    FnExpr(Vec<Expr>, Vec<Expr>),
    // params, values, body
    LetExpr(Vec<Expr>, Vec<Expr>, Vec<Expr>),
    // condition, then-expr, else-expr
    IfExpr(Box<Expr>, Box<Expr>, Box<Expr>),
    Inv(Box<Expr>, Vec<Expr>),
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
    fn_defs: Vec<FnDef>,
    root_ns: Namespace,
    id_table: Vec<Rc<BindingRecord>>,
}

impl Env {
    pub fn test_name_collision(&self, name: &str) -> Result<()> {
        match self.lookup_by_name(&*name) {
            Some(_record) => Err(format!("Name collision: {}", name).into()),
            None => Ok(()),
        }
    }

    // pub fn try_install_binding(&mut self, name: String, span: Span, rhs: Sexp) -> Result<()> {
    //     // test for name collision
    //
    // }

    pub fn fn_defs(&self) -> &[FnDef] {
        &*self.fn_defs
    }

    fn new<T: Into<String>>(ns: T) -> Self {
        let invalid_source_pos = Span::new(0, 0);
        let root_scope_id = ScopeId::from(0);

        let ns = Namespace {
            name: ns.into(),
            scope: root_scope_id,
        };

        let mut env = Env {
            fn_defs: vec![],
            root_ns: ns,
            id_table: vec![],
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

        env
    }

    pub fn add_record<T>(&mut self,
                         symbol: T,
                         // atom_kind: AtomKind,
                         binding: Binding,
                         span: Span)
                         -> BindingId
        where T: Into<String>
    {
        let scope = self.current_scope();
        self.insert(symbol,
                    // atom_kind,
                    binding,
                    scope,
                    span)
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

    fn current_scope(&self) -> ScopeId {
        // TODO
        ScopeId::from(0)
    }

    fn insert<T>(&mut self,
                 symbol: T,
                 // atom_kind: AtomKind,
                 binding: Binding,
                 scope: ScopeId,
                 pos: Span)
                 -> BindingId
        where T: Into<String>
    {
        let id = BindingId::from(self.id_table.len() as u32);
        let record = BindingRecord {
            id: id,
            pos: pos,
            symbol: symbol.into(),
            scope: scope,
            binding: binding,
        };
        self.id_table.push(Rc::new(record));
        id
    }
}

#[derive(Debug)]
struct Namespace {
    name: String,
    scope: ScopeId,
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

    pub fn binding(&self) -> Binding {
        self.binding
    }
}

// #[derive(Copy, Clone, Debug, PartialEq)]
// pub enum AtomKind {
//     Var,
//     Macro,
// }

// #[derive(Copy, Clone, Debug, PartialEq)]
// pub enum ConstKind {
//     Var(ConstType),
//     Macro,
// }

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ConstType {
    Unit,
    I64(i64),
}

// (def x 10) installs 'x' into current namespace (TopLevel)
// (use hello/x) installs 'x' into current namespace (Module)
// (use hello/y) '' 'y' ''
// (use hello) installs (lazily) 'hello/x' and 'hello/y' into current namespace (Module)
// (let [x 10] x) 'x' is Local in it's scope (no associated namespace)
#[derive(Copy, Clone, Debug, PartialEq)]
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

fn display(env: &Env, form: &Form) -> String {
    match *form {
        Form::Directive(ref s) => {
            match *s {
                Directive::Namespace { ref name } => format!("(ns {})", name),
                Directive::Define { id, ref to_value } => {
                    let name = &env.lookup_by_id(id).symbol;
                    let to_value = Form::Expr(to_value.clone());
                    format!("(def {} {})", name, display(env, &to_value))
                }
            }
        }
        Form::Expr(ref e) => display_expr(env, e),
    }
}

fn display_expr(env: &Env, expr: &Expr) -> String {
    match *expr {
        Expr::Atom(id) => format!("{}", &env.lookup_by_id(id).symbol),
        Expr::DoExpr(ref args) => display_inv(env, "do", &**args),
        Expr::FnExpr(ref params, ref body_exprs) => {
            format!("(fn ({}) {})",
                    params.iter().map(|e| display_expr(env, e)).join(" "),
                    body_exprs.iter().map(|e| display_expr(env, e)).join(" "))
        }
        Expr::IfExpr(ref condition, ref then_expr, ref else_expr) => {
            format!("(if {} {} {})",
                    display_expr(env, condition),
                    display_expr(env, then_expr),
                    display_expr(env, else_expr))
        }
        Expr::LetExpr(ref params, ref bindings, ref body_exprs) => {
            let params_bindings = params.iter()
                                        .zip(bindings.iter())
                                        .map(|(p, b)| {
                                            format!("{} {}",
                                                    display_expr(env, p),
                                                    display_expr(env, b))
                                        })
                                        .join(" ");
            let body_exprs = body_exprs.iter().map(|e| display_expr(env, e)).join(" ");
            format!("(let ({}) {})", params_bindings, body_exprs)
        }
        Expr::Inv(ref callee, ref args) => display_inv(env, display_expr(env, callee), &**args),
    }
}

fn display_inv<T>(env: &Env, callee: T, args: &[Expr]) -> String
    where T: Into<String>
{
    use std::iter::once;

    let exprs = once(callee.into())
                    .chain(args.iter()
                               .map(|e| display_expr(env, e)))
                    .join(" ");
    format!("({})", exprs)
}

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

    #[test]
    fn read() {
        let inputs = vec!["10", "(def x 10) x"];

        for input in inputs {
            let sexps = lang::read_in_default_ns_and_env(input);
            let mut env = lang::Env::new("testing");
            let ast = census::census(&mut env, sexps).unwrap();
            println!("{:#?}", env);
            println!("{:#?}", ast);
            let program = compile::compile(&env, &ast).unwrap();
            assert_eq!(program.exec(), "10");
        }
    }
}
