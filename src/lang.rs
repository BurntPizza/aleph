
use itertools::*;

use std::rc::Rc;
use std::error::Error;
use std::convert::Into;
use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};

use read::{self, Span};
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
// match? or is this a complicated macro?

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

fn read_in_default_ns_and_env<T>(input: T) -> (Vec<Form>, Env)
    where T: Into<String>
{
    let mut env = Env::new("default");
    let mut stream = InputStream::new(input.into());

    (read::read(&mut env, &mut stream).unwrap(), env)
}

fn macroexpand(env: &Env, form: Vec<Form>) -> Result<Vec<Form>> {
    unimplemented!()
}

fn fully_parsed(env: &Env, form: &Form) -> bool {

    fn helper(env: &Env, expr: &Expr) -> bool {
        use std::iter::once;

        match *expr {
            Expr::Atom(id) => {
                match env.lookup_by_id(id).kind() {
                    AtomKind::Macro => false,
                    AtomKind::Var => true,
                    AtomKind::Special => true,
                }
            }
            Expr::DoExpr(ref args) => args.iter().all(|e| helper(env, e)),
            Expr::IfExpr(ref cond, ref t_e, ref e_e) => {
                helper(env, cond) && helper(env, t_e) && helper(env, e_e)
            }
            Expr::LetExpr(ref params, ref values, ref body) => {
                params.iter().chain(values.iter()).chain(body.iter()).all(|e| helper(env, e))
            }
            Expr::FnExpr(ref params, ref body) => {
                params.iter().chain(body.iter()).all(|e| helper(env, e))
            }
            Expr::Inv(ref callee, ref args) => {
                once(&**callee).chain(args.iter()).all(|e| helper(env, e))
            }
        }
    }

    match *form {
        Form::Expr(ref e) => helper(env, e),
        Form::Directive(_) => false,
    }
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
                        Form::Expr(Expr::Inv(Box::new(expr),
                                             items.map(|f| {
                                                      match f {
                                                          Form::Expr(e) => e,
                                                          _ => unreachable!(),
                                                      }
                                                  })
                                                  .collect()))
                    }
                    Form::Directive(sta) => {
                        //
                        unimplemented!()
                    }
                }
            }
            None => {
                // empty list (TODO: is root scope ok?)
                let id = env.insert("()", AtomKind::Var, ScopeId::from(0), pos);
                Form::Expr(Expr::Atom(id))
            }
        }
    }
}

pub fn is_special<T>(symbol: T) -> bool
    where T: AsRef<str>
{
    ["let", "if", "fn", "do", "def", "ns", "use", "macro", "defreader"].contains(&symbol.as_ref())
}


fn is_statement<T>(symbol: T) -> bool
    where T: AsRef<str>
{
    ["def", "ns", "use", "defreader"].contains(&symbol.as_ref())
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


pub struct Env {
    root_ns: Namespace,
    id_table: Vec<Rc<BindingRecord>>,
    read_table: Table<char, CharSyntaxType>,
    reader_macros: Table<char, read::ReaderMacroFunction>,
}

impl Env {
    pub fn lookup_syntax_type(&self, c: u8) -> CharSyntaxType {
        self.read_table.get(&(c as char)).cloned().unwrap_or(CharSyntaxType::Invalid)
    }

    pub fn lookup_reader_macro(&self, c: u8) -> Option<read::ReaderMacroFunction> {
        self.reader_macros.get(&(c as char)).map(|&f| f)
    }

    fn new<T: Into<String>>(ns: T) -> Self {
        use self::CharSyntaxType::*;

        let invalid_source_pos = Span::new(0, 0);
        let root_scope_id = ScopeId::from(0);

        let ns = Namespace {
            name: ns.into(),
            scope: root_scope_id,
        };

        let mut env = Env {
            root_ns: ns,
            id_table: vec![],
            read_table: Table::new(),
            reader_macros: Table::new(),
        };

        // readtable
        for k in 0..128u8 {
            let k = k as char;
            let v = match k {
                '_' | '-' | '+' => TokenChar,
                c if c.is_alphanumeric() => TokenChar,
                c if c.is_whitespace() => Whitespace,
                _ => continue,
            };

            env.read_table.insert(k, v);
        }

        env.new_reader_macro(';', read::line_comment_reader, MacroCharType::Terminating);
        env.new_reader_macro('(', read::left_paren_reader, MacroCharType::Terminating);
        env.new_reader_macro(')', read::right_paren_reader, MacroCharType::Terminating);

        // "directives"
        // not present in fully_parsed() == true Forms
        env.insert("fn", AtomKind::Macro, root_scope_id, invalid_source_pos);
        env.insert("ns", AtomKind::Macro, root_scope_id, invalid_source_pos);
        env.insert("use", AtomKind::Macro, root_scope_id, invalid_source_pos);
        env.insert("def", AtomKind::Macro, root_scope_id, invalid_source_pos);
        env.insert("macro", AtomKind::Macro, root_scope_id, invalid_source_pos);
        env.insert("defreader",
                   AtomKind::Macro,
                   root_scope_id,
                   invalid_source_pos);

        env.insert("let", AtomKind::Special, root_scope_id, invalid_source_pos);
        env.insert("if", AtomKind::Special, root_scope_id, invalid_source_pos);
        env.insert("do", AtomKind::Special, root_scope_id, invalid_source_pos);

        env
    }

    pub fn add_record<T>(&mut self, symbol: T, kind: AtomKind, span: Span) -> BindingId
        where T: Into<String>
    {
        let scope = self.current_scope();
        self.insert(symbol, kind, scope, span)
    }

    fn current_scope(&self) -> ScopeId {
        // TODO
        ScopeId::from(0)
    }

    fn insert<T>(&mut self, symbol: T, kind: AtomKind, scope: ScopeId, pos: Span) -> BindingId
        where T: Into<String>
    {
        let id = BindingId::from(self.id_table.len() as u32);
        let record = BindingRecord {
            id: id,
            pos: pos,
            symbol: symbol.into(),
            scope: scope,
            kind: kind,
        };
        self.id_table.push(Rc::new(record));
        id
    }

    fn new_reader_macro(&mut self, c: char, f: read::ReaderMacroFunction, type_: MacroCharType) {
        self.reader_macros.insert(c, f);
        self.read_table.insert(c, CharSyntaxType::MacroChar(type_));
    }

    fn lookup_by_id(&self, id: BindingId) -> Rc<BindingRecord> {
        match self.id_table.get(*id as usize).cloned() {
            Some(record) => record,
            None => panic!("`{:?}` not present in {:#?}", id, self),
        }
    }

    pub fn lookup_by_symbol<T>(&self, symbol: T) -> Option<Rc<BindingRecord>>
        where T: AsRef<str>
    {
        let symbol = symbol.as_ref();
        self.id_table.iter().find(|record| record.symbol == symbol).cloned()
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
    kind: AtomKind,
}

impl BindingRecord {
    pub fn id(&self) -> BindingId {
        self.id
    }

    pub fn kind(&self) -> AtomKind {
        self.kind
    }
}

#[derive(Copy, Clone, Debug)]
pub enum AtomKind {
    Var,
    Macro,
    // invoked like Macro, but is allowed in fully_parsed Forms
    Special,
}

// (def x 10) installs 'x' into current namespace (TopLevel)
// (use hello/x) installs 'x' into current namespace (Module)
// (use hello/y) '' 'y' ''
// (use hello) installs (lazily) 'hello/x' and 'hello/y' into current namespace (Module)
// (let [x 10] x) 'x' is Local in it's scope (no associated namespace)
#[derive(Debug)]
enum BindingKind {
    TopLevel, // member current of namespace
    Module, // member of external namespace
    Local, // not member of namespace (e.g. function args)
}

pub struct InputStream {
    src: String,
    idx: usize,
}

impl InputStream {
    pub fn pos(&self) -> usize {
        self.idx
    }

    fn new(src: String) -> Self {
        assert!(::std::ascii::AsciiExt::is_ascii(&*src));
        InputStream { src: src, idx: 0 }
    }

    pub fn unread(&mut self) {
        assert!(self.idx > 0);
        self.idx -= 1;
    }
}

impl Iterator for InputStream {
    type Item = u8;
    fn next(&mut self) -> Option<Self::Item> {
        if self.idx < self.src.len() {
            let c = Some(self.src.as_bytes()[self.idx]);
            self.idx += 1;
            c
        } else {
            None
        }
    }
}



#[derive(PartialEq, Copy, Clone, Debug)]
pub enum CharSyntaxType {
    /// An invalid character
    Invalid,
    /// A whitespace character
    Whitespace,
    /// A reader macro character
    MacroChar(MacroCharType),
    /// A single-escape character
    SingleEscape,
    /// A multiple-escape character
    MultEscape,
    /// A character that constitutes a token
    TokenChar,
}

/// The classes of reader macro characters
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum MacroCharType {
    /// A terminating macro character
    Terminating,
    /// A non-terminating macro character
    Nonterminating,
}



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
                                format!("{:?}", br.kind()),
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

    #[test]
    fn main() {
        let input = "()";

        let (mut forms, mut env) = lang::read_in_default_ns_and_env(input);
        let mut passes = 0;

        println!("{:?}", env);

        loop {
            if passes == 64 {
                panic!("limit reached!");
            }

            println!("Before: {:?}", forms);
            forms = census::census(&mut env, forms).unwrap();
            println!("After: {:?}", forms);
            forms = lang::macroexpand(&env, forms).unwrap();

            passes += 1;

            if forms.iter().all(|form| lang::fully_parsed(&env, form)) {
                break;
            }
        }

        println!("Forms:");
        println!("{}",
                 forms.iter().map(|form| lang::display(&env, &form)).join(" "));
        panic!();
    }

    #[test]
    fn read() {
        let inputs = vec!["(def x 10)"];

        for input in inputs {
            let (forms, env) = lang::read_in_default_ns_and_env(input);


            println!("{:#?}", env);
            println!("{:#?}", forms);
            assert_eq!(input,
                       forms.iter().map(|form| lang::display(&env, form)).join(" "));
        }
    }
}
