
use itertools::*;

use std::rc::Rc;
use std::error::Error;
use std::convert::Into;
use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};

use read;

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

fn read_in_default_ns_and_env<T>(input: T) -> (Form, Env)
    where T: Into<String>
{
    let mut env = Env::new("default");
    let mut stream = InputStream::new(input.into());

    (read(&mut env, &mut stream).unwrap(), env)
}

fn read(env: &mut Env, stream: &mut InputStream) -> Result<Form> {
    read::read(env, stream)
}

fn census(env: &mut Env, form: Form) -> Result<Form> {
    unimplemented!()
}

fn macroexpand(env: &Env, form: Form) -> Result<Form> {
    unimplemented!()
}

fn fully_parsed(env: &Env, form: &Form) -> bool {
    match *form {
        Form::Expr(ref e) => {
            match *e {
                Expr::Atom(id) => {
                    match env.lookup_by_id(id).kind() {
                        AtomKind::Macro => false,
                        AtomKind::Var => true,
                        AtomKind::Special => true,
                    }
                }
                Expr::Inv(ref callee, ref args) => {
                    if !fully_parsed(env, &**callee) {
                        return false;
                    }

                    for form in args {
                        if !fully_parsed(env, form) {
                            return false;
                        }
                    }

                    true
                }
            }
        }
        Form::Statement(_) => false,
    }
}

#[derive(Clone, Debug)]
pub enum Form {
    Statement(Statement),
    Expr(Expr),
}

impl Form {
    pub fn atom(id: BindingId) -> Self {
        Form::Expr(Expr::Atom(id))
    }

    pub fn list<T>(env: &mut Env, pos: SourcePos, items: T) -> Self
        where T: IntoIterator<Item = Form>
    {
        let mut items = items.into_iter();

        match items.next() {
            Some(form) => {
                match form {
                    Form::Expr(expr) => {
                        Form::Expr(Expr::Inv(Box::new(Form::Expr(expr)), items.collect()))
                    }
                    Form::Statement(sta) => {
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

// fn is_statement<T>(symbol: T) -> bool
//     where T: AsRef<str>
// {
//     ["def", "ns", "use", "defreader"].contains(&symbol.as_ref())
// }

#[derive(Clone, Debug)]
pub enum Statement {
    Namespace {
        name: String,
        forms: Vec<Form>,
    },
    Define {
        id: BindingId,
        to_value: Expr,
    },
}

#[derive(Clone, Debug)]
enum Expr {
    Atom(BindingId), // note: is empty list '()' an atom?
    Inv(Box<Form>, Vec<Form>),
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

        let invalid_source_pos = SourcePos { idx: 0, len: 0 };
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

        // empty list
        env.insert("()", AtomKind::Var, root_scope_id, invalid_source_pos);

        env
    }

    pub fn is_special<T>(&self, symbol: T) -> bool
        where T: AsRef<str>
    {
        ["let", "if", "fn", "def", "ns", "use", "macro", "defreader"].contains(&symbol.as_ref())
    }



    pub fn insert<T>(&mut self,
                     symbol: T,
                     kind: AtomKind,
                     scope: ScopeId,
                     pos: SourcePos)
                     -> BindingId
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
}

#[derive(Debug)]
struct Namespace {
    name: String,
    scope: ScopeId,
}

#[derive(Debug)]
struct BindingRecord {
    pos: SourcePos,
    symbol: String,
    scope: ScopeId,
    id: BindingId,
    kind: AtomKind,
}

impl BindingRecord {
    fn kind(&self) -> AtomKind {
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

#[derive(Copy, Clone, Debug)]
pub struct SourcePos {
    idx: u32,
    len: u32,
}

impl SourcePos {
    pub fn new(idx: u32, len: u32) -> Self {
        SourcePos {
            idx: idx,
            len: len,
        }
    }
}

pub struct InputStream {
    src: String,
    idx: usize,
}

impl InputStream {
    pub fn pos(&self) -> u32 {
        self.idx as u32
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

fn display(form: &Form, env: &Env) -> String {
    use std::iter::once;

    match *form {
        Form::Statement(ref s) => {
            match *s {
                Statement::Namespace { ref name, ref forms } => {
                    format!("(ns {}) {}",
                            name,
                            forms.iter().map(|form| display(form, env)).join(" "))
                }
                Statement::Define { id, ref to_value } => {
                    let name = &env.lookup_by_id(id).symbol;
                    let to_value = Form::Expr(to_value.clone());
                    format!("(define {} {})", name, display(&to_value, env))
                }
            }
        }
        Form::Expr(ref e) => {
            match *e {
                Expr::Atom(id) => format!("{}", &env.lookup_by_id(id).symbol),
                Expr::Inv(ref callee, ref args) => {
                    let forms = once(&**callee)
                                    .chain(args.iter())
                                    .map(|f| display(f, env))
                                    .join(" ");
                    format!("({})", forms)
                }
            }
        }
    }
}

impl Debug for Env {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "Env:")
    }
}



#[cfg(test)]
mod test {
    use lang;

    #[test]
    fn read_basic() {
        let inputs = vec!["", "()", "hello", "(1 2 3)", "(hello (world))"];

        for input in inputs {
            let (form, env) = lang::read_in_default_ns_and_env(input);


            println!("{:#?}", env);
            println!("{:#?}", form);
            assert_eq!(format!("(ns default) {}", input),
                       lang::display(&form, &env));
        }
    }

    #[test]
    fn main() {
        let input = "";

        let (mut form, mut env) = lang::read_in_default_ns_and_env(input);
        let mut repititions = 0;

        while !lang::fully_parsed(&env, &form) {
            if repititions == 64 {
                panic!("limit reached!");
            }

            form = lang::census(&mut env, form).unwrap();
            form = lang::macroexpand(&env, form).unwrap();
            repititions += 1;
        }

        println!("{}", lang::display(&form, &env));
        panic!();
    }
}
