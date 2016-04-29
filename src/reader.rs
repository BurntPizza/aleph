
//! Infrastructure for parsing code

use std::error::Error;
use std::collections::HashMap;
use std::fmt::{self, Display, Debug, Formatter};

use itertools::*;

use self::CharSyntaxType::*;
use self::MacroCharType::*;
use self::ReaderState::*;
use self::ReadErrorType::*;


pub fn read_forms(input: String) -> Result<Vec<Form>, Box<Error>> {
    let mut stream = InputStream::new(input);
    let readtable = ReadTable::default();
    let macrotable = MacroTable::default();

    let mut results = vec![];

    loop {
        match read_token(&mut stream, &readtable, &macrotable) {
            Ok(form) => results.push(form),
            Err(e) => {
                let error_type = e.type_.clone();

                if error_type == ReadErrorType::EOS ||
                   (error_type == ReadErrorType::EmptyToken && stream.next().is_none()) {
                    return Ok(results);
                }
                return Err(e.into());
            }
        }
    }
}

/// Lexical program representation: untyped s-expressions.
#[derive(Debug, PartialEq, Clone)]
pub struct Form {
    kind: FormKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum FormKind {
    Atom(String),
    List(Vec<Form>),
}

impl Form {
    pub fn kind(&self) -> &FormKind {
        &self.kind
    }

    fn into_children(self) -> Vec<Form> {
        match self.kind {
            FormKind::Atom(s) => panic!("{} is not a List", s),
            FormKind::List(v) => v,
        }
    }

    fn new(kind: FormKind) -> Self {
        Form { kind: kind }
    }

    /// Construct an Atom containing a String
    fn atom(s: String) -> Self {
        Form::new(FormKind::Atom(s))
    }
    /// Construct a list of forms
    fn list<I>(src: I) -> Self
        where I: IntoIterator<Item = Form>
    {
        Form::new(FormKind::List(src.into_iter().collect()))
    }
    /// Construct a List form containing nothing
    fn empty_list() -> Self {
        Form::new(FormKind::List(vec![]))
    }

    fn add_to_list(&mut self, item: Form) {
        match self.kind {
            FormKind::Atom(_) => panic!(),
            FormKind::List(ref mut l) => l.push(item),
        }
    }
}

fn expand_let_form(forms: Vec<Form>) -> Result<Form, Box<Error>> {
    if forms.len() < 2 {
        return Err("let form must have at least two arguments".into());
    }

    let (args_list_form, body_forms_list) = forms.split_first().unwrap();

    match *args_list_form.kind() {
        FormKind::Atom(ref s) => {
            Err(format!("1st argument of let form must be a list: {}", s).into())
        }
        FormKind::List(ref arg_forms) => {
            if arg_forms.len() % 2 != 0 {
                return Err("argument list of let form must have even number of forms".into());
            }

            let (bindings, vals): (Vec<(_, Form)>, Vec<(_, Form)>) = arg_forms.iter()
                                                                              .cloned()
                                                                              .enumerate()
                                                                              .partition(|p| {
                                                                                  p.0 % 2 == 0
                                                                              });

            let bindings = bindings.into_iter().map(|(_, form)| form);
            let vals = vals.into_iter().map(|(_, form)| form);

            // TODO more checks here

            let fn_form = Form::atom("fn".into());
            let arg_list = Form::list(bindings);
            let body = body_forms_list.iter().cloned();
            let fn_call = Form::list(vec![fn_form, arg_list].into_iter().chain(body));

            Ok(Form::list(::std::iter::once(fn_call).chain(vals)))
        }
    }
}

// (implicit) macro form, arg forms -> result form
type MacroExpanderFn = fn(Vec<Form>) -> Result<Form, Box<Error>>;

pub struct MacroExpansionEnv {
    table: HashMap<String, MacroExpanderFn>,
}

impl MacroExpansionEnv {
    fn new() -> Self {
        let mut table = HashMap::new();

        table.insert("let".into(), expand_let_form as MacroExpanderFn);

        MacroExpansionEnv { table: table }
    }

    // Ok(Some(fn)) => is a macro form, and this is it's expander
    // Ok(None) => not a macro form
    // Err => some other problem (having this here just in case)
    fn lookup(&self, form: &Form) -> Result<Option<MacroExpanderFn>, Box<Error>> {
        match *form.kind() {
            FormKind::List(ref v) => {
                match v.len() {
                    0 => Ok(None),
                    _ => self.lookup(&v[0]), // should only allow an Atom? (investigate with tests)
                }
            }
            // should this be here?
            FormKind::Atom(ref s) => Ok(self.table.get(s).cloned()),
        }
    }
}

fn macroexpand(mut form: Form, env: &MacroExpansionEnv) -> Result<Form, Box<Error>> {
    loop {
        match try!(macroexpand_1(form, &env)) {
            ExpansionResult::Expanded(new_form) => form = new_form,
            ExpansionResult::NotExpanded(old_form) => return Ok(old_form),
        }
    }
}

enum ExpansionResult {
    Expanded(Form),
    NotExpanded(Form),
}

fn macroexpand_1(form: Form, env: &MacroExpansionEnv) -> Result<ExpansionResult, Box<Error>> {
    match try!(env.lookup(&form)) {
        Some(expand) => {
            // it's a macro invocation, expand it
            let forms = form.into_children().into_iter().skip(1).collect();
            Ok(ExpansionResult::Expanded(try!(expand(forms))))
        }
        None => Ok(ExpansionResult::NotExpanded(form)),
    }
}


// will need to take env as mut arg once defmacro exists
// (via populate_namespaces pass before macroexpansion)
pub fn macroexpand_forms(forms: Vec<Form>) -> Result<(Vec<Form>, MacroExpansionEnv), Box<Error>> {
    let env = MacroExpansionEnv::new();
    let mut results = Vec::with_capacity(forms.len());

    for form in forms {
        results.push(try!(macroexpand(form, &env)));
    }

    Ok((results, env))
}



/// The type of functions implementing reader macros
type ReaderMacroFunction = fn(&mut InputStream, &ReadTable, &MacroTable, u8)
                              -> Result<Option<Form>, ReadError>;


// Note: guaranteed to be ASCII
#[derive(Debug, Clone)]
struct InputStream {
    src: String,
    idx: usize,
}

impl InputStream {
    fn new(src: String) -> Self {
        assert!(::std::ascii::AsciiExt::is_ascii(&*src));
        InputStream { src: src, idx: 0 }
    }

    fn unread(&mut self) {
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



#[derive(Clone, Debug, PartialEq, Default)]
pub struct Span {
    // instead refer to ReaderEnv? Interpreter?
    text: String,
}

impl Span {
    fn new(text: String) -> Self {
        // TODO: default
        Span { text: text }
    }

    pub fn text(&self) -> &str {
        &*self.text
    }
}
impl Display for Form {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let s = match self.kind {
            FormKind::Atom(ref s) => s.clone(),
            FormKind::List(ref list) => format!("({})", list.iter().join(" ")),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Clone)]
enum ReadErrorType {
    EOS,
    EmptyToken, // merge these?
    InvalidToken(String),
    InvalidChar(u8),
    NoMacro(u8),
    Other(String),
}


#[derive(Clone, Debug)]
struct ReadError {
    type_: ReadErrorType,
    desc: String,
}

struct ReadTable(HashMap<u8, CharSyntaxType>);
struct MacroTable(HashMap<u8, ReaderMacroFunction>);

/// The classes of reader macro characters
#[derive(Debug, PartialEq, Copy, Clone)]
enum MacroCharType {
    /// A terminating macro character
    Terminating,
    /// A non-terminating macro character
    Nonterminating,
}

/// The lexical classes of input characters
#[derive(PartialEq, Copy, Clone)]
enum CharSyntaxType {
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

#[derive(Copy, Clone)]
enum ReaderState {
    Reading,
    Accumulating,
    Escaping,
    TokenFinished,
}

impl From<ReadErrorType> for ReadError {
    fn from(src: ReadErrorType) -> Self {
        let desc = match src {
            EOS => "Unexpected end of stream".to_owned(),
            EmptyToken => "Empty token".to_owned(),
            InvalidToken(ref t) => format!("Invalid token: `{}`", t),
            InvalidChar(c) => format!("Invalid character: `{:?}`", c as char),
            NoMacro(c) => format!("No function found for macro character: `{}`", c),
            Other(ref msg) => msg.to_owned(),
        };

        ReadError {
            type_: src,
            desc: desc,
        }
    }
}

impl ::std::error::Error for ReadError {
    fn description(&self) -> &str {
        &*self.desc
    }
}

impl ReadError {
    fn eos() -> Self {
        ReadErrorType::EOS.into()
    }
    fn empty_token() -> Self {
        ReadErrorType::EmptyToken.into()
    }
    fn invalid_token<S: Into<String>>(token: S) -> Self {
        ReadErrorType::InvalidToken(token.into()).into()
    }
    fn invalid_char(c: u8) -> Self {
        ReadErrorType::InvalidChar(c).into()
    }
    fn no_macro_for_char(c: u8) -> Self {
        ReadErrorType::NoMacro(c).into()
    }
    fn other<S: Into<String>>(msg: S) -> Self {
        ReadErrorType::Other(msg.into()).into()
    }
}

/// Ported from the [Common Lisp HyperSpec](http://clhs.lisp.se/Body/02_b.htm)
fn read_token(stream: &mut InputStream,
              readtable: &ReadTable,
              macros: &MacroTable)
              -> Result<Form, ReadError> {
    let mut token = Vec::with_capacity(8);
    let mut state = Reading;

    // step 1
    loop {
        match state {
            Reading => {
                match stream.next() {
                    Some(x) => {
                        match readtable.get(x) {
                            Invalid => return Err(ReadError::invalid_char(x)),
                            Whitespace => {} // repeat step 1
                            MacroChar(_) => {
                                match macros.get(x) {
                                    Some(f) => {
                                        match f(stream, readtable, macros, x) {
                                            Ok(Some(ast)) => return Ok(ast),
                                            Ok(None) => {} // repeat step 1
                                            Err(e) => return Err(e),
                                        }
                                    }
                                    _ => return Err(ReadError::no_macro_for_char(x)),
                                }
                            }
                            SingleEscape => {
                                match stream.next() {
                                    Some(y) => {
                                        token.push(y);
                                        state = Accumulating;
                                    }
                                    None => return Err(ReadError::eos()),
                                }
                            }
                            // enter step 9
                            MultEscape => state = Escaping,
                            // enter step 8
                            TokenChar => {
                                token.push(x);
                                state = Accumulating;
                            }
                        }
                    }
                    None => {
                        return Err(if token.is_empty() {
                            ReadError::empty_token()
                        } else {
                            ReadError::eos()
                        })
                    }
                }
            }
            // step 8
            Accumulating => {
                loop {
                    match stream.next() {
                        Some(y) => {
                            match readtable.get(y) {
                                Invalid => return Err(ReadError::invalid_char(y)),
                                // enter step 10
                                Whitespace => {
                                    state = TokenFinished;
                                    // TODO is this right?
                                    // stream.unread(); // preserve whitespace
                                    break;
                                }
                                // enter step 10
                                MacroChar(Terminating) => {
                                    stream.unread();
                                    state = TokenFinished;
                                    break;
                                }
                                SingleEscape => {
                                    match stream.next() {
                                        // repeat step 8
                                        Some(z) => token.push(z),
                                        None => return Err(ReadError::eos()),
                                    }
                                }
                                MultEscape => {} // enter step 9
                                // repeat step 8
                                TokenChar | MacroChar(Nonterminating) => token.push(y),
                            }
                        }
                        // enter step 10
                        None => {
                            state = TokenFinished;
                            break;
                        }
                    }
                }
            }
            // step 9
            Escaping => {
                loop {
                    match stream.next() {
                        Some(y) => {
                            match readtable.get(y) {
                                // repeat step 9
                                TokenChar | MacroChar(_) | Whitespace => token.push(y),
                                SingleEscape => {
                                    match stream.next() {
                                        // repeat step 9
                                        Some(z) => token.push(z),
                                        None => return Err(ReadError::eos()),
                                    }
                                }
                                // enter step 8
                                MultEscape => {
                                    state = Accumulating;
                                    break;
                                }
                                Invalid => return Err(ReadError::invalid_char(y)),
                            }
                        }
                        None => return Err(ReadError::eos()),
                    }
                }
            }
            // step 10
            TokenFinished => return Ok(Form::atom(String::from_utf8_lossy(&*token).into_owned())),
        }
    }
}


impl MacroTable {
    /// Look up a reader macro function for a character in the table
    #[allow(map_clone)]
    fn get(&self, c: u8) -> Option<ReaderMacroFunction> {
        self.0.get(&c).map(|&f| f)
    }
}

impl ReadTable {
    /// Look up the lexical class of a character in the table
    fn get(&self, c: u8) -> CharSyntaxType {
        *self.0.get(&c).unwrap_or(&Invalid)
    }
}

impl Default for ReadTable {
    fn default() -> Self {
        let mut table = HashMap::new();

        for k in 0..128u8 {
            let v = match k as char {
                '(' | ')' | ';' => MacroChar(Terminating),
                '_' | '-' | '+' => TokenChar,
                c if c.is_alphanumeric() => TokenChar,
                c if c.is_whitespace() => Whitespace,
                _ => Invalid,
            };

            table.insert(k, v);
        }

        ReadTable(table)
    }
}

impl Debug for ReadTable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let entries = (0..self.0.len())
                          .map(|i| i as u8 as char)
                          .zip(self.0.iter());
        f.debug_map()
         .entries(entries)
         .finish()
    }
}

impl Default for MacroTable {
    fn default() -> Self {
        let mut mt: HashMap<u8, ReaderMacroFunction> = HashMap::new();
        mt.insert(b';', line_comment_reader);
        mt.insert(b'(', left_paren_reader);
        mt.insert(b')', right_paren_reader);
        MacroTable(mt)
    }
}

impl Debug for CharSyntaxType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let s = match *self {
            MacroChar(t) => format!("{:?}inatingMacro", t),
            Invalid => "Invalid".to_owned(),
            Whitespace => "Whitespace".to_owned(),
            SingleEscape => "SingleEscape".to_owned(),
            MultEscape => "MultEscape".to_owned(),
            TokenChar => "Token".to_owned(),
        };
        write!(f, "{}", s)
    }
}

impl Display for ReadError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}


/// Reader macro function for line comments.
fn line_comment_reader(stream: &mut InputStream,
                       _: &ReadTable,
                       _: &MacroTable,
                       _: u8)
                       -> Result<Option<Form>, ReadError> {
    loop {
        match stream.next() {
            Some(b'\n') | None => break,
            Some(_) => {}
        }
    }
    Ok(None)
}

/// Reader macro function for reading list s-expressions delimited by parentheses.
fn left_paren_reader(stream: &mut InputStream,
                     readtable: &ReadTable,
                     macros: &MacroTable,
                     _: u8)
                     -> Result<Option<Form>, ReadError> {
    let mut list = vec![];

    loop {
        match stream.next() {
            Some(w) if readtable.get(w) == Whitespace => {}
            Some(c) if c == b')' => return Ok(Some(Form::list(list))),
            Some(_) => {
                stream.unread();
                list.push(try!(read_token(stream, readtable, macros)));
            }
            _ => {
                return Err(ReadError::eos());
            }
        }
    }
}

/// Reader macro function to abort on unexpected closing parentheses.
fn right_paren_reader(_: &mut InputStream,
                      _: &ReadTable,
                      _: &MacroTable,
                      _: u8)
                      -> Result<Option<Form>, ReadError> {
    return Err(ReadError::other("Unexpected right parenthesis"));
}

#[cfg(test)]
mod test {
    use itertools::*;
    use reader;

    #[test]
    fn read_all() {
        let input = "hello world".to_owned();
        let output = "hello world";
        assert_eq!(reader::read_forms(input)
                       .unwrap()
                       .iter()
                       .map(|f| f.to_string())
                       .join(" "),
                   output);
    }

    #[test]
    fn empty_list() {
        assert_eq!(reader::read_forms("()".into())
                       .unwrap()
                       .iter()
                       .map(|f| f.to_string())
                       .join(" "),
                   "()");
    }

    #[test]
    fn line_comment_reader() {
        let input = "; hello\nworld".to_owned();
        let output = "world";
        assert_eq!(reader::read_forms(input)
                       .unwrap()
                       .iter()
                       .map(|f| f.to_string())
                       .join(" "),
                   output);
    }

    #[test]
    fn left_paren_reader() {
        let input = "(hello world)".to_owned();
        let output = input.clone();
        assert_eq!(reader::read_forms(input)
                       .unwrap()
                       .iter()
                       .map(|f| f.to_string())
                       .join(" "),
                   output);
    }

    #[test]
    fn right_paren_reader() {
        let input = ")".to_owned();
        assert!(reader::read_forms(input).is_err());
    }

    #[test]
    fn let_form() {
        let input = "(let (x xd     \
                           y yd)    \
                       (body1) body2)"
                        .into();

        let output = "((fn (x y) (body1) body2) xd yd)";

        assert_eq!(reader::read_forms(input)
                       .and_then(reader::macroexpand_forms)
                       .unwrap()
                       .0
                       .iter()
                       .map(|f| f.to_string())
                       .join(" "),
                   output);
    }
}
