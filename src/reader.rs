
//! Infrastructure for parsing code

use std::collections::HashMap;
use std::fmt::{self, Display, Debug, Formatter};

use itertools::*;

use core;

use self::CharSyntaxType::*;
use self::MacroCharType::*;
use self::ReaderState::*;
use self::ReadErrorType::*;

/// The type of functions implementing reader macros
pub type MacroFunction = fn(&mut ReaderEnv, u8) -> Result<Option<Form>, ()>;

// Note: guaranteed to be ASCII
#[derive(Debug, Clone)]
pub struct InputStream {
    src: String,
    idx: usize,
}

impl InputStream {
    pub fn new(src: String) -> Self {
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



#[derive(Clone, Debug, PartialEq, Default)]
pub struct Span {
    // instead refer to ReaderEnv? Interpreter?
    pub text: String,
}

impl Span {
    fn new(text: String) -> Self {
        // TODO: default
        Span { text: text }
    }
}

/// Lexical program representation: untyped s-expressions.
#[derive(Debug, PartialEq, Clone)]
pub enum Form {
    /// A token, such as an identifier, number, or anything that isn't a `List`.
    Atom(Span),
    /// A list of `Form`s, usually delimited by parentheses.
    List(Vec<Form>),
}

impl Form {
    /// Construct an Atom containing a String
    pub fn atom(s: Span) -> Self {
        Form::Atom(s)
    }
    /// Construct a list of forms
    pub fn list<I>(src: I) -> Self
        where I: IntoIterator<Item = Form>
    {
        Form::List(src.into_iter().collect())
    }
    /// Construct a List form containing nothing
    pub fn empty_list() -> Self {
        Form::List(vec![])
    }

    pub fn add_to_list(&mut self, item: Form) {
        match *self {
            Form::Atom(_) => panic!(),
            Form::List(ref mut l) => l.push(item),
        }
    }
}

impl Display for Form {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let s = match *self {
            Form::Atom(ref s) => s.text.clone(),
            Form::List(ref list) => format!("({})", list.iter().join(" ")),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ReadErrorType {
    EOS,
    EmptyToken, // merge these?
    InvalidToken(String),
    InvalidChar(u8),
    NoMacro(u8),
    Other(String),
}


#[derive(Clone, Debug)]
pub struct ReadError {
    type_: ReadErrorType,
    desc: String,
}


// note: chars do not need to track their position for now, recover that info from InputStream
// this also should have to public
// merge into Environment?
pub struct ReaderEnv {
    pub stream: InputStream,
    pub readtable: ReadTable,
    pub macros: MacroTable,
    pub output: Vec<ReadError>,
}

pub struct ReadTable(HashMap<u8, CharSyntaxType>);
pub struct MacroTable(HashMap<u8, MacroFunction>);

/// The classes of reader macro characters
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum MacroCharType {
    /// A terminating macro character
    Terminating,
    /// A non-terminating macro character
    Nonterminating,
}

/// The lexical classes of input characters
#[derive(PartialEq, Copy, Clone)]
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

#[derive(Copy, Clone)]
enum ReaderState {
    Reading,
    Accumulating,
    Escaping,
    TokenFinished,
}

// read all of a string
pub fn read_all(src: String) -> Result<Vec<Form>, String> {
    let mut reader = ReaderEnv::new_default(InputStream::new(src));

    match reader.read_all() {
        Ok(forms) => Ok(forms),
        Err(_) => Err(reader.output.iter().map(|e| format!("Error: {}\n", e)).join("\n")),
    }
}

// TODO: move readtables etc. out to Environment, right?
/// Read a Form from a String
pub fn read_string(src: String) -> Result<Form, String> {
    // TODO DI
    let mut reader = ReaderEnv::new_default(InputStream::new(src));
    reader.read_token()
          .map_err(|_| reader.output.iter().map(|e| format!("Error: {}\n", e)).join("\n"))
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
    pub fn eos() -> Self {
        ReadErrorType::EOS.into()
    }
    pub fn empty_token() -> Self {
        ReadErrorType::EmptyToken.into()
    }
    pub fn invalid_token<S: Into<String>>(token: S) -> Self {
        ReadErrorType::InvalidToken(token.into()).into()
    }
    pub fn invalid_char(c: u8) -> Self {
        ReadErrorType::InvalidChar(c).into()
    }
    pub fn no_macro_for_char(c: u8) -> Self {
        ReadErrorType::NoMacro(c).into()
    }
    pub fn other<S: Into<String>>(msg: S) -> Self {
        ReadErrorType::Other(msg.into()).into()
    }
}


impl ReaderEnv {
    pub fn new_default(src: InputStream) -> Self {
        ReaderEnv {
            stream: src,
            readtable: ReadTable::default(),
            macros: MacroTable::default(),
            output: vec![],
        }
    }

    pub fn last_error(&self) -> ReadError {
        self.output.last().unwrap().clone()
    }

    pub fn read_all(&mut self) -> Result<Vec<Form>, ()> {
        let mut results = vec![];

        loop {
            match self.read_token() {
                Ok(form) => results.push(form),
                _ => {
                    let error_type = self.output.last().unwrap().type_.clone();

                    if error_type == ReadErrorType::EOS ||
                       (error_type == ReadErrorType::EmptyToken && self.stream.next().is_none()) {
                        return Ok(results);
                    }
                    return Err(());
                }
            }
        }
    }

    /// Ported from the [Common Lisp HyperSpec](http://clhs.lisp.se/Body/02_b.htm)
    pub fn read_token(&mut self) -> Result<Form, ()> {
        macro_rules! ret_err {
            ($e:expr) => {{
                self.output.push($e);
                return Err(());
            }};
        }

        let mut token = Vec::with_capacity(8);
        let mut state = Reading;

        // step 1
        loop {
            match state {
                Reading => {
                    match self.stream.next() {
                        Some(x) => {
                            match self.readtable.get(x) {
                                Invalid => ret_err!(ReadError::invalid_char(x)),
                                Whitespace => {} // repeat step 1
                                MacroChar(_) => {
                                    match self.macros.get(x) {
                                        Some(f) => {
                                            match f(self, x) {
                                                Ok(Some(ast)) => return Ok(ast),
                                                Ok(None) => {} // repeat step 1
                                                Err(e) => return Err(e),
                                            }
                                        }
                                        _ => ret_err!(ReadError::no_macro_for_char(x)),
                                    }
                                }
                                SingleEscape => {
                                    match self.stream.next() {
                                        Some(y) => {
                                            token.push(y);
                                            state = Accumulating;
                                        }
                                        None => ret_err!(ReadError::eos()),
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
                            ret_err!(if token.is_empty() {
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
                        match self.stream.next() {
                            Some(y) => {
                                match self.readtable.get(y) {
                                    Invalid => ret_err!(ReadError::invalid_char(y)),
                                    // enter step 10
                                    Whitespace => {
                                        state = TokenFinished;
                                        // TODO is this right?
                                        // stream.unread(); // preserve whitespace
                                        break;
                                    }
                                    // enter step 10
                                    MacroChar(Terminating) => {
                                        self.stream.unread();
                                        state = TokenFinished;
                                        break;
                                    }
                                    SingleEscape => {
                                        match self.stream.next() {
                                            // repeat step 8
                                            Some(z) => token.push(z),
                                            None => ret_err!(ReadError::eos()),
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
                        match self.stream.next() {
                            Some(y) => {
                                match self.readtable.get(y) {
                                    // repeat step 9
                                    TokenChar | MacroChar(_) | Whitespace => token.push(y),
                                    SingleEscape => {
                                        match self.stream.next() {
                                            // repeat step 9
                                            Some(z) => token.push(z),
                                            None => ret_err!(ReadError::eos()),
                                        }
                                    }
                                    // enter step 8
                                    MultEscape => {
                                        state = Accumulating;
                                        break;
                                    }
                                    Invalid => ret_err!(ReadError::invalid_char(y)),
                                }
                            }
                            None => ret_err!(ReadError::eos()),
                        }
                    }
                }
                // step 10
                TokenFinished => {
                    return Ok(Form::atom(Span::new(String::from_utf8_lossy(&*token).into_owned())))
                }
            }
        }
    }
}


impl MacroTable {
    /// Look up a reader macro function for a character in the table
    #[allow(map_clone)]
    pub fn get(&self, c: u8) -> Option<MacroFunction> {
        self.0.get(&c).map(|&f| f)
    }
}

impl ReadTable {
    /// Look up the lexical class of a character in the table
    pub fn get(&self, c: u8) -> CharSyntaxType {
        *self.0.get(&c).unwrap_or(&Invalid)
    }
}

impl Default for ReadTable {
    fn default() -> Self {
        ReadTable((0..128u8)
                      .map(|c| {
                          (c,
                           {
                              match c as char {
                                  '(' | ')' | ';' => MacroChar(Terminating),
                                  '_' | '-' | '+' => TokenChar,
                                  c if c.is_alphanumeric() => TokenChar,
                                  c if c.is_whitespace() => Whitespace,
                                  _ => Invalid,
                              }
                          })
                      })
                      .collect())
    }
}

impl Debug for ReadTable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_map()
         .entries((0..self.0.len())
                      .map(|i| i as u8 as char)
                      .zip(self.0.iter()))
         .finish()
    }
}

impl Default for MacroTable {
    fn default() -> Self {
        let mut mt: HashMap<u8, MacroFunction> = HashMap::new();
        mt.insert(b';', core::line_comment_reader);
        mt.insert(b'(', core::left_paren_reader);
        mt.insert(b')', core::right_paren_reader);
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


#[cfg(test)]
mod test {
    use itertools::*;
    use reader;

    #[test]
    fn test_read_all() {
        let input = "hello world".to_owned();
        let output = "hello world";
        assert_eq!(reader::read_all(input).unwrap().iter().map(|f| f.to_string()).join(" "),
                   output);
    }
}
