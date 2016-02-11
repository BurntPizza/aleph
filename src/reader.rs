
use std::collections::HashMap;
use std::fmt::{self, Display, Debug, Formatter};
use std::ops::{Deref, DerefMut};

use core;
use form::Form;
use super::InputStream;

use self::CharSyntaxType::*;
use self::MacroCharType::*;
use self::ReaderState::*;
use self::ReadErrorType::*;
use self::TokenParser::*;

pub type MacroFunction = fn(&mut ReaderContext, u8) -> Result<Option<Form>, ()>;

#[derive(Debug)]
pub enum ReadErrorType {
    EOS,
    EmptyToken, // merge these?
    InvalidToken(String),
    InvalidChar(u8),
    NoMacro(u8),
    Other(String),
}

#[derive(Debug)]
pub struct ReadError {
    _type: ReadErrorType,
    _desc: String,
}

// note: chars do not need to track their position for now, recover that info from InputStream
pub struct ReaderContext {
    pub stream: InputStream,
    pub readtable: ReadTable,
    pub macros: MacroTable,
    pub output: Vec<ReadError>,
}

pub struct ReadTable(HashMap<u8, CharSyntaxType>);

pub struct MacroTable(HashMap<u8, MacroFunction>);

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum MacroCharType {
    Term,
    NonTerm,
}

#[derive(PartialEq, Copy, Clone)]
pub enum CharSyntaxType {
    Invalid,
    Whitespace,
    MacroChar(MacroCharType),
    SingleEscape,
    MultEscape,
    TokenChar,
}

#[derive(Copy, Clone)]
enum ReaderState {
    Reading,
    Accumulating,
    Escaping,
    TokenFinished,
}

enum TokenParser {
    Init,
    Symbol(String),
    Error(ReadError),
}


pub fn read_string(src: String) -> Result<Form, Vec<ReadError>> {
    // TODO DI
    let mut reader = ReaderContext {
        stream: InputStream::new(src),
        readtable: ReadTable::default(),
        macros: MacroTable::default(),
        output: vec![],
    };
    reader.read().map_err(move |_| reader.output)
}


impl From<ReadErrorType> for ReadError {
    fn from(src: ReadErrorType) -> Self {
        let desc = match src {
            EOS => "Unexpected end of stream".to_owned(),
            EmptyToken => "Empty token".to_owned(),
            InvalidToken(ref t) => format!("Invalid token: `{}`", t),
            InvalidChar(c) => format!("Invalid character: `{}`", c),
            NoMacro(c) => format!("No function found for macro character: `{}`", c),
            Other(ref msg) => msg.to_owned(),
        };

        ReadError {
            _type: src,
            _desc: desc,
        }
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


impl ReaderContext {
    pub fn read(&mut self) -> Result<Form, ()> {
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
                                    MacroChar(Term) => {
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
                                    TokenChar | MacroChar(NonTerm) => token.push(y),
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
                TokenFinished => return Ok(Form::atom(String::from_utf8_lossy(&*token)
                                                      .into_owned())),
            }
        }
    }
}


impl MacroTable {
    pub fn get(&self, c: u8) -> Option<MacroFunction> {
        self.0.get(&c).map(|&f| f)
    }
}

impl ReadTable {
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
                                  '(' | ')' | ';' => MacroChar(Term),
                                  '_' | '-' => TokenChar,
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
        mt.insert(b'\n', core::line_comment_reader);
        mt.insert(b'(', core::left_paren_reader);
        mt.insert(b')', core::right_paren_reader);
        MacroTable(mt)
    }
}

impl Debug for CharSyntaxType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f,
               "{}",
               match *self {
                   MacroChar(t) => format!("{:?}inatingMacro", t),
                   Invalid => "Invalid".to_owned(),
                   Whitespace => "Whitespace".to_owned(),
                   SingleEscape => "SingleEscape".to_owned(),
                   MultEscape => "MultEscape".to_owned(),
                   TokenChar => "Token".to_owned(),
               })
    }
}

