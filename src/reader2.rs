
use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};
use std::ops::{Deref, DerefMut};
use std::cell::RefCell;


use super::InputStream;
use form::Form;

use self::CharSyntaxType::*;
use self::MacroCharType::*;
use self::ReaderState::*;
use self::ReadErrorType::*;


#[derive(Debug)]
pub enum ReadErrorType {
    EOS,
    EmptyToken, // merge these?
    InvalidToken(String),
    InvalidChar(u8),
    NoMacro(u8),
    Other(String),
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

#[derive(Debug)]
pub struct ReadError {
    _type: ReadErrorType,
    _desc: String,
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

fn mov<T>(e: T) -> T {
    e
}




#[derive(Debug, Copy, Clone)]
enum ReaderState {
    Reading,
    Accumulating,
    Escaping,
    TokenFinished,
}


// note: chars do not need to track their position for now, recover that info from InputStream
pub struct ReaderContext {
    stream: InputStream,
    readtable: ReadTable,
    macros: MacroTable,
    output: Vec<ReadError>,
}

impl ReaderContext {
    
    fn read(&mut self) -> Result<Form, ()> {
        let mut token = Vec::with_capacity(8);
        let mut state = Reading;

        macro_rules! ret_err {
            ($e:expr) => {{
                self.output.push($e);
                return Err(());
            }};
        }
        
        // step 1
        loop {
            match state {
                Reading => {
                    match self.stream.next() {
                        Some(x) => {
                            match self.readtable[&x] {
                                Invalid => ret_err!(ReadError::invalid_char(x)),
                                Whitespace => {} // repeat step 1
                                MacroChar(_) => {
                                    match self.macros.get(&x).map(|&f| f) {
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
                                match self.readtable[&y] {
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
                                match self.readtable[&y] {
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
                TokenFinished => unimplemented!(), //return interpret_token(token),
            }
        }
    }
}


pub struct ReadTable(HashMap<u8, CharSyntaxType>);


pub type MacroFunction = fn(&mut ReaderContext, u8) -> Result<Option<Form>, ()>;

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

impl Deref for ReadTable {
    type Target = HashMap<u8, CharSyntaxType>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ReadTable {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}


impl Deref for MacroTable {
    type Target = HashMap<u8, MacroFunction>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for MacroTable {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}


impl Debug for CharSyntaxType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use self::CharSyntaxType::*;

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
