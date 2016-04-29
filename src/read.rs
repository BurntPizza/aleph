
use std::error::Error;
use std::fmt::{self, Display, Formatter};

use lang::{Env, InputStream, Form, AtomKind, ScopeId, Statement, SourcePos};
use lang::CharSyntaxType::*;

pub fn read(env: &mut Env, stream: &mut InputStream) -> Result<Form, Box<Error>> {
    let mut results = vec![];

    loop {
        match read_token(stream, env) {
            Ok(form) => results.push(form),
            Err(e) => {
                let error_type = e.type_.clone();

                if error_type == ReadErrorType::EOS ||
                   (error_type == ReadErrorType::EmptyToken && stream.next().is_none()) {
                    let result = Form::Statement(Statement::Namespace {
                        name: "default".to_owned(),
                        forms: results,
                    });
                    return Ok(result);
                }
                return Err(e.into());
            }
        }
    }
}


/// The type of functions implementing reader macros
#[derive(Debug, PartialEq, Clone)]
enum ReadErrorType {
    EOS,
    EmptyToken,
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

#[derive(Copy, Clone)]
enum ReaderState {
    Reading,
    Accumulating,
    Escaping,
    TokenFinished,
}

pub type ReaderMacroFunction = fn(&mut InputStream, &mut Env, u8)
                                  -> ::std::result::Result<Option<Form>, ReadError>;

/// Ported from the [Common Lisp HyperSpec](http://clhs.lisp.se/Body/02_b.htm)
fn read_token(stream: &mut InputStream, env: &mut Env) -> ::std::result::Result<Form, ReadError> {

    use lang::MacroCharType::*;
    use self::ReaderState::*;
    use self::ReadErrorType::*;

    let mut token = Vec::with_capacity(8);
    let mut state = Reading;

    // step 1
    loop {
        match state {
            Reading => {
                match stream.next() {
                    Some(x) => {
                        match env.lookup_syntax_type(x) {
                            Invalid => return Err(ReadError::invalid_char(x).into()),
                            Whitespace => {} // repeat step 1
                            MacroChar(_) => {
                                match env.lookup_reader_macro(x) {
                                    Some(f) => {
                                        match f(stream, env, x) {
                                            Ok(Some(ast)) => return Ok(ast),
                                            Ok(None) => {} // repeat step 1
                                            Err(e) => return Err(e),
                                        }
                                    }
                                    _ => return Err(ReadError::no_macro_for_char(x).into()),
                                }
                            }
                            SingleEscape => {
                                match stream.next() {
                                    Some(y) => {
                                        token.push(y);
                                        state = Accumulating;
                                    }
                                    None => return Err(ReadError::eos().into()),
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
                            ReadError::empty_token().into()
                        } else {
                            ReadError::eos().into()
                        })
                    }
                }
            }
            // step 8
            Accumulating => {
                loop {
                    match stream.next() {
                        Some(y) => {
                            match env.lookup_syntax_type(y) {
                                Invalid => return Err(ReadError::invalid_char(y).into()),
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
                                        None => return Err(ReadError::eos().into()),
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
                            match env.lookup_syntax_type(y) {
                                // repeat step 9
                                TokenChar | MacroChar(_) | Whitespace => token.push(y),
                                SingleEscape => {
                                    match stream.next() {
                                        // repeat step 9
                                        Some(z) => token.push(z),
                                        None => return Err(ReadError::eos().into()),
                                    }
                                }
                                // enter step 8
                                MultEscape => {
                                    state = Accumulating;
                                    break;
                                }
                                Invalid => return Err(ReadError::invalid_char(y).into()),
                            }
                        }
                        None => return Err(ReadError::eos().into()),
                    }
                }
            }
            // step 10
            TokenFinished => {
                let token = String::from_utf8(token).unwrap();

                if env.is_special(&token) {
                    return Err(ReadError::other(format!("Cannot shadow special form `{}`", token))
                                   .into());
                }

                let scope = ScopeId::from(0); // TODO!!
                let position = stream.pos();
                let span = SourcePos::new(position, token.len() as u32);
                let id = env.insert(token, AtomKind::Var, scope, span);
                return Ok(Form::atom(id));
            }
        }
    }
}


impl From<ReadErrorType> for ReadError {
    fn from(src: ReadErrorType) -> Self {
        use self::ReadErrorType::*;

        let desc = match src {
            EOS => "Unexpected end of stream".to_owned(),
            EmptyToken => "Empty token".to_owned(),
            InvalidToken(ref t) => format!("Invalid token: `{}`", t),
            InvalidChar(c) => format!("Invalid character: `{}` ({:?})", c as char, c as char),
            NoMacro(c) => format!("No function found for macro character: `{}`", c as char),
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

impl Display for ReadError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.desc)
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

/// Reader macro function for line comments.
pub fn line_comment_reader(stream: &mut InputStream,
                           _: &mut Env,
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
pub fn left_paren_reader(stream: &mut InputStream,
                         env: &mut Env,
                         _: u8)
                         -> Result<Option<Form>, ReadError> {

    let position = stream.pos() - 1;
    let mut list = vec![];

    loop {
        match stream.next() {
            Some(w) if env.lookup_syntax_type(w) == Whitespace => {}
            Some(c) if c == b')' => {
                let len = stream.pos() - position;
                let pos = SourcePos::new(position, len);
                return Ok(Some(Form::list(env, pos, list)));
            }
            Some(_) => {
                stream.unread();
                list.push(try!(read_token(stream, env)));
            }
            _ => {
                return Err(ReadError::eos());
            }
        }
    }
}

/// Reader macro function to abort on unexpected closing parentheses.
pub fn right_paren_reader(_: &mut InputStream,
                          _: &mut Env,
                          _: u8)
                          -> Result<Option<Form>, ReadError> {
    return Err(ReadError::other("Unexpected right parenthesis"));
}
