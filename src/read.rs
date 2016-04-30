
use std::error::Error;
use std::fmt::{self, Display, Formatter};

use lang::{self, Env, InputStream, Form, AtomKind, ScopeId, Directive, Expr};
use lang::CharSyntaxType::*;

pub fn read(env: &mut Env, stream: &mut InputStream) -> Result<Vec<Form>, Box<Error>> {
    let mut results = vec![];

    loop {
        match read_token(stream, env) {
            Ok(form) => results.push(form),
            Err(e) => {
                let error_type = e.type_.clone();

                if error_type == ReadErrorType::EOS ||
                   (error_type == ReadErrorType::EmptyToken && stream.next().is_none()) {
                    break;
                }
                return Err(e.into());
            }
        }
    }

    sexps_to_forms(env, results)
}

fn sexps_to_forms(env: &mut Env, sexps: Vec<Sexp>) -> Result<Vec<Form>, Box<Error>> {
    sexps.into_iter().map(|sexp| sexp_to_form(env, sexp)).collect()
}

fn sexp_to_form(env: &mut Env, sexp: Sexp) -> Result<Form, Box<Error>> {
    match sexp {
        Sexp::Atom(span, token) => {
            let id = env.add_record(token, AtomKind::Var, span);
            Ok(Form::Expr(Expr::Atom(id)))
        }
        Sexp::List(span, mut sexps) => {
            match sexps.len() {
                0 => {
                    let id = env.add_record("()", AtomKind::Var, span);
                    Ok(Form::Expr(Expr::Atom(id)))
                }
                _ => {
                    let first = sexps.remove(0);

                    match first {
                        Sexp::List(span, sexps) => {
                            //
                            Err(unimplemented!())
                        }
                        Sexp::Atom(span, token) => {
                            if lang::is_special(&*token) {
                                parse_special_form(env, token, sexps, span)
                            } else {
                                // kind might be/need overwritten?
                                let id = env.add_record(token, AtomKind::Var, span);
                                let callee = Form::Expr(Expr::Atom(id));
                                let args = try!(sexps_to_forms(env, sexps));

                                Ok(Form::Expr(Expr::Inv(Box::new(callee), args)))
                            }
                        }
                    }
                }
            }
        }
    }
}

fn parse_special_form(env: &mut Env,
                      token: String,
                      args: Vec<Sexp>,
                      span: Span)
                      -> Result<Form, Box<Error>> {
    match &*token {
        "let" => parse_let(env, args, span),
        "if" => parse_if(env, args, span),
        "fn" => parse_fn(env, args, span),
        "do" => parse_do(env, args, span),
        "def" => parse_def(env, args, span),
        "ns" => parse_ns(env, args, span),
        "use" => parse_use(env, args, span),
        "macro" => parse_macro(env, args, span),
        "defreader" => parse_defreader(env, args, span),
        _ => unreachable!(),
    }
}

fn parse_bindings(env: &mut Env, sexp: Sexp) -> Result<Form, Box<Error>> {
    match sexp {
        Sexp::Atom(..) => Err("Bindings list must be a list".into()),
        Sexp::List(_, children) => {
            // assert that all children are suitable Atoms

            for child in children.iter() {
                try!(match *child {
                    Sexp::Atom(_, ref token) => {
                        if lang::is_special(token) {
                            Err(Box::<Error>::from(format!("Binding cannot be keyword: {}", token)))
                        } else {
                            Ok(())
                        }
                    }
                    Sexp::List(..) => Err(format!("Binding in list must be symbol").into()),
                });
            }

            Ok(Form::Expr(Expr::BindingsList(try!(sexps_to_forms(env, children)))))
        }
    }
}


fn parse_let(env: &mut Env, args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
    unimplemented!()
}

fn parse_if(env: &mut Env, args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
    unimplemented!()
}

fn parse_fn(env: &mut Env, mut args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
    // assert args >= 2 (param list, body...)
    if args.len() < 2 {
        return Err("fn needs at least 2 args".into());
    }

    args.push(Sexp::Atom(Span::new(0, 0), "do".to_owned()));

    let bindings_sexp = args.swap_remove(0);
    let body_sexps = args;

    let bindings_form = try!(parse_bindings(env, bindings_sexp));
    let body_form = try!(sexp_to_form(env, Sexp::List(span, body_sexps)));

    Ok(Form::Expr(Expr::FnExpr(Box::new(bindings_form), Box::new(body_form))))
}

fn parse_do(env: &mut Env, args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
    if args.is_empty() {
        return Err("do must have at least 1 arg".into());
    }

    // TODO: more asserts?

    Ok(Form::Expr(Expr::DoExpr(try!(sexps_to_forms(env, args)))))
}

fn parse_def(env: &mut Env, args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
    unimplemented!()
}

fn parse_ns(env: &mut Env, args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
    unimplemented!()
}

fn parse_use(env: &mut Env, args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
    unimplemented!()
}

fn parse_macro(env: &mut Env, args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
    unimplemented!()
}

fn parse_defreader(env: &mut Env, args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
    unimplemented!()
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
                                  -> ::std::result::Result<Option<Sexp>, ReadError>;

#[derive(Debug, Copy, Clone)]
pub struct Span {
    idx: usize,
    len: usize,
}

impl Span {
    pub fn new(idx: usize, len: usize) -> Self {
        Span {
            idx: idx,
            len: len,
        }
    }
}

pub enum Sexp {
    Atom(Span, String),
    List(Span, Vec<Sexp>),
}

/// Ported from the [Common Lisp HyperSpec](http://clhs.lisp.se/Body/02_b.htm)
fn read_token(stream: &mut InputStream, env: &mut Env) -> ::std::result::Result<Sexp, ReadError> {

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
                let span = Span::new(stream.pos(), token.len());
                return Ok(Sexp::Atom(span, token));
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
                           -> Result<Option<Sexp>, ReadError> {
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
                         -> Result<Option<Sexp>, ReadError> {

    let position = stream.pos() - 1;
    let mut list = vec![];

    loop {
        match stream.next() {
            Some(w) if env.lookup_syntax_type(w) == Whitespace => {}
            Some(c) if c == b')' => {
                let len = stream.pos() - position;
                let span = Span::new(position, len);
                return Ok(Some(Sexp::List(span, list)));
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
                          -> Result<Option<Sexp>, ReadError> {
    return Err(ReadError::other("Unexpected right parenthesis"));
}
