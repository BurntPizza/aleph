use itertools::*;

use std::error::Error;
use std::collections::HashMap;
use std::fmt::{self, Display, Debug, Formatter};

use self::CharSyntaxType::*;

pub struct Env {
    read_table: HashMap<char, CharSyntaxType>,
    reader_macros: HashMap<char, ReaderMacroFunction>,
}

impl Env {
    fn lookup_syntax_type(&self, c: u8) -> CharSyntaxType {
        self.read_table.get(&(c as char)).cloned().unwrap_or(CharSyntaxType::Invalid)
    }

    fn lookup_reader_macro(&self, c: u8) -> Option<ReaderMacroFunction> {
        self.reader_macros.get(&(c as char)).cloned()
    }

    fn new_reader_macro(&mut self, c: char, f: ReaderMacroFunction, type_: MacroCharType) {
        self.reader_macros.insert(c, f);
        self.read_table.insert(c, CharSyntaxType::MacroChar(type_));
    }
}

impl Default for Env {
    fn default() -> Self {
        let mut env = Env {
            read_table: HashMap::new(),
            reader_macros: HashMap::new(),
        };

        for k in 0..128u8 {
            let k = k as char;
            let v = match k {
                '_' | '-' | '+' | '*' | '&' => TokenChar,
                c if c.is_alphanumeric() => TokenChar,
                c if c.is_whitespace() => Whitespace,
                _ => continue,
            };

            env.read_table.insert(k, v);
        }

        env.new_reader_macro(';', ReaderMacroFunction(line_comment_reader), MacroCharType::Terminating);
        env.new_reader_macro('(', ReaderMacroFunction(left_paren_reader), MacroCharType::Terminating);
        env.new_reader_macro(')', ReaderMacroFunction(right_paren_reader), MacroCharType::Terminating);
        env.new_reader_macro('\'', ReaderMacroFunction(quote_reader), MacroCharType::Terminating);

        env
    }
}

pub fn read(env: &mut Env, stream: &mut InputStream) -> Result<Vec<Sexp>, Box<Error>> {
    let mut results = vec![];

    loop {
        match read_token(stream, env) {
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

// // TODO: assign actual scope ids (or does that happen during census?)
// // (think about how the macroexpansion loop affects this)
// fn parse_special_form(env: &mut Env,
//                       token: String,
//                       args: Vec<Sexp>,
//                       span: Span)
//                       -> Result<Form, Box<Error>> {
//     match &*token {
//         "let" => parse_let(env, args, span),
//         "if" => parse_if(env, args, span),
//         "fn" => parse_fn(env, args, span),
//         "do" => parse_do(env, args, span),
//         "def" => parse_def(env, args, span),
//         "ns" => parse_ns(env, args, span),
//         "use" => parse_use(env, args, span),
//         "macro" => parse_macro(env, args, span),
//         "defreader" => parse_defreader(env, args, span),
//         _ => unreachable!(),
//     }
// }

// fn assert_params_list_atoms(params: &[Sexp]) -> Result<(), Box<Error>> {
//     for param in params.iter() {
//         try!(match *param {
//             Sexp::Atom(_, ref token) => {
//                 if lang::is_special(token) {
//                     Err(Box::<Error>::from(format!("Param cannot be a special form: {}", token)))
//                 } else {
//                     Ok(())
//                 }
//             }
//             Sexp::List(..) => Err(format!("Param in list must be symbol").into()),
//         });
//     }
//     Ok(())
// }

// fn parse_params_list(env: &mut Env, sexp: Sexp) -> Result<Vec<Expr>, Box<Error>> {
//     match sexp {
//         Sexp::Atom(..) => Err("Params list must be a list".into()),
//         Sexp::List(_, children) => {
//             try!(assert_params_list_atoms(&*children));

// let exprs = forms_to_exprs(try!(sexps_to_forms(env, children)));

//             Ok(exprs)
//         }
//     }
// }

// fn parse_bindings(env: &mut Env, sexp: Sexp) -> Result<(Vec<Expr>, Vec<Expr>), Box<Error>> {
//     match sexp {
//         Sexp::Atom(..) => Err("Bindings list must be a list".into()),
//         Sexp::List(_, children) => {
//             let mut counter = 0;
//             let (params, values): (Vec<_>, Vec<_>) = children.into_iter()
//                                                              .partition(|_| {
//                                                                  counter += 1;
//                                                                  (counter - 1) % 2 == 0
//                                                              });

// try!(assert_params_list_atoms(&*params));

//             let params = forms_to_exprs(try!(sexps_to_forms(env, params)));
//             let values = forms_to_exprs(try!(sexps_to_forms(env, values)));

//             Ok((params, values))
//         }
//     }
// }

// fn parse_let(env: &mut Env, mut args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
//     // assert args >= 2 (bindings list, body...)
//     if args.len() < 2 {
//         return Err("let needs at least 2 args".into());
//     }

//     let bindings_sexp = args.remove(0);
//     let body_sexps = args;

//     let (params, values) = try!(parse_bindings(env, bindings_sexp));
//     let body_exprs = forms_to_exprs(try!(sexps_to_forms(env, body_sexps)));

//     Ok(Form::Expr(Expr::LetExpr(params, values, body_exprs)))
// }

// fn parse_if(env: &mut Env, mut args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
//     if args.len() != 3 {
//         return Err("if needs 3 args".into());
//     }

//     let cond_expr = form_to_expr(try!(sexp_to_form(env, args.remove(0))));
//     let then_expr = form_to_expr(try!(sexp_to_form(env, args.remove(0))));
//     let else_expr = form_to_expr(try!(sexp_to_form(env, args.remove(0))));

//     Ok(Form::Expr(Expr::IfExpr(Box::new(cond_expr),
//                                Box::new(then_expr),
//                                Box::new(else_expr))))
// }

// fn parse_fn(env: &mut Env, mut args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
//     // assert args >= 2 (param list, body...)
//     if args.len() < 2 {
//         return Err("fn needs at least 2 args".into());
//     }

//     let params_sexp = args.remove(0);
//     let body_sexps = args;

//     let params = try!(parse_params_list(env, params_sexp));
//     let body_exprs = forms_to_exprs(try!(sexps_to_forms(env, body_sexps)));

//     Ok(Form::Expr(Expr::FnExpr(params, body_exprs)))
// }

// fn parse_do(env: &mut Env, args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
//     if args.is_empty() {
//         return Err("do must have at least 1 arg".into());
//     }

// // TODO: more asserts?

//     Ok(Form::Expr(Expr::DoExpr(forms_to_exprs(try!(sexps_to_forms(env, args))))))
// }

// fn parse_def(env: &mut Env, mut args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
//     let (name, doc_string, value) = match args.len() {
//         2 => {
//             (try!(sexp_to_form(env, args.remove(0))),
//              None::<String>,
//              try!(sexp_to_form(env, args.remove(0))))
//         }
//         // TODO (waiting on string literals)
//         // 3 => (args.remove(0), args.remove(0), args.remove(0))
//         _ => return Err("def must have 2 args".into()),
//     };

//     match name {
//         Form::Expr(Expr::Atom(id)) => {
//             match value {
//                 Form::Expr(e) => {
//                     Ok(Form::Directive(Directive::Define {
//                         id: id,
//                         to_value: e,
//                     }))
//                 }
//                 _ => return Err("rhs of def must be an expression".into()),
//             }
//         }
//         _ => return Err("lhs of def must be a symbol".into()),
//     }
// }

// fn parse_ns(env: &mut Env, args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
//     unimplemented!()
// }

// fn parse_use(env: &mut Env, args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
//     unimplemented!()
// }

// fn parse_macro(env: &mut Env, args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
//     unimplemented!()
// }

// fn parse_defreader(env: &mut Env, args: Vec<Sexp>, span: Span) -> Result<Form, Box<Error>> {
//     unimplemented!()
// }

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

#[derive(Copy)]
pub struct ReaderMacroFunction(fn(&mut InputStream, &mut Env, u8)
                                  -> ::std::result::Result<Option<Sexp>, ReadError>);

impl Clone for ReaderMacroFunction {
    fn clone(&self) -> Self {
        *self
    }
}

pub fn new_node_id() -> usize {
    use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};
    static COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;

    COUNTER.fetch_add(1, Ordering::SeqCst)
}

#[derive(PartialEq, Debug, Clone)]
pub enum Sexp {
    Atom(String),
    List(Vec<Sexp>),
}

impl Sexp {
    // pub fn id(&self) -> usize {
    //     match *self {
    //         Sexp::Atom(..) |
    //         Sexp::List(..) => id,
    //     }
    // }
    // pub fn empty_atom() -> Self {
    //     Sexp::Atom(0, Span::new(0, 0), String::new())
    // }

    pub fn is_atom_of(&self, v: &str) -> bool {
        match *self {
            Sexp::Atom(ref s) => s == v,
            _ => false,
        }
    }

    pub fn extract_atom(self) -> Result<String, Self> {
        match self {
            Sexp::Atom(s) => Ok(s),
            _ => Err(self),
        }
    }

    pub fn extract_list(self) -> Result<Vec<Sexp>, Self> {
        match self {
            Sexp::List(s) => Ok(s),
            _ => Err(self),
        }
    }
}

impl Display for Sexp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Sexp::Atom(ref string) => write!(f, "{}", string),
            Sexp::List(ref children) => {
                write!(f,
                       "({})",
                       children.iter().map(ToString::to_string).join(" "))
            }
        }
    }
}

/// Ported from the [Common Lisp `HyperSpec`](http://clhs.lisp.se/Body/02_b.htm)
fn read_token(stream: &mut InputStream, env: &mut Env) -> ::std::result::Result<Sexp, ReadError> {

    use self::MacroCharType::*;
    use self::ReaderState::*;
    use self::ReadErrorType::*;

    let positition = stream.pos();
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
                                        match f.0(stream, env, x) {
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
                                TokenChar |
                                MacroChar(Nonterminating) => token.push(y),
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
                return Ok(Sexp::Atom(token));
            }
        }
    }
}


pub struct InputStream {
    src: String,
    idx: usize,
}

impl InputStream {
    pub fn pos(&self) -> usize {
        self.idx
    }

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

pub fn quote_reader(stream: &mut InputStream, env: &mut Env, _: u8) -> Result<Option<Sexp>, ReadError> {
    read_token(stream, env).map(|sexp| Some(Sexp::List(vec![Sexp::Atom("quote".into()), sexp])))
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
                return Ok(Some(Sexp::List(list)));
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
    Err(ReadError::other("Unexpected right parenthesis"))
}
