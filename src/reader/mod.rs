

use std::fmt::{self, Display, Debug, Formatter};
use std::ops::Index;
use std::error;


mod stream;
use self::stream::{Char, CharType, CharStream};


use self::Ast::*;

// TODO: what is nessesary ast-wise for typechecking, inference, and macro expansion?
// and error reporting

#[derive(Debug, PartialEq, Clone)]
pub enum AtomKind {
    Ident(String),
    Integer(i64),
}

/// The AST node representation.
#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    Atom(AtomKind),

    // TODO: VecDeque?
    List(Vec<Ast>),
}

impl Ast {
    fn ident(s: String) -> Self {
        Atom(AtomKind::Ident(s))
    }

    fn integer(v: i64) -> Self {
        Atom(AtomKind::Integer(v))
    }
}

impl Display for Ast {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Atom(ref k) => write!(f, "{}", k),
            List(ref v) => {
                let mut s: String = v.iter().map(|ast| format!("{} ", ast)).collect();
                s.pop();
                write!(f, "({})", s)
            }
        }
    }
}

impl Display for AtomKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use self::AtomKind::*;

        match *self {
            Ident(ref s) => write!(f, "{}", s),
            Integer(v) => write!(f, "{}", v),
        }
    }
}


#[derive(Debug, PartialEq, Copy, Clone)]
pub enum MacroCharType {
    Term,

    #[allow(dead_code)]
    NonTerm,
}


#[derive(PartialEq, Copy, Clone)]
pub enum CharSyntaxType {
    Invalid,
    Whitespace,
    MacroChar(MacroCharType),

    #[allow(dead_code)]
    SingleEscape,

    #[allow(dead_code)]
    MultEscape,
    TokenChar,
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

pub struct ReadTable {
    table: Vec<CharSyntaxType>,
}

impl Default for ReadTable {
    fn default() -> Self {
        use self::CharSyntaxType::*;
        use self::MacroCharType::*;

        ReadTable {
            table: (0..128u8)
                       .map(|c| {
                           let c = c as char;
                           match c {
                               '(' | ')' | ';' => MacroChar(Term),
                               '_' | '-' => TokenChar,
                               _ if c.is_alphanumeric() => TokenChar,
                               _ if c.is_whitespace() => Whitespace,
                               _ => Invalid,
                           }
                       })
                       .collect(),
        }
    }
}

impl Index<Char> for ReadTable {
    type Output = CharSyntaxType;

    fn index(&self, idx: Char) -> &Self::Output {
        &self.table[*idx as usize]
    }
}

impl Index<char> for ReadTable {
    type Output = CharSyntaxType;

    fn index(&self, idx: char) -> &Self::Output {
        &self.table[idx as usize]
    }
}

impl Debug for ReadTable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_map()
         .entries((0..self.table.len()).map(|i| i as CharType as char).zip(self.table.iter()))
         .finish()
    }
}


#[derive(Debug)]
enum ReadErrorType {
    EOS,
    EmptyToken, // merge these?
    InvalidToken(String),
    InvalidChar(Char),
    NoMacro(Char),
    Other(String),
}


#[derive(Debug)]
struct ReadError {
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

    fn invalid_char(c: Char) -> Self {
        ReadErrorType::InvalidChar(c).into()
    }

    fn no_macro_for_char(c: Char) -> Self {
        ReadErrorType::NoMacro(c).into()
    }

    fn other<S: Into<String>>(msg: S) -> Self {
        ReadErrorType::Other(msg.into()).into()
    }
}

impl From<ReadErrorType> for ReadError {
    fn from(src: ReadErrorType) -> Self {
        use self::ReadErrorType::*;

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

impl Display for ReadError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", (self as &error::Error).description())
    }
}

impl error::Error for ReadError {
    fn description(&self) -> &str {
        &self._desc
    }
}


#[derive(Debug, Copy, Clone)]
enum ReaderState {
    Reading,
    Accumulating,
    Escaping,
    TokenFinished,
}



fn line_comment_reader<I>(_: &mut Reader<I>,
                          stream: &mut CharStream<I>,
                          _: Char)
                          -> Result<Option<Ast>, ReadError>
    where I: Iterator<Item = Char>
{
    loop {
        match stream.next() {
            Some(c) if *c == b'\n' => break,
            Some(_) => {}
            _ => break,
        }
    }
    Ok(None)
}

fn left_paren_reader<I>(reader: &mut Reader<I>,
                        stream: &mut CharStream<I>,
                        _: Char)
                        -> Result<Option<Ast>, ReadError>
    where I: Iterator<Item = Char>
{
    use self::CharSyntaxType::*;

    let mut list = vec![];

    loop {
        match stream.next() {
            Some(w) if reader.readtable[w] == Whitespace => {}
            Some(c) if *c == b')' => return Ok(Some(List(list))),
            Some(c) => {
                stream.put_back(c);
                list.push(try!(reader.read(stream)));
            }
            _ => {
                // TODO format!("Unexpected end of stream while reading list: `{:?}`", list)
                return Err(ReadError::eos());
            }
        }
    }
}

fn right_paren_reader<I>(_: &mut Reader<I>,
                         _: &mut CharStream<I>,
                         _: Char)
                         -> Result<Option<Ast>, ReadError>
    where I: Iterator
{
    Err(ReadError::other("Unexpected right parenthesis"))
}


struct MacroTable<I: Iterator<Item = Char>> {
    table: Vec<Option<fn(&mut Reader<I>, &mut CharStream<I>, Char)
                         -> Result<Option<Ast>, ReadError>>>,
}

impl<I: Iterator<Item = Char>> Default for MacroTable<I> {
    fn default() -> Self {
        let mut mt = MacroTable { table: (0..128).map(|_| None).collect() };
        mt.table[b'\n' as usize] = Some(line_comment_reader);
        mt.table[b'(' as usize] = Some(left_paren_reader);
        mt.table[b')' as usize] = Some(right_paren_reader);
        mt
    }
}



trait FSM<I, O, E> {
    fn init() -> Self;
    fn iter(self, I) -> Self;
    fn result(self) -> Result<O, E>;
}

// TODO add more
enum TokenParser {
    Init,
    Minus,
    Numeric(i8, String),
    Symbol(String),
    Error(ReadError),
}

impl FSM<Char, Ast, ReadError> for TokenParser {
    fn init() -> Self {
        TokenParser::Init
    }

    fn iter(self, i: Char) -> Self {
        use self::TokenParser::*;

        match self {
            Init => {
                match *i as char {
                    '-' => Minus,
                    c @ '0'...'9' => {
                        let mut s = String::with_capacity(4);
                        s.push(c);
                        Numeric(1, s)
                    }
                    c => {
                        let mut s = String::with_capacity(6);
                        s.push(c);
                        Symbol(s)
                    }
                }
            }
            Minus => {
                let mut s = String::with_capacity(4);
                match *i as char {
                    c @ '0'...'9' => {
                        s.push(c);
                        Numeric(-1, s)
                    }
                    c => {
                        s.push('-');
                        s.push(c);
                        Symbol(s)
                    }
                }
            }
            Numeric(s, mut m) => {
                match *i as char {
                    c @ '0'...'9' => {
                        m.push(c);
                        Numeric(s, m)
                    }
                    _ => Error(ReadError::invalid_char(i)),
                }
            }
            Symbol(mut v) => {
                v.push(*i as char);
                Symbol(v)
            }
            err => err,
        }
    }

    fn result(self) -> Result<Ast, ReadError> {
        use self::TokenParser::*;
        match self {
            // treat as symbol
            Minus => Ok(Ast::ident("-".to_owned())),
            Symbol(v) => Ok(Ast::ident(v)),
            Numeric(s, m) => {
                match m.parse::<u32>() {
                    Ok(m) => Ok(Ast::integer(s as i64 * m as i64)),
                    Err(e) => Err(ReadError::other(format!("{}", e))),
                }
            }
            Error(e) => Err(e),
            Init => Err(ReadError::empty_token()),
        }
    }
}

// TODO should macro mappings be stored in the readtable?
struct Reader<I>
    where I: Iterator<Item = Char>
{
    readtable: ReadTable,
    macros: MacroTable<I>,
}

impl<I: Iterator<Item = Char>> Default for Reader<I> {
    fn default() -> Self {
        Reader {
            readtable: ReadTable::default(),
            macros: MacroTable::default(),
        }
    }
}

// is input verified as ASCII?


impl<I: Iterator<Item = Char>> Reader<I> {
    // TODO
    fn interpret_token(&self, input: Vec<Char>) -> Result<Ast, ReadError> {
        input.into_iter().fold(TokenParser::init(), TokenParser::iter).result()
    }

    fn read(&mut self, stream: &mut CharStream<I>) -> Result<Ast, ReadError> {
        use self::ReaderState::*;
        use self::CharSyntaxType::*;
        use self::MacroCharType::*;

        let mut token = Vec::with_capacity(8);
        let mut state = Reading;

        // step 1
        loop {
            match state {
                Reading => {
                    match stream.next() {
                        Some(x) => {
                            match self.readtable[x] {
                                Invalid => return Err(ReadError::invalid_char(x)),
                                Whitespace => {} // repeat step 1
                                MacroChar(_) => {
                                    match self.macros.table[*x as usize] {
                                        Some(f) => {
                                            match f(self, stream, x) {
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
                            return if token.is_empty() {
                                Err(ReadError::empty_token())
                            } else {
                                Err(ReadError::eos())
                            }
                        }
                    }
                }
                // step 8
                Accumulating => {
                    loop {
                        match stream.next() {
                            Some(y) => {
                                match self.readtable[y] {
                                    Invalid => return Err(ReadError::invalid_char(y)),
                                    // enter step 10
                                    Whitespace => {
                                        state = TokenFinished;
                                        // TODO is this right?
                                        // stream.unread(); // preserve whitespace
                                        break;
                                    }
                                    // enter step 10
                                    MacroChar(Term) => {
                                        stream.put_back(y);
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
                        match stream.next() {
                            Some(y) => {
                                match self.readtable[y] {
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
                TokenFinished => return self.interpret_token(token),
            }
        }
    }
}

/// Parse one AST node from text.
pub fn parse<T: AsRef<str>>(input: T) -> Ast {
    let input = input.as_ref();
    println!("Text: `{}`", input);
    let mut stream = CharStream::new(input.char_indices().map(|(i, c)| Char::from((i, c as u8))));
    let ast = Reader::default().read(&mut stream).unwrap_or_else(|e| panic!("~Error: {}", e));
    println!("Stream: `{}`",
             stream.map(|c| *c as char).collect::<String>());
    ast
}

// fn as_char_stream<I, C, S>(src: S) -> CharStream<I>
//     where I: Iterator<Item = Char>,
//           S: AsRef<str>
// {
//     let src = src.as_ref();
//     CharStream::new(src.char_indices().map(|(i, c)| Char::from((i, c as u8))))
// }


#[test]
fn left_paren_reader_test() {
    let input = "(() ((  ) ( ) ) (a b  c ( hello   world ) ))";
    // (() (() ()) (a b c (hello world)))
    let expected = List(vec![List(vec![]),
                             List(vec![List(vec![]), List(vec![])]),
                             List(vec![Ast::ident("a".to_owned()),
                                       Ast::ident("b".to_owned()),
                                       Ast::ident("c".to_owned()),
                                       List(vec![Ast::ident("hello".to_owned()),
                                                 Ast::ident("world".to_owned())])])]);

    let mut stream = CharStream::new(input.char_indices()
                                          .map(|(i, c)| Char::from((i, c as u8))));
    let ast = Reader::default().read(&mut stream).expect("read failed");

    println!("{:?}", ast);

    assert_eq!(stream.count(), 0);
    assert_eq!(ast, expected);
}

#[test]
fn line_comment_reader_test() {
    let input = "Hello, World\nhow are you?";
    let expected = "how are you?";

    let mut stream = CharStream::new(input.char_indices()
                                          .map(|(i, c)| Char::from((i, c as u8))));

    // doesn't use first argument
    assert_eq!(None,
               line_comment_reader(unsafe { ::std::mem::uninitialized() },
                                   &mut stream,
                                   Char::from(';'))
                   .unwrap());

    let leftover: String = stream.map(|c| *c as char).collect();

    assert_eq!(leftover, expected);
}

fn test_parse<S: AsRef<str>>(src: S) -> Result<Ast, ReadError> {
    Reader::default().read(&mut CharStream::new(src.as_ref()
                                                   .char_indices()
                                                   .map(|(i, c)| Char::from((i, c as u8)))))
}

// TODO actual tests
#[test]
fn token_parse_test() {
    println!("{:?}", test_parse("hello"));
    println!("{:?}", test_parse("_hello"));
    println!("{:?}", test_parse("1234"));
    println!("{:?}", test_parse("-123"));
    println!("{:?}", test_parse("123f"));
}

#[test]
fn minus_token_print_test() {
    let input = "-";
    let output = parse(input);
    println!("{:?}", output);
    assert_eq!(input, format!("{}", output));
}

#[test]
fn minus_formatting_test() {
    let input = "-";
    assert_eq!(input, format!("{}", input));
    assert_eq!(input, format!("{}", format!("{}", input)));
}
