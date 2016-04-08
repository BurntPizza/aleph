

use std::fmt::{self, Display, Formatter};

use itertools::*;

use self::Form::*;
use reader::Span;

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
        Atom(s)
    }
    /// Construct a list of forms
    pub fn list<I>(src: I) -> Self
        where I: IntoIterator<Item = Form>
    {
        List(src.into_iter().collect())
    }
    /// Construct a List form containing nothing
    pub fn empty_list() -> Self {
        List(vec![])
    }


    pub fn add_to_list(&mut self, item: Form) {
        match *self {
            Atom(_) => panic!(),
            List(ref mut l) => l.push(item),
        }
    }
}

impl Display for Form {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let s = match *self {
            Atom(ref s) => s.text.clone(),
            List(ref list) => format!("({})", list.iter().join(" ")),
        };
        write!(f, "{}", s)
    }
}

// includes special forms
pub enum AstNode {
    Ident,
    Do,
    Fn,
}
