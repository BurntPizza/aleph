

use std::fmt::{self, Display, Formatter};
use std::collections::VecDeque;

use itertools::*;

use self::Form::*;

/// Lexical program representation: untyped s-expressions.
#[derive(Debug, PartialEq, Clone)]
pub enum Form {
    /// A token, such as an identifier, number, or anything that isn't a `List`.
    Atom(String),
    /// A list of `Form`s, usually delimited by parentheses.
    List(VecDeque<Form>),
}

impl Form {
    /// Construct an Atom containing a String
    pub fn atom(s: String) -> Self {
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
        List(VecDeque::new())
    }


    pub fn add_to_list(&mut self, item: Form) {
        match *self {
            Atom(_) => panic!(),
            List(ref mut l) => l.push_back(item)
        }
    }
}

impl Display for Form {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let s = match *self {
            Atom(ref s) => s.clone(),
            List(ref list) => format!("({})", list.iter().join(" ")),
        };
        write!(f, "{}", s)
    }
}


// AST node types for each special form, right? see wiki diagram for AST
// some nodes contain type info (or at least slots for it)
// a Form -> AST fn
// do modules go in the AST?
enum Ast {
    // Special Forms
    Def,
    Quote,
    Fn,
    Macro,
    Cond,
    //? Let,
    
}


// constraint graph? each edge has a vector of contraints relating to the nodes it connects?
// might need to be a hypergraph...


// also Identifier vs Literal vs FnCall
// Literals map to FnCalls? (macros?) (compiler macros?)


// so what are the special forms?

/*

environment:
Reader env.
Analyzer env.



*/
