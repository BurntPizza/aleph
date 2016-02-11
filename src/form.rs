
use std::fmt::{self, Display, Formatter};
use std::collections::VecDeque;

use itertools::*;

use self::Form::*;

/// Lexical program representation
#[derive(Debug)]
pub enum Form {
    Atom(String),
    List(VecDeque<Form>),
}

impl Form {
    pub fn atom(s: String) -> Self {
        Atom(s)
    }

    pub fn list<I>(src: I) -> Self
        where I: IntoIterator<Item = Form>
    {
        List(src.into_iter().collect())
    }
}

impl Display for Form {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f,
               "{}",
               match *self {
                   Atom(ref s) => s.clone(),
                   List(ref list) => format!("({})", list.iter().join(" ")),
               })
    }
}
