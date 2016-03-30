

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


// AST node types for each special form, right? see wiki diagram for AST
// some nodes contain type info (or at least slots for it)
// a Form -> AST fn
// do modules go in the AST?

// pub enum Ast {
//     // Special Forms
//     Def,
//     Quote,
//     Fn,
//     Macro,
//     Cond,

//     List(VecDeque<Ast>),
// }

#[derive(Debug)]
pub enum Ast {
    Atom(Val),
    Invoke(Vec<Ast>),
}

pub enum Type {
    T_,
}

// Index into global interpreter datastore
// (I think)
#[derive(PartialEq, Debug)]
pub struct Val(u64);

pub const NULL_VAL: Val = Val(0);

impl Ast {
    // Constructors:

    pub fn atom(v: Val) -> Self {
        Ast::Atom(v)
    }

    pub fn invoke<I: IntoIterator<Item = Ast>>(itr: I) -> Self {
        Ast::Invoke(itr.into_iter().collect())
    }

    // Utilities:

    pub fn get_val_id(&self) -> &Val {
        match *self {
            Ast::Atom(ref val) => val,
            _ => self.is_not("an Atom"),
        }
    }

    pub fn list_of<I: IntoIterator<Item = Ast>>(itr: I) -> Self {
        use self::Ast::*;

        Invoke(::std::iter::once(Atom(// get_val_id!(core::list)
                                      unimplemented!()))
                   .chain(itr.into_iter())
                   .collect())
    }

    pub fn add_to_list(&mut self, item: Ast) {
        use self::Ast::*;

        match *self {
            Invoke(ref mut v) if *v.get(0).map_or(&NULL_VAL, Ast::get_val_id) ==
                                 *Ast::get_val_id(// core::list
                                                  unimplemented!()) => v.push(item),
            _ => self.is_not("a list"),
        }
    }

    pub fn list_plus(mut self, item: Ast) -> Self {
        self.add_to_list(item);
        self
    }

    fn is_not<S: AsRef<str>>(&self, msg: S) -> ! {
        panic!("{:?} is not {}!", self, msg.as_ref())
    }
}
