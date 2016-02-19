

// fix TODOs, panics, etc.

// need to figure out default readtable. TODO: keep going
// then reader macro table. TODO: figure out where it goes

// TODO
// namespaces: both the datastructure itself, and the DS for tracking them while parsing/analysing
// overall DS is a stack, right? each 'stack frame' is an NS.


// readtables ;; TODO: COW functionality
// scope system ;; needs first: (namespace system?) ;; No, other way around
//     scopes are tracked by a stack, the overall structure is a tree, rooted at the root scope
//     what are namespaces exactly, if we already have scopes?
//     just a set of public symbols? parent info?

// macro expander ;; needs first: (scope system)
// AST representation

// TODO start on core lib


#![warn(missing_docs)]

use std::collections::BTreeMap;

extern crate itertools;

pub mod core;
pub mod form;
pub mod reader;

use form::Form;

pub type Args<'a> = &'a [Form];
pub type Function = fn(Args) -> Result<Form, String>;

pub struct Environment {
    table: BTreeMap<String, Function>,
}

impl Default for Environment {
    fn default() -> Self {
        let mut table: BTreeMap<String, Function> = BTreeMap::new();
        table.insert("print".to_owned(), core::print);
        table.insert("println".to_owned(), core::println);
        Environment { table: table }
    }
}

// eval() placeholder
pub fn tmp_eval(env: &mut Environment, input: Form) -> Result<Form, String> {
    match input {
        Form::Atom(_) => Ok(input),
        Form::List(ref v) if v.is_empty() => Ok(input.clone()),
        Form::List(ref v) => {
            let elems = v.as_slices().0;
            let name = &elems[0];
            let args = &elems[1..];

            if let &Form::Atom(ref s) = name {
                match env.table.get(s) {
                    Some(f) => {
                        match f(args) {
                            Ok(form) => Ok(form),
                            Err(s) => Err(s),
                        }
                    }
                    None => Err(format!("No such function: `{}`", s)),
                }
            } else {
                panic!("first elem isn't an Atom: {:?}", elems);
            }
        }
    }
}



/// Note: guaranteed to be ASCII
pub struct InputStream {
    src: String,
    idx: usize,
}

impl InputStream {
    pub fn new(src: String) -> Self {
        assert!(std::ascii::AsciiExt::is_ascii(&*src));
        InputStream { src: src, idx: 0 }
    }

    fn unread(&mut self) {
        assert!(self.idx > 0);
        self.idx -= 1;
    }

    fn rewind(&mut self) {
        self.idx = 0;
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


// Next: impl read() and organize reader module
// then: analyze() & AST (just basic type system)
//
// Get something working. Stop thinking so hard.
// Don't need to implement namespaces to have basic interpretation going on.
// ITERATE!
//
//
