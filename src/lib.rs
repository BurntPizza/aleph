
//!The Aleph Programming Langauge

#![warn(missing_docs)]

#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]
#![cfg_attr(not(feature="clippy"), allow(unknown_lints))]


use std::collections::BTreeMap;

extern crate itertools;

pub mod core;
pub mod form;
pub mod reader;

use form::Form;

/// 
pub type Args<'a> = &'a [Form];
///
pub type Function = fn(Args) -> Result<Form, String>;

///
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

/// eval() placeholder
pub fn tmp_eval(env: &mut Environment, input: Form) -> Result<Form, String> {
    match input {
        Form::Atom(_) => Ok(input),
        Form::List(ref v) if v.is_empty() => Ok(Form::empty_list()),
        Form::List(ref v) => {
            let elems = v.as_slices().0;
            let name = &elems[0];
            let args = &elems[1..];

            if let Form::Atom(ref s) = *name {
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
#[derive(Clone)]
pub struct InputStream {
    src: String,
    idx: usize,
}

impl InputStream {
    ///
    pub fn new(src: String) -> Self {
        assert!(std::ascii::AsciiExt::is_ascii(&*src));
        InputStream { src: src, idx: 0 }
    }

    fn unread(&mut self) {
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
