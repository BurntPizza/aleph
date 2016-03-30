
//! The Aleph Programming Langauge


#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]
#![cfg_attr(not(feature="clippy"), allow(unknown_lints))]


use std::collections::BTreeMap;

extern crate itertools;
extern crate hamt;

pub mod core;
pub mod repr;
pub mod reader;
pub mod analyzer;

use repr::Form;


pub struct Interpreter;

impl Interpreter {
    // signature?
    pub fn interpret<S: AsRef<str>>(input: S) {

        use std::fmt;
        use std::error;

        use repr::Ast;

        type Err = Box<error::Error>;

        trait Exec {
            fn exec(&self) -> Result<(), Err>;
        }

        #[derive(Debug)]
        enum Error {

        }

        impl fmt::Display for Error {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                unimplemented!()
            }
        }

        impl error::Error for Error {
            fn description(&self) -> &str {
                unimplemented!()
            }
        }


        fn read(input: &str) -> Result<Form, Err> {
            unimplemented!()
        }

        fn analyze(input: Form) -> Result<Ast, Err> {
            unimplemented!()
        }

        type TypedAst = (); // TODO
        type Bytecode = usize; // TODO

        fn typecheck(input: Ast) -> Result<TypedAst, Err> {
            unimplemented!()
        }

        fn compile(input: TypedAst) -> Result<Bytecode, Err> {
            unimplemented!()
        }

        fn exec<T: Exec>(input: T) -> Result<(), Err> {
            input.exec()
        }

        
        impl Exec for TypedAst {
            fn exec(&self) -> Result<(), Err> {
                unimplemented!()
            }
        }
        impl Exec for Bytecode {
            fn exec(&self) -> Result<(), Err> {
                unimplemented!()
            }
        }

        read(input.as_ref())
            .and_then(analyze)
            .and_then(typecheck)
         //   .and_then(compile)
            .and_then(exec)
            .unwrap();
    }
}

pub type Args<'a> = &'a [Form];
pub type Function = fn(Args) -> Result<Form, String>;


pub struct Environment {
    table: BTreeMap<String, Function>,
}

impl Default for Environment {
    fn default() -> Self {
        // use include! + macros?
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
            let (name, args) = v.split_at(1);

            if let Form::Atom(ref s) = name[0] {
                match env.table.get(&s.text) {
                    Some(f) => {
                        match f(args) {
                            Ok(form) => Ok(form),
                            Err(s) => Err(s),
                        }
                    }
                    None => Err(format!("No such function: `{}`", s.text)),
                }
            } else {
                panic!("first elem isn't an Atom: {:?}", &v[..]);
            }
        }
    }
}



// Note: guaranteed to be ASCII
#[derive(Debug, Clone)]
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
