
use std::error::Error;

use repr::{Form, Ast};
use reader;
use analyzer;

pub fn interpret(input: String) {
    read(input)
        .and_then(analyze)
        .and_then(typecheck)
        .and_then(exec)
        .unwrap();
}

fn read(input: String) -> Result<Form, Err> {
    let mut reader = reader::ReaderEnv::new_default(super::InputStream::new(input));
    reader.read_all().map_err(|_| reader.last_error().into())
}

fn analyze(input: Form) -> Result<Ast, Err> {
    analyzer::analyze_from_root(input).map_err(Into::into)
}

fn typecheck(input: Ast) -> Result<TypedAst, Err> {
    unimplemented!()
}

fn compile(input: TypedAst) -> Result<Bytecode, Err> {
    unimplemented!()
}

fn exec<T: Exec>(input: T) -> Result<(), Err> {
    input.exec()
}

type TypedAst = (); // TODO
type Bytecode = usize; // TODO





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

type Err = Box<Error>;

trait Exec {
    fn exec(&self) -> Result<(), Err>;
}
