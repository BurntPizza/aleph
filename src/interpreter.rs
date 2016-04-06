
use std::error::Error;

use repr::Form;
use reader;
use analyzer::{self, Analysis};

// TODO: return type
pub fn interpret<T: Into<String>>(input: T) -> String {
    let input = input.into();
    let ast = read(input).unwrap();
    let analysis = analyze(&ast).unwrap();

    analysis.exec().unwrap()

    //    read(input)
    //        .and_then(analyze)
    //        .and_then(typecheck)
    //        .and_then(compile)
    //        .and_then(exec)
    //        .unwrap()
    //        .to_string()
}

fn read(input: String) -> Result<Form, Err> {
    let mut reader = reader::ReaderEnv::new_default(super::InputStream::new(input));
    reader.read_all().map_err(|_| reader.last_error().into())
}

fn analyze(input: &Form) -> Result<Analysis, Err> {
    analyzer::analyze_from_root(&input).map_err(Into::into)
}

fn compile(input: TypedAst) -> Result<Bytecode, Err> {
    unimplemented!()
}

fn exec<T: Exec>(input: T) -> ExecResult {
    input.exec()
}

type TypedAst = (); // TODO
type Bytecode = usize; // TODO


// For now, just represent the result as a string
type ExecResult = Result<String, Err>;

// For testing until typecheck is implemented
impl<'a> Exec for analyzer::Analysis<'a> {
    fn exec(&self) -> ExecResult {
        unimplemented!()
    }
}

impl Exec for TypedAst {
    fn exec(&self) -> ExecResult {
        unimplemented!()
    }
}

impl Exec for Bytecode {
    fn exec(&self) -> ExecResult {
        unimplemented!()
    }
}

type Err = Box<Error>;

trait Exec {
    fn exec(&self) -> ExecResult;
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test1() {
        assert_eq!(interpret("()"), "()");
    }
}
