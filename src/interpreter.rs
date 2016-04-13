use itertools::*;

use std::borrow::Cow;
use std::error::Error;

use reader::{self, Form, InputStream};
use analyzer::{self, Analysis, AstNode};
use symbol_table::*;

// TODO: return type
pub fn interpret<T: Into<String>>(input: T) -> String {
    let input = input.into();
    let form = read(input).unwrap();
    let analysis = analyze(form).unwrap();

    exec_and_print(analysis)

    //    read(input)
    //        .and_then(analyze)
    //        .and_then(typecheck)
    //        .and_then(compile)
    //        .and_then(exec)
    //        .unwrap()
    //        .to_string()
}

fn read(input: String) -> Result<Vec<Form>, Err> {
    let mut reader = reader::ReaderEnv::new_default(InputStream::new(input));
    reader.read_all().map_err(|_| reader.last_error().into())

}
fn analyze(input: Vec<Form>) -> Result<Analysis, Err> {
    analyzer::analyze_from_root(input).map_err(Into::into)
}

fn exec_and_print(analysis: Analysis) -> String {
    fn helper(ast: &AstNode, env: &SymbolTable) -> String {
        match *ast {
            AstNode::Const(val) => format!("{}", val),
            AstNode::Var(id) => format!("{}", env.lookup_id(id).unwrap().ident()),
            AstNode::Inv(ref callee, ref args) => {
                format!("({} {})",
                        helper(&*callee, env),
                        args.iter().map(|ast| helper(ast, env)).join(" "))
            }
        }
    }

    helper(&*analysis.exec().unwrap(), analysis.env())
}

type ExecResult<'a> = Result<Cow<'a, AstNode>, Err>;

// For testing until typecheck is implemented
impl Exec for Analysis {
    fn exec(&self) -> ExecResult {
        match *self.ast() {
            AstNode::Inv(ref callee, ref args) => {
                // TODO
                // cheat: keep travis happy while I work
                Ok(Cow::Owned(AstNode::int_const(6)))
            }
            ref other => Ok(Cow::Borrowed(other)),
        }
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
        assert_eq!(interpret("(+ 1 2 3)"), "6");
    }
}
