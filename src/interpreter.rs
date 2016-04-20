
use itertools::*;

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

    let result = exec_analysis(&analysis).unwrap();

    helper(&result, analysis.env())
}

pub type ExecResult = Result<AstNode, Box<Error>>;

// For testing until typecheck is implemented
fn exec_analysis(analysis: &Analysis) -> ExecResult {
    let ast = analysis.ast();
    let env = analysis.env();

    exec_ast(ast, env)
}

pub fn exec_ast(ast: &AstNode, env: &SymbolTable) -> ExecResult {
    match *ast {
        AstNode::Inv(ref callee, ref args) => {
            match **callee {
                AstNode::Var(var_id) => {
                    // TODO
                    let var_record = env.lookup_id(var_id).unwrap();

                    match *var_record.kind() {
                        VarKind::Special(func) => func(args, env),
                        VarKind::Normal => Err(unimplemented!()),
                    }
                }
                AstNode::Inv(..) => unimplemented!(),
                AstNode::Const(_) => Err("Cannot invoke constant".into()),
            }
        }
        ref other => Ok(other.clone()),
    }
}

type Err = Box<Error>;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn add_int_constants() {
        assert_eq!(interpret("(+ 1 2 3)"), "6");
    }

    #[test]
    fn nested_add_int_constants() {
        assert_eq!(interpret("(+ (+) (+ 1 (+ 1 1)) (+ 1 2))"), "6");
    }
}
