
use itertools::*;

use std::error::Error;

use reader::{self, Form, InputStream};
use analyzer::{self, Analysis, AstNode};
use symbol_table::*;
use vm::*;


// TODO: return type
pub fn interpret<T: Into<String>>(input: T) -> String {
    let input = input.into();
    let form = read(input).unwrap();
    let analysis = analyze(form).unwrap();
    let program = compile(analysis).unwrap();

    exec(program)

    //    read(input)
    //        .and_then(analyze)
    //        .and_then(typecheck)
    //        .and_then(compile)
    //        .and_then(exec)
    //        .unwrap()
    //        .to_string()
}

fn read(input: String) -> Result<Vec<Form>, Box<Error>> {
    let mut reader = reader::ReaderEnv::new_default(InputStream::new(input));
    reader.read_all().map_err(|_| reader.last_error().into())

}
fn analyze(input: Vec<Form>) -> Result<Analysis, Box<Error>> {
    analyzer::analyze_from_root(input).map_err(Into::into)
}

fn compile(input: Analysis) -> Result<Program, Box<Error>> {
    fn emit_const(p: &mut ProgramBuilder, val: i64) -> Result<(), Box<Error>> {
        p.load_const(val);
        Ok(())
    }

    fn emit_var(p: &mut ProgramBuilder, var_id: u32, env: &SymbolTable) -> Result<(), Box<Error>> {
        let var_record = env.lookup_id(var_id).unwrap();

        unimplemented!()
    }

    fn emit_do(p: &mut ProgramBuilder,
               env: &SymbolTable,
               args: &[AstNode])
               -> Result<(), Box<Error>> {
        let (last, others) = args.split_last().expect("emit_do");

        for arg in others {
            try!(emit(arg, env, p));
            p.drop();
        }
        emit(last, env, p)
    }

    fn emit_inv(p: &mut ProgramBuilder,
                callee: &AstNode,
                args: &[AstNode],
                env: &SymbolTable)
                -> Result<(), Box<Error>> {


        match *callee {
            AstNode::Var(id) => {
                let var_record = env.lookup_id(id).unwrap();
                match *var_record.kind() {
                    VarKind::Normal => {
                        for arg in args {
                            try!(emit(arg, env, p));
                        }
                        emit(callee, env, p)
                    }
                    VarKind::Special => {
                        match var_record.ident() {
                            "do" => emit_do(p, env, args),
                            "+" => {
                                assert!(args.len() < 256);

                                for arg in args {
                                    try!(emit(arg, env, p));
                                }

                                p.add(args.len() as u8);
                                Ok(())
                            } 
                            _ => unimplemented!(),
                        }
                    }
                }
            }
            _ => {
                for arg in args {
                    try!(emit(arg, env, p));
                }
                emit(callee, env, p)
            }
        }
    }

    fn emit(node: &AstNode, env: &SymbolTable, p: &mut ProgramBuilder) -> Result<(), Box<Error>> {
        match *node {
            AstNode::Const(val) => emit_const(p, val),
            AstNode::Var(id) => emit_var(p, id, env),
            AstNode::Inv(ref callee, ref args) => emit_inv(p, callee, args, env),
        }
    }

    let mut program = new_program();
    try!(emit(input.ast(), input.env(), &mut program));
    program.exit();

    let program = program.finish();

    println!("Program: {:?}", program);
    Ok(program)
}

fn exec(input: Program) -> String {
    format!("{}", exec_program(input))
}


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
