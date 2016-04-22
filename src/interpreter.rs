
use std::error::Error;
use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};

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
    fn emit_const(p: &mut ProgramBuilder, val: i64) -> Result<bool, Box<Error>> {
        p.load_i64(val);
        Ok(true)
    }

    fn emit_var(p: &mut ProgramBuilder,
                var_id: u32,
                env: &SymbolTable)
                -> Result<bool, Box<Error>> {
        let var_record = env.lookup_id(var_id).unwrap();

        match *var_record.kind() {
            VarKind::Var => {
                p.load_var(var_id);
                Ok(true)
            }
            _ => unimplemented!(),
        }
    }

    fn emit_do(p: &mut ProgramBuilder,
               env: &SymbolTable,
               args: &[AstNode])
               -> Result<bool, Box<Error>> {
        let (last, others) = args.split_last().expect("emit_do");

        for arg in others {
            // if something was left on the stack, clean it up
            if try!(emit(arg, env, p)) {
                p.pop();
            }
        }

        emit(last, env, p)
    }

    fn emit_def(p: &mut ProgramBuilder,
                env: &SymbolTable,
                args: &[AstNode])
                -> Result<bool, Box<Error>> {
        assert_eq!(args.len(), 2);

        match args[0] {
            AstNode::Var(id) => {
                match args[1] {
                    AstNode::Const(val) => {
                        p.def_var(id);
                        p.load_i64(val);
                        p.store_var(id);
                    }
                    AstNode::Var(val_id) => {
                        p.def_var(id);
                        p.load_var(val_id);
                        p.store_var(id);
                    }
                    AstNode::Inv(ref callee, ref args) => {
                        p.def_var(id);
                        emit_inv(p, callee, args, env).unwrap();
                        p.store_var(id);
                    }
                }
            }
            _ => return Err("1st arg of `def` must be a var".into()),
        }

        Ok(false) // def never puts anything on the stack (TODO: nil?)
    }

    fn emit_fn(p: &mut ProgramBuilder,
               env: &SymbolTable,
               args: &[AstNode])
               -> Result<bool, Box<Error>> {

        fn next_fn_id() -> u32 {
            static COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;

            COUNTER.fetch_add(1, Ordering::SeqCst) as u32
        }

        assert!(args.len() > 1);
        let (arg_list, body) = args.split_first().unwrap();

        match *arg_list {
            AstNode::Inv(..) => {}
            _ => return Err("Malformed argument list".into()),
        }

        // return ref to function? (addr?)
        let id = next_fn_id();
        p.begin_fn_def(id);

        // emit param marshalling header
        if let AstNode::Inv(ref callee, ref args) = *arg_list {
            for arg in ::std::iter::once(&**callee).chain(args.iter()) {
                match *arg {
                    AstNode::Var(arg_id) => {
                        p.def_var(arg_id);
                        p.store_var(arg_id);
                    }
                    _ => return Err("Argument lists may only contain vars".into()),
                }
            }
        }

        // emit body, wrapped in `do` form
        try!(emit_do(p, env, body));
        p.ret();

        p.end_fn_def();
        Ok(true)
    }

    fn emit_inv(p: &mut ProgramBuilder,
                callee: &AstNode,
                args: &[AstNode],
                env: &SymbolTable)
                -> Result<bool, Box<Error>> {


        match *callee {
            AstNode::Var(id) => {
                let var_record = env.lookup_id(id).unwrap();
                match *var_record.kind() {
                    VarKind::Var => {
                        for arg in args {
                            try!(emit(arg, env, p));
                        }

                        p.load_var(id);
                        p.call();
                        Ok(true) // TODO: not all fns return a value
                    }
                    VarKind::NormalFn => {
                        unimplemented!();
                        // for arg in args {
                        //     try!(emit(arg, env, p));
                        // }
                        // emit(callee, env, p)
                    }
                    VarKind::SpecialFn => {
                        match var_record.ident() {
                            "def" => emit_def(p, env, args),
                            "fn" => emit_fn(p, env, args),
                            "do" => emit_do(p, env, args),
                            "+" => {
                                assert!(args.len() < 256);

                                for arg in args {
                                    try!(emit(arg, env, p));
                                }

                                p.add(args.len() as u8);
                                Ok(true)
                            }
                            _ => unimplemented!(),
                        }
                    }
                    VarKind::Constant => Err("cannot invoke constant".into()),
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

    fn emit(node: &AstNode, env: &SymbolTable, p: &mut ProgramBuilder) -> Result<bool, Box<Error>> {
        match *node {
            AstNode::Const(val) => emit_const(p, val),
            AstNode::Var(id) => emit_var(p, id, env),
            AstNode::Inv(ref callee, ref args) => emit_inv(p, callee, args, env),
        }
    }

    let mut program = new_program();
    assert!(try!(emit(input.ast(), input.env(), &mut program)),
            "nothing will be on the stack");
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

    #[test]
    fn def_const_var() {
        assert_eq!(interpret("(def x 7) x"), "7");
    }

    #[test]
    fn add_const_vars() {
        assert_eq!(interpret("(def x 3) (def y 4) (+ x 7 y)"), "14");
    }

    #[test]
    fn copy_var() {
        assert_eq!(interpret("(def x 3) (def y x) y"), "3");
    }

    #[test]
    fn def_fn() {
        assert_eq!(interpret("(def f (fn (x) (+ 1 x x))) (f 7)"), "15");
    }
}
