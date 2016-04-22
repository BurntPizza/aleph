
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
    // use scopes!
    // no special 'top-level' anonymous scope
    // walk tree and visit nodes:
    // - fn nodes: emit function in text section, 'value' of the node is... the addr? a special NormalFn kind?
    //
    //

    // some things are not known until post-assembly/linking, primarily fn addresses

    unimplemented!()
    // let mut program = new_program();
    // assert!(try!(emit(input.ast(), input.env(), &mut program)),
    //         "nothing will be on the stack");
    // program.exit();

    // let program = program.finish(input.env());

    // println!("Program: {:?}", program);
    // Ok(program)
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

    #[test]
    fn nested_def_fn() {
        assert_eq!(interpret("(def quadruple (fn (x) \
                                               (def double (fn (y) \
                                                             (+ y y))) \
                                               (+ (double x) (double x)))) \
                              (quadruple 7)"),
                   "28");
    }
}
