

use itertools::*;

use std::error::Error;
use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};

use reader::{self, Form, InputStream};
use analyzer::{self, AstNode};
use symbol_table::*;
use vm::*;


// TODO: return type
pub fn interpret<T: Into<String>>(input: T) -> String {
    let input = input.into();
    let form = read(input).unwrap();
    let (ast, env) = analyze(form).unwrap();
    // let (ast, env) = macroexpand(ast, env).unwrap();
    let program = compile(ast, env).unwrap();

    exec(program)
}

fn read(input: String) -> Result<Vec<Form>, Box<Error>> {
    let mut reader = reader::ReaderEnv::new_default(InputStream::new(input));
    reader.read_all().map_err(|_| reader.last_error().into())

}
fn analyze(input: Vec<Form>) -> Result<(AstNode, SymbolTable), Box<Error>> {
    analyzer::analyze_from_root(input).map_err(Into::into)
}

fn macroexpand(mut form: AstNode, env: SymbolTable) -> Result<(AstNode, SymbolTable), Box<Error>> {
    enum ExpansionResult {
        Expanded(AstNode),
        NotExpanded(AstNode),
    }

    fn macroexpand_1(form: AstNode, env: &SymbolTable) -> ExpansionResult {
        unimplemented!()
    }

    loop {
        match macroexpand_1(form, &env) {
            ExpansionResult::Expanded(new_ast) => form = new_ast,
            ExpansionResult::NotExpanded(old_ast) => return Ok((old_ast, env)),
        }
    }
}

fn compile(ast: AstNode, mut env: SymbolTable) -> Result<Program, Box<Error>> {
    // use scopes!
    // no special 'top-level' anonymous scope
    // walk tree and visit nodes:
    // - fn nodes: emit function in text section, 'value' of the node is... the addr? a special NormalFn kind?
    //
    //

    // some things are not known until post-assembly/linking, primarily fn addresses

    // difference between evaluating a form, and invoking it ('s evaluation result)

    // investigate reader (do tests) to make sure empty forms are possible (like empty argument lists)

    fn emit_do_invocation(args: &[AstNode],
                          env: &mut SymbolTable,
                          p: &mut ProgramBuilder)
                          -> Result<(), Box<Error>> {

        match args.len() {
            0 => unimplemented!(),
            1 => emit(&args[0], env, p),
            _ => {
                let (last, others) = args.split_last().unwrap();

                for node in others {
                    p.mark_stack();
                    try!(emit(node, env, p));
                    p.pop_to_mark();
                }

                emit(last, env, p)
            }
        }
    }

    fn emit_var_invocation(var_id: u32,
                           args: &[AstNode],
                           env: &mut SymbolTable,
                           p: &mut ProgramBuilder)
                           -> Result<(), Box<Error>> {
        let record = env.lookup_id(var_id).unwrap();

        // check for special forms
        match record.kind() {
            BindingKind::Special => {
                match record.ident() {
                    "do" => emit_do_invocation(args, env, p),
                    "+" => {
                        // move this somewhere else?
                        for node in args {
                            try!(emit(node, env, p));
                        }

                        p.add(args.len());
                        Ok(())
                    }
                    i => panic!("no special impl: {:?}", i),
                }
            }
            BindingKind::Fn => {
                p.eval_fn(var_id);
                Ok(())
            }
            ref k => panic!("invoke var kind: not impl: {:?}", k),
        }
    }

    // TODO
    fn emit_var_evaluation(var_id: u32,
                           env: &SymbolTable,
                           p: &mut ProgramBuilder)
                           -> Result<(), Box<Error>> {
        let record = env.lookup_id(var_id).unwrap();

        match record.kind() {
            BindingKind::Var => {
                p.eval_binding(var_id);
                Ok(())
            }
            ref k => panic!("not impl: {:?}", k),
        }
    }

    fn emit_fn_def(fn_var_id: u32,
                   def_args: &[AstNode],
                   env: &mut SymbolTable,
                   p: &mut ProgramBuilder)
                   -> Result<(), Box<Error>> {
        // one for param list, at least one for body
        assert!(def_args.len() >= 2);

        let (param_list, body_forms) = def_args.split_first().unwrap();

        match *param_list {
            AstNode::Inv(ref callee, ref args) => {
                // wrap body in do form
                let do_id = env.lookup_ident("do")
                               .map(|r| r.id())
                               .expect("No `do` found in env");

                p.begin_fn_def(fn_var_id);

                // pop stack into locals
                // params: (a b c d)
                // stack: [a b c d] ->top

                let params = try!(::std::iter::once(&**callee)
                                      .chain(args)
                                      .map(|node| {
                                          match *node {
                                              AstNode::Var(id) => Ok(id),
                                              _ => {
                                                  Err(Box::<Error>::from("param lists may only \
                                                                          contain vars"))
                                              }
                                          }
                                      })
                                      .fold_results(vec![], |mut acc, p| {
                                          acc.push(p);
                                          acc
                                      }));

                p.make_locals(&params[..]);

                // emit body
                let body = AstNode::inv(AstNode::var(do_id), body_forms.into());
                try!(emit(&body, env, p));
                p.ret();

                p.end_fn_def();
                Ok(())
            }
            _ => Err("1st arg of `fn` must be param list".into()),
        }
    }

    // figure out how to refactor this away
    fn emit_inv_invocation(callee_callee: &AstNode,
                           callee_args: &[AstNode],
                           outer_args: &[AstNode],
                           env: &mut SymbolTable,
                           p: &mut ProgramBuilder)
                           -> Result<(), Box<Error>> {
        match *callee_callee {
            AstNode::Var(var_id) => {
                // I don't like this having to be here
                let record = env.lookup_id(var_id).unwrap();
                match record.kind() {
                    BindingKind::Special => {
                        match record.ident() {
                            "fn" => {
                                // emit_fn_expr_invocation: (fn callee_args)
                                for node in outer_args {
                                    try!(emit(node, env, p));
                                }

                                let anon_record = env.fresh_numbered_ident("fn", BindingKind::Fn);
                                let fn_var_id = anon_record.id();

                                try!(emit_fn_def(fn_var_id, callee_args, env, p));
                                try!(emit_var_invocation(fn_var_id, outer_args, env, p));
                                Ok(())
                            }
                            i => panic!("not impl: {:?}", i),
                        }
                    }
                    ref k => panic!("not impl: {:?}", k),
                }
            }
            _ => panic!("not impl: {:?}", callee_callee),
        }
    }

    fn emit(node: &AstNode,
            env: &mut SymbolTable,
            p: &mut ProgramBuilder)
            -> Result<(), Box<Error>> {
        match *node {
            AstNode::Inv(ref callee, ref args) => {
                // what is being invoked?
                match **callee {
                    AstNode::Var(var_id) => emit_var_invocation(var_id, args, env, p),
                    AstNode::Inv(ref callee_callee, ref callee_args) => {
                        emit_inv_invocation(callee_callee, callee_args, args, env, p)
                    }
                    _ => unimplemented!(), // can't invoke other things yet
                }
            }
            AstNode::Var(var_id) => emit_var_evaluation(var_id, env, p),
            AstNode::Const(val) => {
                p.i64_const(val);
                Ok(())
            }
        }
    }


    let mut program = new_program();

    try!(emit(&ast, &mut env, &mut program));

    program.exit();

    println!("{:?}", env);

    let program = program.finish(env);

    println!("{:?}", program);
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
    fn fn_expr_inv() {
        assert_eq!(interpret("((fn (x y z) (+ x y z)) 1 2 3)"), "6");
    }

    // #[test]
    // fn let_expr() {
    //     assert_eq!(interpret("(let (x 1 y -1) (+ x y 5))"), "6");
    // }

    // #[test]
    // fn def_const_var() {
    //     assert_eq!(interpret("(def x 7) x"), "7");
    // }

    // #[test]
    // fn add_const_vars() {
    //     assert_eq!(interpret("(def x 3) (def y 4) (+ x x y) (+ x 7 y)"), "14");
    // }

    // #[test]
    // fn copy_var() {
    //     assert_eq!(interpret("(def x 3) (def y x) y"), "3");
    // }

    // #[test]
    // fn re_def_vars() {
    //     assert_eq!(interpret("(def y 3) (def y 5) y"), "5");
    // }
    // #[ignore]
    // #[test]
    // fn def_fn() {
    //     assert_eq!(interpret("(def f (fn (x) (+ 1 x x))) (f 7)"), "15");
    // }

    // #[ignore]
    // #[test]
    // fn nested_def_fn() {
    //     assert_eq!(interpret("(def quadruple (fn (x) \
    //                                            (def double (fn (y) \
    //                                                          (+ y y))) \
    //                                            (+ (double x) (double x)))) \
    //                           (quadruple 7)"),
    //                "28");
    // }
}
