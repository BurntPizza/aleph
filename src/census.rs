
use itertools::*;

use std::error::Error;

use read::Sexp;
use lang::{self, Env, Ast, BindingKey, Binding, ConstType};

pub fn census(env: &mut Env, sexps: Vec<Sexp>) -> Result<Vec<Ast>, Box<Error>> {
    // assert directives only in top-level
    // execute directives

    // assign scope ids to expressions
    // expand macros
    // repeat

    // specials should be unshadowable

    // TODO

    // TODO: convert to map()

    sexps.into_iter()
         .filter_map(|sexp| match sexp_to_ast(env, sexp) {
             Ok(Some(ast)) => Some(Ok(ast)),
             Ok(None) => None,
             Err(e) => Some(Err(e)),
         })
         .fold_results(vec![], vec_collector)
}

fn sexp_to_ast(env: &mut Env, sexp: Sexp) -> Result<Option<Ast>, Box<Error>> {
    match sexp {
        Sexp::Atom(span, string) => {
            match string.parse::<i64>() {
                Ok(val) => Ok(Some(Ast::I64Literal(span, val))),
                _ => Ok(Some(Ast::Atom(span, BindingKey::String(string)))),
            }
        }
        Sexp::List(span, mut sexps) => {
            match sexps.len() {
                0 => Ok(Some(Ast::EmptyList(span))),
                _ => {
                    let first = sexps.remove(0);

                    match first {
                        Sexp::Atom(span, ref s) if s == "ns" => {
                            // set current ns
                            // Ok(None)
                            unimplemented!()
                        }
                        Sexp::Atom(span, ref s) if s == "use" => {
                            // import names
                            // Ok(None)
                            unimplemented!()
                        }
                        Sexp::Atom(_, ref s) if s == "def" => {
                            // install binding
                            assert_eq!(sexps.len(), 2);
                            let rhs = sexps.pop().unwrap();
                            let (name, span) = match sexps.pop().unwrap() {
                                Sexp::Atom(span, string) => (string, span),
                                Sexp::List(..) => return Err("`def` LHS must be an Atom".into()),
                            };

                            try!(env.test_name_collision(&*name));

                            let binding = match rhs {
                                Sexp::Atom(span, string) => {
                                    match string.parse::<i64>() {
                                        Ok(val) => Binding::Const(ConstType::I64(val)),
                                        _ => unimplemented!(),
                                    }
                                }
                                Sexp::List(span, sexps) => unimplemented!(),
                            };

                            let id = env.add_record(name, binding, span);

                            Ok(None)
                        }
                        Sexp::Atom(span, ref s) if s == "defreader" => unreachable!(),
                        _ => {
                            Ok(Some(Ast::Inv(span,
                                             Box::new(try!(sexp_to_ast(env, first)).unwrap()),
                                             try!(sexps.into_iter()
                                                       .map(|sexp| {
                                                           sexp_to_ast(env, sexp)
                                                               .map(Option::unwrap)
                                                       })
                                                       .fold_results(vec![], vec_collector)))))
                        }
                    }
                }
            }
        }
    }
}

fn vec_collector<T>(mut b: Vec<T>, a: T) -> Vec<T> {
    b.push(a);
    b
}
