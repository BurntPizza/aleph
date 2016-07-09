use itertools::*;

use std::iter::once;
use std::error::Error;

use read::{Sexp, Span};
use lang::{Env, Ast};


pub fn census(env: &mut Env, mut sexps: Vec<Sexp>) -> Result<Vec<Ast>, Box<Error>> {
    loop {
        let (unparsed_defs, non_defs) = extract_defs(sexps);
        sexps = non_defs;

        for UnParsedDef(span, name, rhs) in unparsed_defs {
            try!(def_occurs_check(once(&name).chain(once(&rhs))));

            match name {
                Sexp::Atom(_, string) => {
                    env.lazy_add_def(span, string, rhs);
                }
                _ => return Err(format!("First arg of `def` must be a symbol: {:?}", name).into()),
            }
        }

        try!(def_occurs_check(sexps.iter()));
        try!(env.finish_work_list());

        // TODO: expand macros
        break; // if no macros expanded
    }

    let asts = try!(sexps_to_asts(env, sexps));

    Ok(asts)
}

struct UnParsedDef(Span, Sexp, Sexp);

fn extract_defs(sexps: Vec<Sexp>) -> (Vec<UnParsedDef>, Vec<Sexp>) {
    sexps.into_iter().partition_map(|sexp| {
        match sexp {
            Sexp::List(span, mut sexps) => {
                if if let Some(&Sexp::Atom(_, ref name)) = sexps.first() {
                    name == "def"
                } else {
                    false
                } {
                    assert_eq!(sexps.len(), 3);
                    let _def_symbol = sexps.remove(0);
                    let name = sexps.remove(0);
                    let rhs = sexps.remove(0);
                    return Partition::Left(UnParsedDef(span, name, rhs));
                } else {
                    Partition::Right(Sexp::List(span, sexps))
                }
            }
            _ => Partition::Right(sexp),
        }
    })
}

fn def_occurs_check<'a, T>(sexps: T) -> Result<(), Box<Error>>
    where T: Iterator<Item = &'a Sexp>
{
    for sexp in sexps {
        match *sexp {
            Sexp::Atom(_, ref name) => {
                if name == "def" {
                    return Err(format!("`def` not allowed as a non-top-level form: {:?}", sexp)
                                   .into());
                }
            }
            Sexp::List(_, ref sexps) => try!(def_occurs_check(sexps.iter())),
        }
    }

    Ok(())
}

fn sexps_to_asts(env: &mut Env, sexps: Vec<Sexp>) -> Result<Vec<Ast>, Box<Error>> {
    sexps.into_iter()
         .filter_map(|sexp| match sexp_to_ast(env, sexp) {
             Ok(ast) => Some(Ok(ast)),
             Err(e) => Some(Err(e)),
         })
         .fold_results(vec![], vec_collector)
}

fn sexp_to_ast(env: &mut Env, sexp: Sexp) -> Result<Ast, Box<Error>> {
    match sexp {
        Sexp::Atom(span, string) => {
            match string.parse::<i64>() {
                Ok(val) => Ok(Ast::I64Literal(span, val)),
                _ => {
                    if string == "true" {
                        Ok(Ast::BoolLiteral(span, true))
                    } else if string == "false" {
                        Ok(Ast::BoolLiteral(span, false))
                    } else {
                        println!("pre-lookup: {:?}", env);
                        Ok(Ast::Atom(span, env.lookup_by_name(&*string).expect("oh no").id()))
                    }
                }
            }
        }
        Sexp::List(span, mut sexps) => {
            match sexps.len() {
                0 => Ok(Ast::EmptyList(span)),
                _ => {
                    let first = sexps.remove(0);

                    match first {
                        Sexp::Atom(_, ref s) if s == "ns" => {
                            // set current ns
                            // Ok(None)
                            unimplemented!()
                        }
                        Sexp::Atom(_, ref s) if s == "use" => {
                            // import names
                            // Ok(None)
                            unimplemented!()
                        }
                        Sexp::Atom(_, ref s) if s == "if" => {
                            assert_eq!(sexps.len(), 3);

                            let cond_expr = try!(sexp_to_ast(env, sexps.remove(0)));
                            let then_expr = try!(sexp_to_ast(env, sexps.remove(0)));
                            let else_expr = try!(sexp_to_ast(env, sexps.remove(0)));

                            Ok(Ast::If(span,
                                       Box::new(cond_expr),
                                       Box::new(then_expr),
                                       Box::new(else_expr)))
                        }
                        Sexp::Atom(_, ref s) if s == "let" => {
                            assert!(sexps.len() >= 2);
                            env.push();

                            let binding_list_src = match sexps.remove(0) {
                                Sexp::List(_, sexps) => {
                                    assert!(sexps.len() % 2 == 0);

                                    sexps.into_iter()
                                         .chunks_lazy(2)
                                         .into_iter()
                                         .map(|mut chunk| {
                                             let (param_span, param_name) = match chunk.next()
                                                                                       .unwrap() {
                                                 Sexp::Atom(span, string) => (span, string),
                                                 _ => panic!("param must be Atom"),
                                             };
                                             let value = chunk.next().unwrap();

                                             (param_span, param_name, value)
                                         })
                                         .collect_vec()
                                }
                                _ => panic!("First arg of `let` must be a list"),
                            };

                            let mut binding_list = vec![];

                            for (span, name, value_sexp) in binding_list_src {
                                let value = try!(sexp_to_ast(env, value_sexp));
                                let record = env.add_record_ast(span, name, &value);

                                binding_list.push((record.id(), value));
                            }

                            let body_asts = try!(sexps_to_asts(env, sexps));
                            env.pop();

                            Ok(Ast::Let(span, binding_list, body_asts))
                        }
                        Sexp::Atom(_, ref s) if s == "def" => unreachable!(),
                        Sexp::Atom(_, ref s) if s == "defreader" => unreachable!(),
                        _ => {
                            Ok(Ast::Inv(span,
                                        Box::new(try!(sexp_to_ast(env, first))),
                                        try!(sexps.into_iter()
                                                  .map(|sexp| sexp_to_ast(env, sexp))
                                                  .fold_results(vec![], vec_collector))))
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
