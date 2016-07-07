use itertools::*;

use std::iter::once;
use std::error::Error;

use read::{Sexp, Span};
use lang::{Env, Ast, BindingKey};


pub fn census(env: &mut Env, mut sexps: Vec<Sexp>) -> Result<Vec<Ast>, Box<Error>> {
    loop {
        let (unparsed_defs, non_defs) = extract_defs(sexps);
        sexps = non_defs;

        for UnParsedDef(span, name, rhs) in unparsed_defs {
            try!(def_occurs_check(once(&name).chain(once(&rhs))));

            match name {
                Sexp::Atom(_, string) => {
                    env.add_record(span, string, rhs);
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
                _ => {
                    if string == "true" {
                        Ok(Some(Ast::BoolLiteral(span, true)))
                    } else if string == "false" {
                        Ok(Some(Ast::BoolLiteral(span, false)))
                    } else {
                        Ok(Some(Ast::Atom(span, BindingKey::String(string))))
                    }
                }
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
                        Sexp::Atom(_, ref s) if s == "def" => Ok(None),
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
