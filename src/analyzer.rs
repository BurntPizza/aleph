
use std::error;
use std::fmt::{self, Display, Formatter};

use core;
use reader::Form;
use symbol_table::*;

pub fn analyze_from_root(forms: Vec<Form>) -> Result<Analysis, AnalyzerError> {
    let mut env = SymbolTable::empty();
    env.add_ident("do", VarKind::Special(core::special_form_do))
       .unwrap();

    // TODO: should be core, right?
    env.add_ident("+", VarKind::Special(core::builtin_fn_plus)).unwrap();

    // ////

    let mut top_level_nodes = vec![];

    for form in forms {
        top_level_nodes.push(try!(analyze_in_env(&form, &mut env)));
    }

    // ///

    let do_id = env.lookup_ident("do")
                   .map(Record::id)
                   .expect("No `do` found in env");

    let ast = AstNode::inv(AstNode::var(do_id), top_level_nodes);

    let analysis = Analysis {
        ast: ast,
        symbol_table: env,
    };

    println!("analysis: {:#?}", analysis);

    Ok(analysis)
}

fn analyze_in_env(form: &Form, env: &mut SymbolTable) -> Result<AstNode, AnalyzerError> {
    match *form {
        Form::Atom(ref s) => {
            let text = &*s.text;

            match text.parse::<i64>() {
                Ok(val) => Ok(AstNode::int_const(val)),
                Err(_) => {
                    let id = try!(env.lookup_ident(text)
                                     .map(Record::id)
                                     .ok_or(AnalyzerError::UndefinedIdent(text.to_owned())));
                    Ok(AstNode::var(id))
                }
            }
        }
        Form::List(ref v) => {
            match v.len() {
                0 => Err(AnalyzerError::empty_form()),
                _ => {
                    let invocation = &v[0];
                    let args = &v[1..];
                    let callee = try!(analyze_in_env(invocation, env));
                    let mut evaluated_args = Vec::with_capacity(args.len());

                    for arg in args {
                        let arg = try!(analyze_in_env(arg, env));
                        evaluated_args.push(arg);
                    }

                    Ok(AstNode::inv(callee, evaluated_args))
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Analysis {
    ast: AstNode,
    symbol_table: SymbolTable,
}

impl Analysis {
    pub fn ast(&self) -> &AstNode {
        &self.ast
    }

    pub fn env(&self) -> &SymbolTable {
        &self.symbol_table
    }
}

#[derive(Debug, Clone)]
pub enum AstNode {
    // TODO
    Const(i64),

    // symbol table id
    Var(u32),

    // callee, args
    Inv(Box<AstNode>, Vec<AstNode>),
}

impl AstNode {
    pub fn int_const(val: i64) -> Self {
        AstNode::Const(val)
    }

    pub fn var(id: u32) -> Self {
        AstNode::Var(id)
    }

    pub fn inv(callee: AstNode, args: Vec<AstNode>) -> Self {
        AstNode::Inv(Box::new(callee), args)
    }
}

#[derive(Debug)]
pub enum AnalyzerError {
    EmptyForm,
    UndefinedIdent(String),
}

impl AnalyzerError {
    pub fn empty_form() -> Self {
        AnalyzerError::EmptyForm
    }
}


impl Display for AnalyzerError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // TODO
        write!(f, "{:?}", self)
    }
}

impl error::Error for AnalyzerError {
    fn description(&self) -> &str {
        unimplemented!()
    }
}
