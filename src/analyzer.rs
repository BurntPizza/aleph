
use std::error;
use std::fmt::{self, Display, Formatter};

use reader::{Form, FormKind};
use symbol_table::*;

pub fn analyze_from_root(forms: Vec<Form>) -> Result<(AstNode, SymbolTable), AnalyzerError> {
    let mut env = SymbolTable::empty();

    env.add_ident("def", BindingKind::Special).unwrap();
    env.add_ident("fn", BindingKind::Special).unwrap();
    env.add_ident("do", BindingKind::Special).unwrap();
    env.add_ident("+", BindingKind::Special).unwrap();

    // ////

    let mut top_level_nodes = vec![];

    for form in forms {
        top_level_nodes.push(try!(analyze_in_env(&form, &mut env)));
    }

    // ///

    let do_id = env.lookup_ident("do")
                   .map(|r| r.id())
                   .expect("No `do` found in env");

    let ast = AstNode::inv(AstNode::var(do_id), top_level_nodes);

    Ok((ast, env))
}

#[allow(option_map_unwrap_or_else)] // map_or_else has lifetime problems in this case
fn analyze_in_env(form: &Form, env: &mut SymbolTable) -> Result<AstNode, AnalyzerError> {
    match *form.kind() {
        FormKind::Atom(ref s) => {
            let text = &*s.clone();

            match text.parse::<i64>() {
                Ok(val) => Ok(AstNode::int_const(val)),
                Err(_) => {
                    // TODO unwrap, normal?
                    let id = env.lookup_ident(text)
                                .map(|r| r.id())
                                .unwrap_or_else(|| {
                                    env.add_ident(text, BindingKind::Var)
                                       .unwrap()
                                       .id()
                                });
                    Ok(AstNode::var(id))
                }
            }
        }
        FormKind::List(ref v) => {
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
