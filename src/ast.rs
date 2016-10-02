
use itertools::*;

use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

use read::Sexp;

#[derive(Debug)]
pub struct File {
    defs: HashMap<String, Expr>,
    exprs: Vec<Expr>,
}

impl File {
    pub fn exprs(&self) -> &[Expr] {
        &*self.exprs
    }
}

#[derive(Debug)]
pub enum Item {
    Def(Def),
    Expr(Expr),
}

#[derive(Debug)]
pub struct Def {
    name: Symbol,
    rhs: Expr,
}

#[derive(Debug)]
pub struct Symbol {
    ident: String,
}

impl Symbol {
    pub fn ident(&self) -> &str {
        &*self.ident
    }
}

#[derive(Debug)]
pub enum Expr {
    Unit,
    I64(i64),
    Bool(bool),
    Sym(Symbol),
    App(App),
    Lambda(Lambda),
    Let(Let),
    If(If),
}

#[derive(Debug)]
pub struct App {
    callee: Box<Expr>,
    args: Vec<Expr>,
}

#[derive(Debug)]
pub struct Lambda {
    args: Vec<Symbol>,
    body: Box<Expr>,
}

#[derive(Debug)]
pub struct Let {
    bindings: Vec<(Symbol, Expr)>,
    body: Vec<Expr>,
}

#[derive(Debug)]
pub struct If {
    cond_expr: Box<Expr>,
    then_expr: Box<Expr>,
    else_expr: Box<Expr>,
}


type Sexps = Vec<Sexp>;

impl Into<File> for Sexps {
    fn into(self) -> File {
        debug!("Analyzing top-level forms...");

        let (defs, exprs): (Vec<_>, Vec<_>) = self.into_iter().partition_map(|sexp| {
            match sexp.into() {
                Item::Def(def) => Either::Left(def),
                Item::Expr(e) => Either::Right(e),
            }
        });

        check_defs(&*defs);

        let defs = defs.into_iter().map(|Def { name, rhs }| (name.ident, rhs)).collect();

        File {
            defs: defs,
            exprs: exprs,
        }
    }
}

fn check_defs(defs: &[Def]) {
    use petgraph::{algo, Graph};

    fn get_deps(e: &Expr) -> Vec<&str> {
        match *e {
            Expr::Sym(ref s) => vec![&*s.ident],
            Expr::Bool(..) | Expr::I64(..) | Expr::Unit => vec![],
            _ => panic!("get_deps: unimpl: {:?}", e),
        }
    }

    let mut deps = Graph::<_, ()>::new();
    let mut name_to_node_idx = HashMap::new();
    let mut name_collisions = vec![];

    for def in defs {
        debug!("Found def: {}", def.name.ident);
        let node_idx = deps.add_node(def);

        if let Some(_) = name_to_node_idx.insert(&*def.name.ident, node_idx) {
            name_collisions.push(&*def.name.ident);
            error!("Name collision");
        }
    }

    if !name_collisions.is_empty() {
        for name in name_collisions {
            panic!("Name collision: {}", name);
        }
    }

    debug!("Gathering def dependencies...");

    for idx in deps.node_indices() {
        for name in get_deps(&deps[idx].rhs) {
            let dep_idx = *name_to_node_idx.get(&*name).unwrap_or_else(|| {
                error!("Unknown identifier");
                panic!("Unknown identifier: {}", name)
            });

            debug!("Found dependency: {} -> {}",
                   deps[idx].name.ident,
                   deps[dep_idx].name.ident);
            deps.add_edge(idx, dep_idx, ());
        }
    }

    debug!("Checking for cyclic definitions...");

    let cyclic_groups = algo::scc(&deps).into_iter().filter(|scc| scc.len() > 1).collect_vec();

    if !cyclic_groups.is_empty() {
        error!("Cyclic definitions found");
        panic!("Cyclic definitions:\n\t{}",
               cyclic_groups.into_iter()
                            .map(|scc| {
                                format!("[{}]",
                                        scc.into_iter()
                                           .map(|def| &deps[def].name.ident)
                                           .sorted()
                                           .into_iter()
                                           .join(", "))
                            })
                            .sorted()
                            .into_iter()
                            .join("\n\t"));
    }
}

impl Into<Item> for Sexp {
    fn into(self) -> Item {
        match self {
            Sexp::List(id, span, mut sexps) => {
                if if let Some(&Sexp::Atom(_, _, ref name)) = sexps.first() {
                    name == "def"
                } else {
                    false
                } {
                    assert_eq!(sexps.len(), 3);
                    let _def_symbol = sexps.remove(0);
                    let name = match sexps.remove(0) {
                        Sexp::Atom(_, _, string) => string,
                        _ => panic!(),
                    };
                    let rhs = sexps.remove(0);

                    Item::Def(Def {
                        name: name.into(),
                        rhs: rhs.into(),
                    })
                } else {
                    Item::Expr(Sexp::List(id, span, sexps).into())
                }
            }
            _ => Item::Expr(self.into()),
        }
    }
}

impl Into<Expr> for Sexp {
    fn into(self) -> Expr {
        match self {
            Sexp::Atom(_, _, string) => {
                string.parse().map(Expr::I64).unwrap_or_else(|_| {
                    match &*string {
                        "true" => Expr::Bool(true),
                        "false" => Expr::Bool(false),
                        _ => Expr::Sym(string.into()),
                    }
                })
            }
            Sexp::List(id, span, mut sexps) => {
                if sexps.is_empty() {
                    return Expr::Unit;
                }

                let first = sexps.remove(0);

                match first {
                    Sexp::Atom(_, _, ref s) if s == "ns" => {
                        // set current ns
                        // Ok(None)
                        unimplemented!()
                    }
                    Sexp::Atom(_, _, ref s) if s == "use" => {
                        // import names
                        // Ok(None)
                        unimplemented!()
                    }
                    Sexp::Atom(_, _, ref s) if s == "do" => {
                        assert!(sexps.len() > 0);

                        unimplemented!()
                        // let body = sexps.into_iter()
                        //                 .map(Into::into)
                        //                 .collect();

                        // Ast::Do(id, scope.clone(), span, body)
                    }
                    Sexp::Atom(_, _, ref s) if s == "if" => {
                        assert_eq!(sexps.len(), 3);

                        unimplemented!()
                        // let cond_expr = try!(sexp_to_ast(scope.clone(), sexps.remove(0)));
                        // let then_expr = try!(sexp_to_ast(scope.clone(), sexps.remove(0)));
                        // let else_expr = try!(sexp_to_ast(scope.clone(), sexps.remove(0)));

                        // Ok(Ast::If(id,
                        //            scope.clone(),
                        //            span,
                        //            Box::new(cond_expr),
                        //            Box::new(then_expr),
                        //            Box::new(else_expr)))
                    }
                    Sexp::Atom(_, _, ref s) if s == "let" => {
                        assert!(sexps.len() >= 2);

                        unimplemented!()

                        // let inner_scope = Scope::child_of(scope.clone());
                        // let binding_list_src = match sexps.remove(0) {
                        //     Sexp::List(_, _, sexps) => {
                        //         assert!(sexps.len() % 2 == 0);

                        //         sexps.into_iter()
                        //              .chunks_lazy(2)
                        //              .into_iter()
                        //              .map(|mut chunk| {
                        //                  let (param_id, param_span, param_name) = {
                        //                      match chunk.next().unwrap() {
                        //                          Sexp::Atom(id, span, string) => (id, span, string),
                        //                          _ => panic!("param must be Atom"),
                        //                      }
                        //                  };
                        //                  let value = chunk.next().unwrap();

                        //                  (param_id, param_span, param_name, value)
                        //              })
                        //              .collect_vec()
                        //     }
                        //     _ => panic!("First arg of `let` must be a list"),
                        // };

                        // let mut binding_list = vec![];

                        // for (id, span, name, value_sexp) in binding_list_src {
                        //     let value = try!(sexp_to_ast(inner_scope.clone(), value_sexp));
                        //     scope.add_binding(name.clone(), span, value.clone());
                        //     binding_list.push((id, name, value));
                        // }

                        // let body_asts = try!(sexps.into_iter()
                        //                           .map(|sexp| {
                        //                               sexp_to_ast(inner_scope.clone(), sexp)
                        //                           })
                        //                           .fold_results(vec![], vec_collector));

                        // Ok(Ast::Let(id, scope.clone(), span, binding_list, body_asts))
                    }
                    Sexp::Atom(_, _, ref s) if s == "fn" => {
                        assert!(sexps.len() >= 2);

                        unimplemented!()

                        // let inner_scope = Scope::child_of(scope.clone());
                        // let params_list = match sexps.remove(0) {
                        //     Sexp::List(_, _, sexps) => {
                        //         sexps.into_iter()
                        //              .map(|sexp| match sexp.clone() {
                        //                  Sexp::Atom(id, span, string) => {
                        //                      let ast = try!(sexp_to_ast(inner_scope.clone(), sexp));
                        //                      let binding_id = inner_scope.add_binding(string,
                        //                                                               span,
                        //                                                               ast);

                        //                      Ok((id, binding_id))
                        //                  }
                        //                  _ => Err("param must be Atom".into()),
                        //              })
                        //              .fold_results(vec![], vec_collector)
                        //     }
                        //     _ => Err("First arg of `fn` must be a list".into()),
                        // };

                        // let params_list = match params_list {
                        //     Ok(v) => v,
                        //     Err(e) => return Err(e),
                        // };

                        // let body_asts = try!(sexps.into_iter()
                        //                           .map(|sexp| {
                        //                               sexp_to_ast(inner_scope.clone(), sexp)
                        //                           })
                        //                           .fold_results(vec![], vec_collector));

                        // // TODO (NOW): lambda lifting
                        // // what is needed in a fn prototype?


                        // Ok(Ast::Fn(id, scope.clone(), span, params_list, body_asts))
                    }
                    Sexp::Atom(_, _, ref s) if s == "def" => unreachable!(),
                    Sexp::Atom(_, _, ref s) if s == "defreader" => unreachable!(),
                    _ => {
                        Expr::App(App {
                            callee: Box::new(first.into()),
                            args: sexps.into_iter().map(Into::into).collect(),
                        })
                    }
                }
            }
        }
    }
}

impl Into<Symbol> for String {
    fn into(self) -> Symbol {
        Symbol { ident: self }
    }
}


#[derive(Debug)]
pub enum Constraint<'a, 'b> {
    Eq(Term<'a>, Term<'b>),
}

#[derive(Debug)]
pub enum Term<'e> {
    TExpr(&'e Expr),
    Var(String),
    I64,
    Arrow(Vec<Term<'e>>, Box<Term<'e>>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f,
               "{}",
               match *self {
                   Expr::Unit => "()".to_owned(),
                   Expr::Bool(val) => format!("{}", val),
                   Expr::I64(val) => format!("{}", val),
                   Expr::Sym(ref sym) => sym.ident().to_owned(),
                   Expr::App(ref app) => {
                       format!("({} {})",
                               app.callee,
                               app.args.iter().map(|a| format!("{}", a)).join(" "))
                   }
                   _ => panic!("Expr::fmt: unimpl: {:?}", self),
               })
    }
}

impl<'a> Display for Term<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f,
               "{}",
               match *self {
                   Term::I64 => "i64".to_owned(),
                   Term::Var(ref s) => format!("Var<{}>", s),
                   Term::TExpr(ref e) => fmt_type(e),
                   Term::Arrow(ref args, ref body) => {
                       format!("({} -> {})",
                               args.iter().map(|a| a.to_string()).join(", "),
                               body.to_string())
                   }
               })
    }
}

fn fmt_type(e: &Expr) -> String {
    match *e {
        Expr::Unit => "()".to_owned(),
        Expr::Bool(_) => "bool".to_owned(),
        Expr::I64(_) => "i64".to_owned(),
        Expr::Sym(ref s) => format!("Var<{}>", s.ident()),
        Expr::App(ref app) => {
            format!("(typeof ({} {}))",
                    fmt_type(&*app.callee),
                    app.args.iter().map(fmt_type).join(", "))
        }
        _ => panic!("fmt_type: unimpl: {:?}", e),
    }
}

impl<'e> From<&'e Lambda> for Term<'e> {
    fn from(a: &Lambda) -> Term {
        let args = a.args.iter().map(|sym| Term::Var(sym.ident().to_owned())).collect();
        Term::Arrow(args, Box::new(Term::TExpr(&a.body)))
    }
}

impl<'e> From<&'e Symbol> for Term<'e> {
    fn from(a: &Symbol) -> Term {
        Term::Var(a.ident().to_owned())
    }
}

pub fn analyze(e: &Expr) -> Vec<Constraint> {

    macro_rules! ret {
        ($first:expr; $($rest:expr),*) => ({
            let mut v = $first;
            $(v.append(&mut $rest);)*
            v
        });
    }

    fn eq<'a, 'b>(a: Term<'a>, b: Term<'b>) -> Constraint<'a, 'b> {
        Constraint::Eq(a, b)
    }

    match *e {
        // e = I64
        Expr::I64(..) => vec![eq(Term::TExpr(e), Term::I64)],
        // e = Var
        Expr::Sym(ref sym) => vec![eq(Term::TExpr(e), sym.into())],
        // e = (lam.args -> lam.body)
        Expr::Lambda(ref lam) => {
            ret!(
                vec![eq(
                    Term::TExpr(e), 
                    lam.into())];
                analyze(&*lam.body))
        }
        // callee = (args -> e)
        Expr::App(App { ref callee, ref args }) => {
            ret!(
                vec![eq(
                    Term::TExpr(callee),
                    Term::Arrow(
                        args.iter()
                            .map(|arg| Term::TExpr(arg))
                            .collect(),
                        Box::new(Term::TExpr(e))))];
                analyze(callee),
                analyze_all(&*args))
        }
        _ => unimplemented!(),
    }
}

pub fn analyze_all(es: &[Expr]) -> Vec<Constraint> {
    let mut v = Vec::with_capacity(es.len());

    for e in es {
        v.append(&mut analyze(e));
    }

    v
}


fn print_constraints(v: &[Constraint]) {
    println!("\nConstraints:");

    for c in v {
        match *c {
            Constraint::Eq(ref a, ref b) => {
                if let Term::TExpr(ref e) = *a {
                    println!("{} = {}", e, b);
                } else {
                    panic!("print_constraints")
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    extern crate slog_envlogger;

    use itertools::*;

    use lang;
    use ast;

    fn test_(input: &str, expected: &str) {
        let _ = slog_envlogger::init();
        debug!("");
        debug!("Test input: {}", input);

        let sexps = lang::read(input);
        let file: ast::File = sexps.into();
        let constraints = ast::analyze_all(file.exprs());
        println!("\nExprs:\n{}", file.exprs().iter().map(|e| e.to_string()).join("\n"));
        ast::print_constraints(&*constraints);
        panic!();
    }


    macro_rules! test {
        ($name:ident, $input:expr, $expected:expr) => {
            test!($name, $input, $expected,);
        };
        ($name:ident, $input:expr, $expected:expr, $($extra:meta),*) => {
            #[test]
            $(#[$extra])*
            fn $name() {
                test_($input, $expected);
            }
        }
    }

   // test!(file_input, "(a 1)", "1");
}
