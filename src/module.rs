
use super::*;
use read::Sexp;



use std::rc::Rc;
use std::borrow::Borrow;
use std::fmt::{self, Debug, Formatter};
use macros::*;

#[derive(Copy)]
pub struct ExternFnPtr(pub fn(&[Value]) -> Value);

impl PartialEq for ExternFnPtr {
    fn eq(&self, other: &Self) -> bool {
        self.0 as *const () == other.0 as *const ()
    }
}

impl Clone for ExternFnPtr {
    fn clone(&self) -> Self {
        ExternFnPtr(self.0)
    }
}

impl Debug for ExternFnPtr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_tuple("ExternFnPtr")
            .field(&(self.0 as *const ()))
            .finish()
    }
}

#[derive(Debug)]
pub enum Extern {
    Fn { ty: Type, ptr: ExternFnPtr },
}

#[derive(Debug)]
struct ParamList {
    vars: Vec<String>,
    tail: Option<String>,
}

fn parse_param_list(sexps: &[Sexp]) -> ParamList {
    let mut vars = vec![];
    let mut iter = sexps.into_iter();

    while let Some(sexp) = iter.next() {
        match *sexp {
            Sexp::Atom(ref sym) => {
                if sym == "&" {
                    let tail = match iter.next() {
                        Some(&Sexp::Atom(ref tail_sym)) => Some(tail_sym.to_owned()),
                        Some(&Sexp::List(_)) => unimplemented!(),
                        _ => panic!(),
                    };

                    assert!(iter.next().is_none());
                    return ParamList { vars, tail };
                }

                if sym.starts_with("&") {
                    assert!(iter.next().is_none());
                    let tail = Some(sym[1..].to_owned());
                    return ParamList { vars, tail };
                }

                vars.push(sym.to_owned());
            }
            Sexp::List(_) => unimplemented!(),
        }
    }

    ParamList { vars, tail: None }
}

#[derive(Debug)]
pub struct Module {
    pub top_level: Vec<TExp>,
    pub externs: HashMap<String, Extern>,
    pub defs: HashMap<String, Rc<(TExp, Type)>>,
    pub exports: HashMap<String, Rc<(TExp, Type)>>,
}

#[derive(Default)]
pub struct ModuleBuilder<'builder> {
    externs: HashMap<String, Extern>,
    linked_modules: HashMap<String, &'builder Module>,
}

impl<'builder> ModuleBuilder<'builder> {
    pub fn register_extern<S: Into<String>>(&mut self, sym: S, e: Extern) {
        self.externs.insert(sym.into(), e);
    }

    pub fn link_module<'a: 'builder, S>(&mut self, name: S, module: &'a Module)
    where
        S: Into<String>,
    {
        self.link_module_impl(name.into(), module)
    }

    fn link_module_impl<'a: 'builder>(&mut self, name: String, module: &'a Module) {
        let name = name.into();
        if self.linked_modules.contains_key(&name) {
            panic!();
        }

        self.linked_modules.insert(name, module);
    }

    pub fn parse_mod<S: Into<String>>(self, input: S) -> Module {
        fn parse_mod_impl(this: ModuleBuilder, input: String) -> Module {
            let mut env = InferenceEnv::default();
            let mut input = read::InputStream::new(input.into());
            let mut reader_env = read::Env::default();
            let mut conv_env = CEnv {
                counter: 0,
                vars: HashMap::new(),
            };

            for (sym, e) in &this.externs {
                match *e {
                    Extern::Fn { ref ty, .. } => {
                        env.vars.insert(sym.clone(), ty.clone());
                        conv_env.new_var(sym.clone(), ty.clone());
                    }
                }
            }

            let sexps = read::read(&mut reader_env, &mut input).unwrap();

            let mut menv = MacroExpansionEnv::default();
            let sexps = menv.macro_expand(sexps);

            let mut top_level = vec![];
            let mut exported_syms = HashSet::new();
            let mut defs = vec![];
            let mut macro_defs = vec![];
            let mut use_statements = vec![];

            for sexp in sexps {
                match sexp {
                    Sexp::List(children) => {
                        match children.first() {
                            Some(&Sexp::Atom(ref sym)) => {
                                match &**sym {
                                    "def" | "pub" => {
                                        assert_eq!(children.len(), 3);
                                        let name = &children[1];
                                        let body = children[2].clone();

                                        let name = match *name {
                                            Sexp::Atom(ref sym) => sym,
                                            _ => panic!(),
                                        };

                                        if sym == "pub" {
                                            if exported_syms.contains(&*name) {
                                                panic!();
                                            }
                                            exported_syms.insert(name.clone());
                                        }

                                        defs.push((name.to_owned(), body));
                                    }
                                    "use" => {
                                        let children = children[1..].to_owned();
                                        use_statements.push(children);
                                    }
                                    "defmacro" => {
                                        assert!(children.len() >= 3);
                                        let name = match children[1] {
                                            Sexp::Atom(ref sym) => sym.to_owned(),
                                            _ => panic!(),
                                        };
                                        let params = match children[2] {
                                            Sexp::List(ref sexps) => parse_param_list(sexps),
                                            _ => panic!(),
                                        };
                                        let body = children[3..].to_owned();

                                        macro_defs.push((name, (params, body)));
                                    }
                                    _ => {
                                        top_level.push(Sexp::List(children.clone()));
                                    }
                                }
                            }
                            _ => top_level.push(Sexp::List(children.clone())),
                        }
                    }
                    _ => top_level.push(sexp),
                }
            }

            enum Import {
                All(String),
            }

            let mut imports = vec![];

            for mut path in use_statements {
                assert!(path.len() == 2); // TODO

                // let mut module;

                let segment = match path.remove(0) {
                    Sexp::Atom(s) => s,
                    _ => panic!(),
                };

                let module = match this.linked_modules.get(&segment) {
                    Some(m) => m,
                    _ => panic!(),
                };

                match path.remove(0) {
                    Sexp::Atom(s) => {
                        if s == "*" {
                            for (s, rc) in &module.exports {
                                imports.push((s.clone(), rc.clone()));
                            }
                        } else {
                            let import = match module.exports.get(&s) {
                                Some(rc) => {
                                    imports.push((s.clone(), rc.clone()));
                                }
                                _ => panic!(),
                            };
                        }
                    }
                    _ => panic!(),
                }
            }

            check_for_name_collisions(&defs, &macro_defs, &imports);

            // do macro expansion


            // by this point we know that there are no collisons,
            // and which symbols will be exported

            // process defs:

            let rev_topo = check_for_cyclic_deps(defs);
            let mut exports = HashMap::new();

            let defs: HashMap<String, Rc<(TExp, Type)>> = rev_topo
                .into_iter()
                .map(|(name, sexp)| {
                    let exp = sexp_to_exp(sexp);
                    let ty = g(&mut env, &exp);
                    let exp = types::deref_term(exp);
                    let texp = exp_to_texp(&mut conv_env, exp);
                    let rc = Rc::new((texp, ty));
                    if exported_syms.contains(&name) {
                        exports.insert(name.clone(), rc.clone());
                    }
                    (name, rc)
                })
                .chain(imports)
                .collect();

            for (name, rc) in &defs {
                let &(_, ref ty) = &**rc;
                env.vars.insert(name.clone(), ty.clone());
                conv_env.new_var(name.clone(), ty.clone());
            }

            let exps = sexps_to_exps(top_level);
            let exps = exps.into_iter().map(|e| f(&mut env, e)).collect_vec();

            let texps = exps.into_iter()
                .map(|e| exp_to_texp(&mut conv_env, e))
                .collect();


            Module {
                externs: this.externs,
                top_level: texps,
                defs,
                exports,
            }
        }

        parse_mod_impl(self, input.into())
    }
}

fn check_for_name_collisions(
    defs: &[(String, Sexp)],
    macro_defs: &[(String, (ParamList, Vec<Sexp>))],
    imports: &[(String, Rc<(TExp, Type)>)],
) {
    let mut names = HashSet::new();

    for &(ref s, _) in defs {
        if names.contains(&s) {
            panic!();
        }

        names.insert(s);
    }

    for &(ref s, _) in macro_defs {
        if names.contains(&s) {
            panic!();
        }

        names.insert(s);
    }

    for &(ref s, _) in imports {
        if names.contains(&s) {
            panic!();
        }

        names.insert(s);
    }
}

fn check_for_cyclic_deps(mut defs: Vec<(String, Sexp)>) -> Vec<(String, Sexp)> {
    fn get_deps(sexp: &Sexp, deps: &mut Vec<usize>, mapping: &HashMap<&str, usize>) {
        match *sexp {
            Sexp::Atom(ref s) => {
                if let Some(&i) = mapping.get(&**s) {
                    deps.push(i);
                }
            }
            Sexp::List(ref c) => {
                for s in c {
                    get_deps(s, deps, mapping);
                }
            }
        }
    }

    let mut g = pg::Graph::<usize, ()>::with_capacity(defs.len(), 0);

    {
        let mapping: HashMap<&str, usize> = defs.iter()
            .enumerate()
            .map(|(i, &(ref name, _))| (&**name, i))
            .collect();


        for i in 0..defs.len() {
            g.add_node(i);
        }

        for (i, &(ref name, ref sexp)) in defs.iter().enumerate() {
            let mut deps = vec![];
            get_deps(sexp, &mut deps, &mapping);

            for dep in deps {
                let n = pg::graph::NodeIndex::new;
                g.add_edge(n(i), n(dep), ());
            }
        }
    }

    let topo = pg::algo::toposort(&g, None).unwrap();

    for (i, id) in topo.into_iter().rev().enumerate() {
        let idx = id.index();
        defs.swap(i, idx);
    }

    defs
}

fn sexp_let(mut args: Vec<Sexp>) -> Ast {
    assert!(args.len() > 1);
    let body = sexps_to_exps(args.split_off(1));
    let params = match args.remove(0) {
        Sexp::List(params) => {
            assert!(params.len() % 2 == 0);
            let mut args = Vec::with_capacity(params.len() / 2);

            for (sym_sexp, exp_sexp) in params.into_iter().tuples() {
                let sym = match sym_sexp {
                    Sexp::Atom(sym) => sym,
                    _ => panic!(),
                };

                let ty = gentyp();
                let exp = sexp_to_exp(exp_sexp);

                args.push((sym, ty, exp));
            }

            args
        }
        _ => panic!("first arg to let must be a list"),
    };

    Ast::Let(params, body)
}

fn sexp_fn(mut args: Vec<Sexp>) -> Ast {
    assert!(args.len() > 1);
    let body = sexps_to_exps(args.split_off(1));
    let params = match args.remove(0) {
        Sexp::List(params) => {
            // (sym, typ)
            let mut args = Vec::with_capacity(params.len());

            for sym_sexp in params.into_iter() {
                let sym = match sym_sexp {
                    Sexp::Atom(sym) => sym,
                    _ => panic!(),
                };

                let ty = gentyp();
                args.push((sym, ty));
            }

            args
        }
        _ => panic!("first arg to fn must be a list"),
    };
    let ty = Type::Fun(
        repeat_call(gentyp).take(params.len()).collect(),
        Box::new(gentyp()),
    );
    Ast::Fn(ty, params, body)
}

fn sexp_if(mut args: Vec<Sexp>) -> Ast {
    match args.len() {
        2 => {
            let cond = sexp_to_exp(args.remove(0));
            let b1 = sexp_to_exp(args.remove(0));
            Ast::If(Box::new(cond), Box::new(b1))
        }
        3 => {
            let cond = sexp_to_exp(args.remove(0));
            let b1 = sexp_to_exp(args.remove(0));
            let b2 = sexp_to_exp(args.remove(0));
            Ast::IfElse(Box::new(cond), Box::new(b1), Box::new(b2))
        }
        _ => panic!(),
    }
}


fn sexp_quote(mut args: Vec<Sexp>) -> Ast {
    debug_assert_eq!(args.len(), 1);
    Ast::Quote(args.remove(0))
}


fn sexp_to_exp(s: Sexp) -> Ast {
    match s {
        Sexp::Atom(sym) => {
            match sym.parse() {
                Ok(val) => Ast::Int(val),
                _ => {
                    match &*sym {
                        "true" => Ast::Bool(true),
                        "false" => Ast::Bool(false),
                        _ => Ast::Var(sym),
                    }
                }
            }
        }
        Sexp::List(mut children) => {
            if children.is_empty() {
                Ast::Unit
            } else {
                let callee = sexp_to_exp(children.remove(0));
                match callee {
                    Ast::Var(sym) => {
                        match &*sym {
                            "+" => Ast::Add(sexps_to_exps(children)),
                            "let" => sexp_let(children),
                            "if" => sexp_if(children),
                            "fn" => sexp_fn(children),
                            "quote" => sexp_quote(children),
                            _ => Ast::App(Box::new(Ast::Var(sym)), sexps_to_exps(children)),
                        }
                    }
                    _ => Ast::App(Box::new(callee), sexps_to_exps(children)),
                }
            }
        }
    }
}

fn sexps_to_exps(s: Vec<Sexp>) -> Vec<Ast> {
    s.into_iter().map(sexp_to_exp).collect()
}

fn exp_to_texp(env: &mut CEnv, e: Ast) -> TExp {
    match e {
        Ast::Unit => TExp::Unit,
        Ast::Bool(val) => TExp::Bool(val),
        Ast::Int(val) => TExp::Int(val),
        Ast::Quote(sexp) => TExp::Quote(sexp),
        Ast::Let(bindings, body) => {
            scope! {
                env.vars => env.vars.clone();

                let bindings = bindings
                    .into_iter()
                    .map(|(sym, ty, exp)| {
                        let var = env.new_var(sym, ty);
                        let texp = exp_to_texp(env, exp);
                        (var, texp)
                    })
                    .collect();
                let body = body.into_iter().map(|e| exp_to_texp(env, e)).collect();

                TExp::Let(bindings, body)
            }
        }
        Ast::Fn(ty, params, body) => {
            scope! {
                env.vars => env.vars.clone();

                let params = params
                    .into_iter()
                    .map(|(sym, ty)| env.new_var(sym, ty))
                    .collect();

                let body = body.into_iter().map(|e| exp_to_texp(env, e)).collect();

                TExp::Fn(env.new_fn(params, body))
            }
        }
        Ast::Var(sym) => {
            match env.lookup(sym) {
                Some(ref var) => TExp::Var((*var).clone()),
                _ => unreachable!(),
            }
        }
        Ast::App(callee, args) => {
            let texps = iter::once(*callee)
                .chain(args)
                .map(|e| exp_to_texp(env, e))
                .collect();
            TExp::App(texps)
        }
        Ast::IfElse(cond, b1, b2) => {
            let cond = exp_to_texp(env, *cond);
            let b1 = exp_to_texp(env, *b1);
            let b2 = exp_to_texp(env, *b2);
            TExp::IfElse(Box::new((cond, b1, b2)))
        }
        Ast::If(cond, then) => {
            let cond = exp_to_texp(env, *cond);
            let then = exp_to_texp(env, *then);
            TExp::If(Box::new((cond, then)))
        }
        Ast::Add(args) => TExp::Add(args.into_iter().map(|e| exp_to_texp(env, e)).collect()),
    }
}

struct CEnv {
    counter: usize,
    vars: HashMap<String, Var>,
}

impl CEnv {
    fn new_var(&mut self, sym: String, ty: Type) -> Var {
        let id = self.counter;
        self.counter += 1;
        let info = Rc::new(VarInfo {
            sym: sym.clone(),
            ty,
        });
        let var = Var { id, info };
        self.vars.insert(sym, var.clone());
        var
    }
    fn new_fn(&mut self, params: Vec<Var>, body: Vec<TExp>) -> Fn {
        let id = self.counter;
        self.counter += 1;
        let info = Rc::new(FnInfo { params, body });
        Fn { id, info }
    }
    fn lookup<S: AsRef<str>>(&self, sym: S) -> Option<&Var> {
        self.vars.get(sym.as_ref())
    }
}
