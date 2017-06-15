
use super::*;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Int(i64),
    Fn(usize),
    ExternFn(ExternFnPtr),
}

#[derive(Default)]
pub struct Interpreter {
    vars: HashMap<usize, Value>,
    fns: HashMap<usize, Rc<FnInfo>>,
    externs: HashMap<String, Value>,
}

impl Interpreter {
    pub fn exec_module(&mut self, m: &Module) -> Value {
        for (sym, e) in &m.externs {
            match *e {
                Extern::Fn { ptr, .. } => {
                    self.externs.insert(sym.clone(), Value::ExternFn(ptr));
                }
            }
        }

        // fix this?
        for (sym, rc) in &m.defs {
            let &(ref texp, _) = &**rc;
            let val = self.eval(texp);
            self.externs.insert(sym.clone(), val);
        }

        if let Some((last, rest)) = m.top_level.split_last() {
            for texp in rest {
                self.eval(texp);
            }
            self.eval(last)
        } else {
            Value::Unit
        }
    }

    fn eval(&mut self, exp: &TExp) -> Value {
        match *exp {
            TExp::Unit => Value::Unit,
            TExp::Bool(val) => Value::Bool(val),
            TExp::Int(val) => Value::Int(val),
            TExp::Add(ref args) => {
                Value::Int(
                    args.into_iter()
                        .map(|arg| match self.eval(arg) {
                            Value::Int(val) => val,
                            _ => unreachable!(),
                        })
                        .fold(0, |acc, e| acc + e),
                )
            }
            TExp::Var(ref var) => {
                if let Some(&val) = self.vars.get(&var.id) {
                    val
                } else {
                    self.externs[&var.info.sym]
                }
            }
            TExp::IfElse(ref exps) => {
                let (ref cond, ref b1, ref b2) = **exps;
                match self.eval(cond) {
                    Value::Bool(val) => if val { self.eval(b1) } else { self.eval(b2) },
                    _ => unreachable!(),
                }
            }
            TExp::If(ref exps) => {
                let (ref cond, ref then) = **exps;
                match self.eval(cond) {
                    Value::Bool(val) => {
                        if val {
                            self.eval(then);
                        }
                    }
                    _ => unreachable!(),
                }
                Value::Unit
            }
            TExp::App(ref exps) => {
                let (callee, args) = exps.split_first().unwrap_or_else(|| unreachable!());
                match self.eval(callee) {
                    Value::Fn(id) => {
                        let FnInfo {
                            ref params,
                            ref body,
                        } = *(self.fns[&id].clone());

                        scope! {
                            self.vars => self.vars.clone();

                            for (var, arg) in zip(params, args) {
                                let value = self.eval(arg);
                                self.vars.insert(var.id, value);
                            }

                            let (last, rest) = body.split_last().unwrap_or_else(|| unreachable!());

                            for exp in rest {
                                self.eval(exp);
                            }

                            let value = self.eval(last);

                            value
                        }
                    }
                    Value::ExternFn(ptr) => {
                        let args = args.into_iter().map(|e| self.eval(e)).collect_vec();
                        ptr.0(&*args)
                    }
                    _ => unreachable!(),
                }
            }
            TExp::Let(ref bindings, ref body) => {
                scope! {
                    self.vars => self.vars.clone();

                    for &(ref var, ref exp) in bindings {
                        let value = self.eval(exp);
                        self.vars.insert(var.id, value);
                    }

                    let (last, rest) = body.split_last().unwrap_or_else(|| unreachable!());

                    for exp in rest {
                        self.eval(exp);
                    }

                    let value = self.eval(last);

                    value
                }
            }
            TExp::Fn(Fn { id, ref info }) => {
                self.fns.insert(id, info.clone());
                Value::Fn(id)
            }
        }
    }
}
