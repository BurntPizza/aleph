
use std::collections::HashMap;

use read::{Sexp};

pub struct MacroExpansionEnv {
    macros: HashMap<String, Box<Fn(Vec<Sexp>) -> Vec<Sexp>>>,
}

impl MacroExpansionEnv {
    pub fn macro_expand(&mut self, mut sexps: Vec<Sexp>) -> Vec<Sexp> {
        loop {
            let mut changed = false;
            let mut new_sexps = Vec::with_capacity(sexps.len());

            for sexp in sexps {
                match self.macro_expand1(sexp) {
                    Ok(expansion) => {
                        new_sexps.extend(expansion);
                        changed = true;
                    }
                    Err(sexp) => new_sexps.push(sexp),
                }
            }

            if !changed {
                return new_sexps;
            }

            sexps = new_sexps;
        }
    }

    fn macro_expand1(&self, sexp: Sexp) -> Result<Vec<Sexp>, Sexp> {
        match sexp {
            Sexp::List(mut children) => {
                if children.is_empty() {
                    return Err(Sexp::List(children));
                }

                match children[0].clone() {
                    Sexp::Atom(sym) => {
                        match self.macros.get(&sym) {
                            Some(ref f) => {
                                children.remove(0);
                                Ok(f(children))
                            }
                            _ => Err(Sexp::List(children)),
                        }
                    }
                    _ => Err(Sexp::List(children)),
                }
            }
            _ => Err(sexp),
        }
    }
}

impl Default for MacroExpansionEnv {
    fn default() -> Self {
        let mut macros = HashMap::new();

        let do_fn = |mut sexps: Vec<Sexp>| {
            let let_atom = Sexp::Atom("let".into());
            let bindings = Sexp::List(vec![]);
            sexps.insert(0, let_atom);
            sexps.insert(1, bindings);
            vec![Sexp::List(sexps)]
        };

        macros.insert(
            "do".into(),
            Box::new(do_fn) as Box<Fn(Vec<Sexp>) -> Vec<Sexp>>,
        );

        MacroExpansionEnv { macros }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::*;

    #[test]
    fn test_do() {
        let input = "(do 1 () 3)";
        let expected_val = Value::Int(3);

        let m = ModuleBuilder::default().parse_mod(input);
        let mut interp = Interpreter::default();
        let result = interp.exec_module(&m);

        assert_eq!(result, expected_val);
    }
}
