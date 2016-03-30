
use hamt;
use itertools::*;

use super::repr::{Form, Ast};

pub fn analyze_from_root(form: Form) -> Result<Ast, String> {
    analyze_in_env(form, &mut AnalyzerEnv::root()).map_err(|e| format!("Error: {:?}", e))
}

fn analyze_in_env<'s>(form: Form, env: &mut AnalyzerEnv<'s>) -> Result<Ast, AnalyzerError> {
    match form {
        Form::Atom(s) => {
            env.current_scope
               .get_binding(&s.text)
               .map_or(Err(AnalyzerError::UndefinedIdent(s.text)), |&b| unimplemented!()) // TODO:
        }
        Form::List(v) => {
            v.into_iter()
             .map(|form| analyze_in_env(form, env))
             .fold_results(Ast::list_of(vec![]), |acc, ast| acc.list_plus(ast))
        }
    }
}

pub struct AnalyzerEnv<'s> {
    pub current_scope: ScopeEnv<'s>,
    pub errors: Vec<AnalyzerError>,
}

impl<'s> AnalyzerEnv<'s> {
    pub fn root() -> Self {
        AnalyzerEnv {
            current_scope: ScopeEnv::root(),
            errors: vec![],
        }
    }
}

pub struct ScopeEnv<'p> {
    parent: Option<&'p ScopeEnv<'p>>,
    bindings: Bindings,
}

impl<'p> ScopeEnv<'p> {
    pub fn root() -> Self {
        ScopeEnv {
            parent: None,
            bindings: root_bindings(),
        }
    }

    pub fn push_scope(&self) -> ScopeEnv {
        ScopeEnv {
            parent: Some(self),
            bindings: self.bindings.clone(),
        }
    }

    pub fn bind(self, key: BindKey, value: Binding) -> Self {
        ScopeEnv {
            parent: self.parent,
            bindings: self.bindings.plus(key, value),
        }
    }

    pub fn get_binding(&self, key: &BindKey) -> Option<&Binding> {
        self.bindings.find(key)
    }
}

#[derive(Debug)]
pub enum AnalyzerError {
    UndefinedIdent(String),
}

// TODO
pub type Binding = usize;


pub type BindKey = String;
type Bindings = hamt::HamtMap<BindKey, Binding>;

// maybe move to core.rs
fn root_bindings() -> Bindings {
    // TODO
    // generate from core lib
    hamt::HamtMap::new()
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_scope_push_pop() {
        let mut root = ScopeEnv::root();

        for (s, b) in vec![("hello", 0), ("world", 1)] {
            root = root.bind(s.to_owned(), b);
        }

        {
            let mut new_scope = root.push_scope();

            for (s, b) in vec![("hello", 2), ("earth", 3)] {
                new_scope = new_scope.bind(s.to_owned(), b);
            }

            assert_eq!(new_scope.bindings.len(), 3);
            assert_eq!(new_scope.get_binding(&"hello".to_owned()), Some(&2));
        }

        assert_eq!(root.bindings.len(), 2);
        assert_eq!(root.get_binding(&"hello".to_owned()), Some(&0));
    }
}
