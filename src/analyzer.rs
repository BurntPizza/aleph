
use hamt::HamtMap;
use itertools::*;

use std::collections::HashMap;

use super::repr::{Form, Ast};

pub fn analyze_from_root<'a>(form: &Form) -> Result<(AnalyzerEnv<'a>, Ast), AnalyzerError> {
    let mut env = AnalyzerEnv::root();
    analyze_in_env(form, &mut env).map(|result| (env, result))
}

fn analyze_in_env(form: &Form, env: &mut AnalyzerEnv) -> Result<Ast, AnalyzerError> {
    // match form {
    //     Form::Atom(s) => {
    //         env.current_scope
    //            .defined
    //            .get(&s.text)
    //             // dunno about this yet
    //            .map_or(Err(AnalyzerError::UndefinedIdent(s.text)), |&bind_key| {
    //                // TODO
    //                unimplemented!()
    //            })
    //     }
    //     Form::List(v) => {
    //         v.into_iter()
    //          .map(|form| analyze_in_env(form, env))
    //          .fold_results(Ast::list_of(vec![]), |acc, ast| acc.list_plus(ast))
    //     }
    // }
}


// pub struct AnalyzerEnv<'s> {
//     pub current_scope: ScopeEnv<'s>,
//     pub errors: Vec<AnalyzerError>,
// }

// impl<'s> AnalyzerEnv<'s> {
//     pub fn root() -> Self {
//         AnalyzerEnv {
//             current_scope: ScopeEnv::root(),
//             errors: vec![],
//         }
//     }
// }

// type ScopeTag = u32;

// // TODO
// #[derive(PartialEq, Eq, Hash, Copy, Clone)]
// pub struct BindKey {
//     scope: ScopeTag,
//     tag: u32,
// }

// pub type Binding = String;

// type Bindings = HamtMap<BindKey, Binding>;


// pub struct ScopeEnv<'p> {
//     tag: ScopeTag,
//     parent: Option<&'p ScopeEnv<'p>>,
//     bindings: Bindings, // HamtMap<BindKey, Binding>,
//     defined: HashMap<Binding, BindKey>,
// }

// static SCOPE_TAG: ScopeTag = 0;

// fn new_scope_tag() -> ScopeTag {
//     let scope_tag = SCOPE_TAG;

//     SCOPE_TAG.checked_add(1);
//     scope_tag
// }

// impl<'p> ScopeEnv<'p> {
//     pub fn root() -> Self {
//         ScopeEnv {
//             tag: new_scope_tag(),
//             parent: None,
//             bindings: root_bindings(),
//             defined: Default::default(),
//         }
//     }

//     pub fn push_scope(&self) -> ScopeEnv {
//         ScopeEnv {
//             tag: new_scope_tag(),
//             parent: Some(self),
//             bindings: self.bindings.clone(),
//             defined: Default::default(),
//         }
//     }

//     // establishes a binding that with be unique in this scope
//     pub fn bind(mut self, key: Binding) -> Self {
//         let val = self.hash(&key);
//         self.defined.insert(key, val);
//         self
//     }

//     pub fn get_binding(&self, key: &BindKey) -> Option<&Binding> {
//         self.bindings.find(key)
//     }

//     // create new
//     fn hash(&self, k: &Binding) -> BindKey {
//         unimplemented!()
//     }
// }

// #[derive(Debug)]
// pub enum AnalyzerError {
//     UndefinedIdent(String),
// }

// impl ::std::fmt::Display for AnalyzerError {
//     fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
//         // TODO
//         write!(f, "{:?}", self)
//     }
// }

// impl ::std::error::Error for AnalyzerError {
//     fn description(&self) -> &str {
//         unimplemented!()
//     }
// }



// // maybe move to core.rs
// fn root_bindings() -> Bindings {
//     // TODO
//     // generate from core lib
//     HamtMap::new()
// }


// #[cfg(test)]
// mod test {
//     use super::*;
//     use super::super::repr::Val;

//     #[test]
//     fn test_scope_push_pop() {
//         let mut root = ScopeEnv::root();

//         for s in vec!["hello", "world"] {
//             root = root.bind(s.to_owned());
//         }

//         {
//             let mut new_scope = root.push_scope();

//             for s in vec!["hello", "earth"] {
//                 new_scope = new_scope.bind(s.to_owned());
//             }

//             assert_eq!(new_scope.bindings.len(), 3);
//             // assert_eq!(new_scope.get_binding(&Val::new(2)),
//             //            Some(&"hello".to_owned()));
//         }

//         assert_eq!(root.bindings.len(), 2);
//         // assert_eq!(root.get_binding(&Val::new(0)), Some(&"hello".to_owned()));
//     }
// }
