
use hamt::HamtMap;
use itertools::*;

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};

use super::repr::Form;

pub fn analyze_from_root(form: &Form) -> Result<Analysis, AnalyzerError> {
    let mut env = root_symbol_table();

    analyze_in_env(form, &mut env).map(|_| {
        Analysis {
            ast: form,
            symbol_table: env,
        }
    })
}

fn analyze_in_env(form: &Form, env: &mut SymbolTable) -> Result<(), AnalyzerError> {

    unimplemented!()
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
    //          .fold_results(Form::list_of(vec![]), |acc, ast| acc.list_plus(ast))
    //     }
    // }
}

fn root_symbol_table<'a>() -> SymbolTable<'a> {
    unimplemented!()
}

pub struct Analysis<'a> {
    ast: &'a Form, // correct lifetime?
    symbol_table: SymbolTable<'a>,
}

pub type SymK = String;
pub type SymV = SymbolProps;
type ScopeTag = usize;

static SCOPE_TAG_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;

fn new_scope_tag() -> ScopeTag {
    SCOPE_TAG_COUNTER.fetch_add(1, Ordering::SeqCst)
}

pub struct SymbolTable<'p> {
    parent: Option<&'p SymbolTable<'p>>,
    scope: ScopeTag,
    keyed_by_name: HamtMap<SymK, SymV>,
}

impl<'p> SymbolTable<'p> {
    pub fn new() -> Self {
        SymbolTable {
            parent: None,
            scope: new_scope_tag(),
            keyed_by_name: HamtMap::new(),
        }
    }

    pub fn push_scope(&'p self) -> Self {
        SymbolTable {
            parent: Some(self),
            scope: new_scope_tag(),
            keyed_by_name: self.keyed_by_name.clone(),
        }
    }

    pub fn num_bindings(&self) -> usize {
        self.keyed_by_name.len()
    }

    pub fn add(&mut self, key: SymK, val: SymV) {
        // not sure I like the need to clone here, at least it's cheap.
        self.keyed_by_name = self.keyed_by_name.clone().plus(key, val);
    }
}

#[derive(Default)]
pub struct SymbolProps {
    kind: SymbolKind,
}

pub enum SymbolKind {
    Var,
}

impl Default for SymbolKind {
    fn default() -> Self {
        SymbolKind::Var
    }
}

#[derive(Debug)]
pub enum AnalyzerError {
    UndefinedIdent(String),
}

impl ::std::fmt::Display for AnalyzerError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        // TODO
        write!(f, "{:?}", self)
    }
}

impl ::std::error::Error for AnalyzerError {
    fn description(&self) -> &str {
        unimplemented!()
    }
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn symbol_table_add() {
        let mut table = SymbolTable::new();

        assert_eq!(table.num_bindings(), 0);
        table.add("".into(), Default::default());
        assert_eq!(table.num_bindings(), 1);
    }
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
