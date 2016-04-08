
use rose_tree::{self, RoseTree};
use hamt::HamtMap;
use itertools::*;

use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};

use super::repr::{Form, AstNode};

pub fn analyze_from_root(forms: Vec<Form>) -> Result<Analysis, AnalyzerError> {
    let mut analysis = Analysis {
        ast: root_ast(),
        symbol_table: root_symbol_table(),
    };

    for form in forms {
        try!(analyze_in_env(&form,
                            &mut analysis,
                            Ix::new(rose_tree::ROOT),
                            Ix::new(rose_tree::ROOT)));
    }

    Ok(analysis)
}

fn analyze_in_env(form: &Form,
                  analysis: &mut Analysis,
                  current_ast: Ix,
                  current_scope: Ix)
                  -> Result<(), AnalyzerError> {

    let ref mut node = analysis.ast[current_ast];
    let ref mut scope = analysis.symbol_table[current_scope];

    match *form {
        // visit and mutate analysis
        Form::Atom(ref s) => unimplemented!(),
        Form::List(ref v) => {
            match v.len() {
                0 => Err(AnalyzerError::empty_form()),
                1 => unimplemented!(),
                _ => {
                    let ref invocation = v[0];
                    let args = &v[1..];

                    match *invocation {
                        Form::Atom(ref span) => {
                            match scope.lookup(&span.text) {
                                Some(props) => {
                                    println!("K: `{}`, V: {:?}", span.text, props);
                                    Err(unimplemented!())
                                } 
                                None => Err(AnalyzerError::UndefinedIdent(span.text.clone())),
                            }
                        }
                        Form::List(ref v) => Err(unimplemented!()),
                    }
                }
            }
        }
    }
}

fn root_ast() -> RoseTree<(AstNode, Ix)> {
    RoseTree::new((AstNode::Do, Ix::new(rose_tree::ROOT))).0
}

fn root_symbol_table() -> RoseTree<SymbolTable> {
    let mut table = SymbolTable::new();
    // TODO
    let builtins = vec![("+", Default::default())];

    for kv in builtins {
        table.add(kv.0.to_owned(), kv.1);
    }

    RoseTree::new(table).0
}

type Ix = rose_tree::NodeIndex;

pub struct Analysis {
    ast: RoseTree<(AstNode, Ix)>,
    symbol_table: RoseTree<SymbolTable>,
}

pub type SymK = String;
pub type SymV = SymbolProps;
type ScopeTag = usize;

static SCOPE_TAG_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;

fn new_scope_tag() -> ScopeTag {
    SCOPE_TAG_COUNTER.fetch_add(1, Ordering::SeqCst)
}

pub struct SymbolTable {
    scope: ScopeTag,
    keyed_by_name: HamtMap<SymK, SymV>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scope: new_scope_tag(),
            keyed_by_name: HamtMap::new(),
        }
    }

    // TODO
    pub fn push_scope(&self) -> Self {
        SymbolTable {
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

    pub fn lookup(&self, key: &SymK) -> Option<&SymV> {
        self.keyed_by_name.find(key)
    }
}

#[derive(Default, Debug)]
pub struct SymbolProps {
    kind: SymbolKind,
}

#[derive(Debug)]
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
    EmptyForm,
    UndefinedIdent(String),
}

impl AnalyzerError {
    pub fn empty_form() -> Self {
        AnalyzerError::EmptyForm
    }
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
