

// fix TODOs, panics, etc.

// need to figure out default readtable. TODO: keep going
// then reader macro table. TODO: figure out where it goes

// TODO
// namespaces: both the datastructure itself, and the DS for tracking them while parsing/analysing
// overall DS is a stack, right? each 'stack frame' is an NS.


// readtables ;; TODO: COW functionality
// scope system ;; needs first: (namespace system?) ;; No, other way around
//     scopes are tracked by a stack, the overall structure is a tree, rooted at the root scope
//     what are namespaces exactly, if we already have scopes?
//     just a set of public symbols? parent info?

// macro expander ;; needs first: (scope system)
// AST representation


#![warn(missing_docs)]

extern crate itertools;



pub mod reader;



#[cfg(test)]
extern crate quickcheck;
