
//! The Aleph Programming Langauge

#[macro_use]
extern crate itertools;
extern crate petgraph;
extern crate disjoint_sets;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

pub mod lang;
pub mod read;
pub mod ast;
pub mod to_be_put_in_lang;
// mod typecheck;

// mod census;
// mod compile;
// mod print_table;

// pub mod reader;
// pub mod analyzer;
// pub mod symbol_table;
// pub mod interpreter; // this will eventually be entirely replaced by vm
// pub mod vm;
