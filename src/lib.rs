
//! The Aleph Programming Langauge

// temp
#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]


#[macro_use]
extern crate itertools;
extern crate petgraph;
extern crate ena;
extern crate disjoint_sets;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

extern crate tiered_map;

// pub mod _lang;
pub mod read;
// pub mod ast;
pub mod lang;
// mod typecheck;

// mod census;
// mod compile;
// mod print_table;

// pub mod reader;
// pub mod analyzer;
// pub mod symbol_table;
// pub mod interpreter; // this will eventually be entirely replaced by vm
// pub mod vm;
