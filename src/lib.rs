
//! The Aleph Programming Langauge

#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]
#![cfg_attr(not(feature="clippy"), allow(unknown_lints))]


extern crate itertools;
extern crate hamt;
extern crate byteorder;

pub mod symbol_table;
pub mod core;
pub mod reader;
pub mod analyzer;
pub mod interpreter; // this will eventually be entirely replaced by vm
pub mod vm;
