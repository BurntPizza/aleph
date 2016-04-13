
//! The Aleph Programming Langauge

#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]
#![cfg_attr(not(feature="clippy"), allow(unknown_lints))]


extern crate itertools;
extern crate hamt;

pub mod symbol_table;
pub mod core;
pub mod repr;
pub mod reader;
pub mod analyzer;
pub mod interpreter;

use reader::Form;

pub type Args<'a> = &'a [Form];
pub type Function = fn(Args) -> Result<Form, String>;
