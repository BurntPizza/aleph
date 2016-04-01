
#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]

extern crate aleph;
use aleph::{reader, Environment};

extern crate itertools;
use itertools::*;

extern crate ansi_term;
use ansi_term::Colour;

#[macro_use]
extern crate clap;

use clap::{App, AppSettings, SubCommand};

use std::io::prelude::*;


fn main() {
    let version = option_env!("CARGO_PKG_VERSION").expect("Error: needs to build with Cargo");

    let matches = App::new("Aleph")
                      .about("The Aleph compiler")
                      .version(version)
                      .settings(&[AppSettings::ArgRequiredElseHelp,
                                  AppSettings::SubcommandsNegateReqs])
                      .subcommand(SubCommand::with_name("read")
                                      .about("Evalutate expressions from CLI")
                                      .arg_from_usage("<exprs>... 'Expression(s) to evalutate'"))
                      .subcommand(SubCommand::with_name("repl").about("Start a REPL"))
                      .subcommand(SubCommand::with_name("dump")
                                      .about("(TMP) Dump the default readtable"))
                      .get_matches();

    if let Some(_) = matches.subcommand_matches("dump") {
        println!("{:#?}", reader::ReadTable::default());

    } else if let Some(m) = matches.subcommand_matches("read") {
        println!("{}",
                 reader::read_string(m.values_of("exprs").unwrap().join(" "))
                     .and_then(|f| aleph::tmp_eval(&mut Environment::default(), f))
                     .map(|f| Colour::Green.paint(f.to_string()))
                     .unwrap_or_else(|e| Colour::Red.paint(e)));

    } else if let Some(_) = matches.subcommand_matches("repl") {
        // crappy repl
        println!("\nAleph {}\n\nEnter 'quit' to exit the REPL.", version);

        let mut env = Environment::default();

        loop {
            print!("\n> ");
            std::io::stdout().flush().unwrap();

            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            input = input.trim().to_owned();

            if input == "quit" {
                break;
            }

            println!("{}",
                     reader::read_string(input)
                         .and_then(|f| aleph::tmp_eval(&mut env, f))
                         .map(|f| Colour::Green.paint(f.to_string()))
                         .unwrap_or_else(|e| Colour::Red.paint(e)));
        }
    }
}
