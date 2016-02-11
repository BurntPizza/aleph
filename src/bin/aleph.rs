
extern crate aleph;
use aleph::reader;

extern crate itertools;
use itertools::*;

#[macro_use]
extern crate clap;

use clap::{App, AppSettings, SubCommand};


use std::fs::*;
use std::io::{stderr, Read, Write};


// mostly just temp impl for testing

fn main() {
    let matches = App::new("Aleph")
                      .about("The Aleph compiler")
                      .version(option_env!("CARGO_PKG_VERSION")
                                   .expect("Error: needs to build with Cargo"))
                      .settings(&[AppSettings::ArgRequiredElseHelp,
                                  AppSettings::SubcommandsNegateReqs])
                      .subcommand(SubCommand::with_name("read")
                                      .about("Evalutate expressions from CLI")
                                      .arg_from_usage("<exprs>... 'Expression(s) to evalutate'"))
                      .subcommand(SubCommand::with_name("compile")
                                      .about("Compile a file")
                                      .arg_from_usage("<file> 'File to compile'"))
                      .subcommand(SubCommand::with_name("dump")
                                      .about("(TMP) Dump the default readtable"))
                      .get_matches();

    if let Some(_) = matches.subcommand_matches("dump") {
        println!("{:#?}", reader::ReadTable::default());

    } else if let Some(m) = matches.subcommand_matches("read") {
        println!("{}",
                 reader::read_string(m.values_of("exprs").unwrap().join(" "))
                     .map(|form| format!("{}", form))
                     .unwrap_or_else(|e| {
                         e.into_iter().map(|err| format!("{:?}", err)).join("\n\n")
                     }));

    } else if let Some(m) = matches.subcommand_matches("compile") {
        match File::open(m.value_of("file").unwrap()) {
            Ok(mut f) => {
                match f.metadata().map(|m| m.is_file()) {
                    Ok(false) => {
                        write!(stderr(), "{}", "Input must be a file.")
                            .expect("failed to write to stderr");
                    }
                    Err(e) => {
                        write!(stderr(), "{}", e).expect("failed to write to stderr");
                    }
                    _ => {
                        let mut buf = String::new();
                        f.read_to_string(&mut buf).unwrap_or_else(|e| {
                            write!(stderr(), "{}", e).expect("failed to write to stderr");
                            std::process::exit(-1);
                        });
                        println!("{:?}", reader::read_string(buf));
                    }
                }
            }
            Err(e) => {
                write!(stderr(), "{}", e).expect("failed to write to stderr");
            }
        }

    }
}
