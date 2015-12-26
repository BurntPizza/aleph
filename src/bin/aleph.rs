
extern crate aleph;
use aleph::reader;


#[macro_use]
extern crate clap;

use clap::{App, AppSettings, SubCommand};



fn main() {
    let matches = App::new("aleph")
                      .about("The Aleph compiler")
                      .version(option_env!("CARGO_PKG_VERSION")
                                   .expect("Error: needs to build with Cargo"))
                      .settings(&[AppSettings::ArgRequiredElseHelp])
                      .subcommand(SubCommand::with_name("read")
                                      .arg_from_usage("<EXPR>... 'Expression(s) to evalutate'"))
                      .subcommand(SubCommand::with_name("dump"))
                      .get_matches();

    if let Some(_) = matches.subcommand_matches("dump") {
        println!("{:#?}", reader::ReadTable::default());
        std::process::exit(0);
    }
    
    println!("{}",
             reader::parse(matches.subcommand_matches("read")
                                         .expect("sub")
                                         .values_of("EXPR")
                                         .unwrap()
                                         .join(" ")));
}
