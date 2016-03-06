
//! The Core library: the language underpinnings.

use itertools::*;

use reader::{ReaderContext, ReadError};
use reader::CharSyntaxType::*;
use repr::Form;
use super::Args;

/// Reader macro function for line comments.
pub fn line_comment_reader(reader: &mut ReaderContext, _: u8) -> Result<Option<Form>, ()> {
    loop {
        match reader.stream.next() {
            Some(b'\n') | None => break,
            Some(_) => {}
        }
    }
    Ok(None)
}

/// Reader macro function for reading list s-expressions delimited by parentheses.
pub fn left_paren_reader(reader: &mut ReaderContext, _: u8) -> Result<Option<Form>, ()> {
    let mut list = vec![];

    loop {
        match reader.stream.next() {
            Some(w) if reader.readtable.get(w) == Whitespace => {}
            Some(c) if c == b')' => return Ok(Some(Form::list(list))),
            Some(_) => {
                reader.stream.unread();
                list.push(try!(reader.read()));
            }
            _ => {
                // TODO format!("Unexpected end of stream while reading list: `{:?}`", list)
                reader.output.push(ReadError::eos());
                return Err(());
            }
        }
    }
}

/// Reader macro function to abort on unexpected closing parentheses.
pub fn right_paren_reader(reader: &mut ReaderContext, _: u8) -> Result<Option<Form>, ()> {
    reader.output.push(ReadError::other("Unexpected right parenthesis"));
    Err(())
}

/// Print to stdout.
pub fn print(s: Args) -> Result<Form, String> {
    print!("{}", s.iter().join(" "));
    Ok(Form::empty_list())
}

/// Print to stdout with newline.
pub fn println(s: Args) -> Result<Form, String> {
    println!("{}", s.iter().join(" "));
    Ok(Form::empty_list())
}
