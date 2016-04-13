
//! The Core library: the language underpinnings.

// TODO: ALL of this (expect maybe the reader macros)

use itertools::*;

use reader::{Form, ReaderEnv, ReadError};
use reader::CharSyntaxType::*;

pub type Args<'a> = &'a [Form];
pub type Function = fn(Args) -> Result<Form, String>;


/// Reader macro function for line comments.
pub fn line_comment_reader(reader: &mut ReaderEnv, _: u8) -> Result<Option<Form>, ()> {
    loop {
        match reader.stream.next() {
            Some(b'\n') | None => break,
            Some(_) => {}
        }
    }
    Ok(None)
}

/// Reader macro function for reading list s-expressions delimited by parentheses.
pub fn left_paren_reader(reader: &mut ReaderEnv, _: u8) -> Result<Option<Form>, ()> {
    let mut list = vec![];

    loop {
        match reader.stream.next() {
            Some(w) if reader.readtable.get(w) == Whitespace => {}
            Some(c) if c == b')' => return Ok(Some(Form::list(list))),
            Some(_) => {
                reader.stream.unread();
                list.push(try!(reader.read_token()));
            }
            _ => {
                reader.output.push(ReadError::eos());
                return Err(());
            }
        }
    }
}

/// Reader macro function to abort on unexpected closing parentheses.
pub fn right_paren_reader(reader: &mut ReaderEnv, _: u8) -> Result<Option<Form>, ()> {
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


#[cfg(test)]
mod test {
    use reader;

    #[test]
    fn test_line_comment_reader() {
        let input = "; hello\nworld".to_owned();
        let output = "world";
        assert_eq!(reader::read_string(input).unwrap().to_string(), output);
    }

    #[test]
    fn test_left_paren_reader() {
        let input = "(hello world)".to_owned();
        let output = input.clone();
        assert_eq!(reader::read_string(input).unwrap().to_string(), output);
    }

    #[test]
    fn test_right_paren_reader() {
        let input = ")".to_owned();
        let output = "Error: Unexpected right parenthesis\n";
        assert_eq!(reader::read_string(input).err().unwrap(), output);
    }
}
