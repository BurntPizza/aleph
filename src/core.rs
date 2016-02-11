
use reader::{ReaderContext, ReadError};
use reader::CharSyntaxType::*;
use form::Form;

pub fn line_comment_reader(reader: &mut ReaderContext, _: u8) -> Result<Option<Form>, ()> {
    loop {
        match reader.stream.next() {
            Some(c) if c == b'\n' => break,
            Some(_) => {}
            _ => break,
        }
    }
    Ok(None)
}

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

pub fn right_paren_reader(reader: &mut ReaderContext, _: u8) -> Result<Option<Form>, ()> {
    reader.output.push(ReadError::other("Unexpected right parenthesis"));
    Err(())
}
