


use std::ops::Deref;
use std::fmt::{self, Display, Debug, Formatter};


use itertools::PutBack;


pub type CharType = u8;

#[derive(Debug, Copy, Clone)]
pub struct StreamPosition {
    idx: usize,
}

const SP_DEFAULT: StreamPosition = StreamPosition { idx: 0 };

impl From<usize> for StreamPosition {
    fn from(idx: usize) -> Self {
        StreamPosition { idx: idx }
    }
}

#[derive(Copy, Clone)]
pub struct Char(StreamPosition, CharType);

impl Debug for Char {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Char({}, {})", self.0.idx, self.1 as char)
    }
}

impl Deref for Char {
    type Target = CharType;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<I, P> From<(P, I)> for Char
    where P: Into<StreamPosition>,
          I: Into<CharType>
{
    fn from(src: (P, I)) -> Self {
        use std::ascii::AsciiExt;
        let c = src.1.into();
        assert!(c.is_ascii(), "'Char's must be ASCII: `{}`", c);
        Char(src.0.into(), c)
    }
}

impl From<char> for Char {
    fn from(c: char) -> Self {
        (SP_DEFAULT, c as CharType).into()
    }
}

impl From<CharType> for Char {
    fn from(c: CharType) -> Self {
        Char::from(c as char)
    }
}

impl Display for Char {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.1 as char)
    }
}


pub type CharStream<I> = PutBack<I>;
