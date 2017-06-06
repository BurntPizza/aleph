
use std::error::Error;
use std::rc::Rc;
use std::collections::HashMap;

use itertools::*;

use read::Sexp;


pub type AResult<T> = Result<T, Box<::std::error::Error>>;




#[cfg(test)]
mod tests {
    // extern crate slog_envlogger;

    use read::{self, InputStream, Sexp};

    pub fn read(text: &str) -> Vec<Sexp> {
        let mut env = Default::default();
        let mut stream = InputStream::new(text.to_string());

        read::read(&mut env, &mut stream).unwrap()
    }
}
