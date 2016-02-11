

// fix TODOs, panics, etc.

// need to figure out default readtable. TODO: keep going
// then reader macro table. TODO: figure out where it goes

// TODO
// namespaces: both the datastructure itself, and the DS for tracking them while parsing/analysing
// overall DS is a stack, right? each 'stack frame' is an NS.


// readtables ;; TODO: COW functionality
// scope system ;; needs first: (namespace system?) ;; No, other way around
//     scopes are tracked by a stack, the overall structure is a tree, rooted at the root scope
//     what are namespaces exactly, if we already have scopes?
//     just a set of public symbols? parent info?

// macro expander ;; needs first: (scope system)
// AST representation

// TODO start on core lib


#![warn(missing_docs)]



extern crate itertools;

pub mod core;
pub mod form;
pub mod reader;



// TODOs

/// Note: guaranteed to be ASCII
pub struct InputStream {
    src: String,
    idx: usize,
}

impl InputStream {
    fn new(src: String) -> Self {        
        assert!(std::ascii::AsciiExt::is_ascii(&*src));
        InputStream {
            src: src,
            idx: 0
        }
    }

    fn unread(&mut self) {
        assert!(self.idx > 0);
        self.idx -= 1;
    }

    fn rewind(&mut self) {
        self.idx = 0;
    }
}

impl Iterator for InputStream {
    type Item = u8;
    fn next(&mut self) -> Option<Self::Item> {
        if self.idx < self.src.len() {
            let c = Some(self.src.as_bytes()[self.idx]);
            self.idx += 1;
            c
        } else {None}
    }
}


/*

Next: impl read() and organize reader module
then: analyze() & AST (just basic type system)

Get something working. Stop thinking so hard.
Don't need to implement namespaces to have basic interpretation going on.
ITERATE!

*/
