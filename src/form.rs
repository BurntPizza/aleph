

use self::Form::*;

pub enum Form {
    Atom(String),
    List(Option<Box<Form>>, Option<Box<Form>>),
}

impl Form {
    pub fn atom(s: String) -> Self {
        Atom(s)
    }

    pub fn list<I>(src: I) -> Self
        where I: IntoIterator<Item = Form> {
        let mut src: Vec<_> = src.into_iter().collect();        
        match src.len() {
            0 => List(None, None),
            _ => {
                let mut head = List(Some(Box::new(src.pop().unwrap())), None);
                for form in src.into_iter().rev() {
                    head = List(Some(Box::new(form)), Some(Box::new(head)));
                }
                head
            }
        }
    }
}
