
use hamt::HamtMap;
use itertools::*;

use std::error::Error;
use std::fmt::{self, Debug, Formatter};
use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};

pub struct SymbolTable {
    ident_map: HamtMap<String, u32>,
    id_map: HamtMap<u32, Record>,
}

impl SymbolTable {
    pub fn empty() -> Self {
        SymbolTable {
            ident_map: Default::default(),
            id_map: Default::default(),
        }
    }

    pub fn fresh_numbered_ident<T: Into<String>>(&mut self, ident: T, kind: BindingKind) -> Record {
        let ident = ident.into();
        let mut n = 0;

        loop {
            match self.add_ident(format!("{}${}", ident, n), kind) {
                Ok(r) => return r,
                Err(_) => n += 1,
            }
        }
    }

    pub fn add_ident<T: Into<String>>(&mut self,
                                      ident: T,
                                      kind: BindingKind)
                                      -> Result<Record, Box<Error>> {
        let ident = ident.into();

        if let Some(record) = self.lookup_ident(&*ident) {
            let msg = format!("Error adding identifier {} to {:?}, that record already exists: \
                               {:#?}",
                              ident,
                              self,
                              record);
            return Err(msg.into());
        }

        let id = next_id();
        let record = Record {
            id: id,
            ident: ident.clone(),
            kind: kind,
        };

        self.ident_map = self.ident_map.clone().plus(ident, id);
        self.id_map = self.id_map.clone().plus(id, record);

        Ok(self.lookup_id(id).unwrap().clone())
    }

    pub fn lookup_ident<T: Into<String>>(&self, ident: T) -> Option<Record> {
        self.ident_map.find(&ident.into()).and_then(|&id| self.lookup_id(id))
    }

    pub fn lookup_id(&self, id: u32) -> Option<Record> {
        self.id_map.find(&id).cloned()
    }

    // TODO tree api
    // probably leave it until it's needed (and thus requirements are known)
}

#[derive(Debug, Clone)]
pub struct Record {
    ident: String,
    kind: BindingKind,
    id: u32,
}

impl Record {
    pub fn id(&self) -> u32 {
        self.id
    }

    pub fn ident(&self) -> &str {
        &*self.ident
    }

    pub fn kind(&self) -> BindingKind {
        self.kind
    }
}

// TODO function ref?
custom_derive! {
    #[derive(Copy, Clone, IterVariantNames(BindingKindVariantNames))]
    pub enum BindingKind {
        // builtins
        Special,
        // user defined:
        Fn,
        Macro,
        Var,
        Constant,
    }
}

impl Debug for BindingKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let string = match *self {
            BindingKind::Special => "Special",
            BindingKind::Fn => "Fn",
            BindingKind::Macro => "Macro",
            BindingKind::Constant => "Constant",
            BindingKind::Var => "Var",
        };

        write!(f, "{}", string)
    }
}

fn next_id() -> u32 {
    static COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;

    COUNTER.fetch_add(1, Ordering::SeqCst) as u32
}


impl Debug for SymbolTable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use std::cmp;

        let mut records = self.id_map.iter().map(|kv| kv.1).collect::<Vec<_>>();
        records.sort_by(|a, b| a.id().cmp(&b.id()));

        let id_size = cmp::max("id".len(),
                               records.last().map_or(0, |r| r.id().to_string().len()));

        let ident_size = cmp::max("ident".len(),
                                  records.iter().map(|r| r.ident().len()).max().unwrap_or(0));

        let var_kind_size: usize = BindingKind::iter_variant_names()
                                       .map(|s| s.len())
                                       .max()
                                       .unwrap();

        let header = format!("| {:^3$} | {:^4$} | {:^5$} |",
                             "id",
                             "ident",
                             "kind",
                             id_size,
                             ident_size,
                             var_kind_size);

        let sep = format!("|-{0:->1$}-|-{0:->2$}-|-{0:->3$}-|",
                          "",
                          id_size,
                          ident_size,
                          var_kind_size);

        try!(writeln!(f, "SymbolTable:"));
        try!(writeln!(f, "{}", header));
        try!(writeln!(f, "{}", sep));

        for r in records {
            let r_kind = match r.kind {
                BindingKind::Special => "special",
                BindingKind::Fn => "fn",
                BindingKind::Macro => "macro",
                BindingKind::Constant => "constant",
                BindingKind::Var => "var",
            };

            let out = format!("| {:>3$} | {:>4$} | {:>5$} |",
                              r.id(),
                              r.ident(),
                              r_kind,
                              id_size,
                              ident_size,
                              var_kind_size);

            try!(writeln!(f, "{}", out))
        }

        Ok(())
    }
}
