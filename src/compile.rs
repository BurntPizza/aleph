
// TODO: rename module to vm

use std::mem;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

use byteorder::*;
use itertools::*;

use lang::{Env, Ast, BindingKey, Binding, ConstType, Result};

type Endianness = NativeEndian;
type Table<K, V> = HashMap<K, V>;

type ConstTableIdx = u16;
type FnAddrT = u32;
type FnDefIdT = u32;


pub fn compile(env: &Env, ast: &[Ast]) -> Result<Program> {

    // defs are stored in env, as are fn prototypes

    let mut code = vec![];
    let mut bcode = vec![];
    let mut callsites = vec![];
    let mut jumpsites = vec![];
    let mut fn_id_to_fn_addr = Table::new();
    let mut label_id_to_addr = Table::new();
    let mut const_table = vec![];

    let mut labels = LabelGen::new();

    // toplevel codegen
    for node in ast {
        codegen(env, &mut labels, node, &mut code);
    }

    code.push(MIns::Exit);

    // fn codegen
    for def in env.fn_defs() {
        code.push(MIns::FnDefBegin(FnDefId::from(def.id())));

        for node in def.ast() {
            codegen(env, &mut labels, node, &mut code);
        }
    }

    println!("\n===\n{}\n===",
             code.iter().map(|m| format!("{:?}", m)).join("\n"));

    // TODO: opt passes here

    for mins in code {
        if mins.is_simple() {
            mins.emit(&mut bcode);
        } else {
            match mins {
                MIns::FnDefBegin(id) => {
                    fn_id_to_fn_addr.insert(id, FnAddr::from(bcode.len() as FnAddrT));
                }
                MIns::I64(val) => {
                    let idx = const_table.len() as ConstTableIdx;
                    const_table.push(val); // TODO: Slot

                    bcode.push(Ins::LoadConst.into());
                    bcode.write_u16::<Endianness>(idx).unwrap();
                }
                MIns::CallFn(id) => {
                    bcode.push(Ins::Call.into());

                    let current_idx = bcode.len();
                    callsites.push((current_idx, id));

                    bcode.write_u32::<Endianness>(0 as FnAddrT).unwrap();
                }
                MIns::Label(id) => {
                    label_id_to_addr.insert(id, FnAddr::from(bcode.len() as FnAddrT));
                }
                MIns::Jmp(id) => {
                    bcode.push(Ins::Jmp.into());
                    jumpsites.push((id, bcode.len()));
                    bcode.write_u32::<Endianness>(0 as FnAddrT).unwrap();
                }
                MIns::JmpIfFalse(id) => {
                    bcode.push(Ins::JmpF.into());
                    jumpsites.push((id, bcode.len()));
                    bcode.write_u32::<Endianness>(0 as FnAddrT).unwrap();
                }
                _ => unreachable!(),
            }
        }
    }

    // patch jumpsites
    for (id, addr) in jumpsites {
        let target_addr = label_id_to_addr[&id].into();
        Endianness::write_u32(&mut bcode[addr..addr + mem::size_of::<FnAddrT>()],
                              target_addr);
    }

    // patch callsites
    for (i, id) in callsites {
        let addr = fn_id_to_fn_addr[&id].into();
        Endianness::write_u32(&mut bcode[i..i + mem::size_of::<FnAddrT>()], addr);
    }

    Ok(Program {
        code: bcode,
        const_pool: const_table,
    })
}

struct LabelGen(u64);

impl LabelGen {
    fn new() -> Self {
        LabelGen(0)
    }

    fn new_label(&mut self) -> LabelId {
        let id = self.0;
        self.0 += 1;
        LabelId::from(id)
    }
}

fn codegen(env: &Env, labels: &mut LabelGen, ast: &Ast, code: &mut Vec<MIns>) {
    match *ast {
        Ast::I64Literal(span, val) => {
            code.push(MIns::I64(val));
        }
        Ast::BoolLiteral(span, val) => {
            code.push(MIns::Bool(val));
        }
        Ast::Atom(span, ref binding_key) => {
            let record = match *binding_key {
                BindingKey::String(ref k) => {
                    env.lookup_by_name(k).expect("no binding found by that name")
                }
                BindingKey::Id(k) => env.lookup_by_id(k),
            };

            match *record.binding() {
                // 'x' after '(def x 10)'
                Binding::Const(ConstType::I64(val)) => {
                    code.push(MIns::I64(val));
                }
                _ => unimplemented!(),
            }
        }
        Ast::EmptyList(..) => unimplemented!(),
        Ast::Do(..) => unimplemented!(),
        Ast::Fn(..) => unimplemented!(),
        Ast::If(_, ref cond_expr, ref then_expr, ref else_expr) => {
            let label1 = labels.new_label();
            let label2 = labels.new_label();

            codegen(env, labels, &**cond_expr, code);
            code.push(MIns::JmpIfFalse(label1));

            codegen(env, labels, &**then_expr, code);
            code.push(MIns::Jmp(label2));
            code.push(MIns::Label(label1));

            codegen(env, labels, &**else_expr, code);
            code.push(MIns::Label(label2));
        }
        Ast::Inv(span, ref callee, ref args) => {
            // lookup callee
            // lookup and unify args with callee sig

            let fn_def_id = unimplemented!();

            // codegen:
            // gen each arg
            // emit fn_ptr/call

            // note the order
            for arg in args.iter() {
                codegen(env, labels, arg, code);
            }

            code.push(MIns::CallFn(fn_def_id));
        }
        Ast::Let(..) => unimplemented!(),
    }
}



// macro assembler instructions
#[derive(Clone, Debug)]
enum MIns {
    I64(i64),
    Bool(bool),

    Jmp(LabelId),
    JmpIfFalse(LabelId),
    Label(LabelId),
    // num_args
    Add(usize),
    LoadLocal(u8), // idx in local table
    SaveLocal(u8), // idx in local table
    CallFn(FnDefId),
    Ret,
    Exit,

    FnPtr(FnDefId),
    CallPtr,

    FnDefBegin(FnDefId),
    MarkStack,
    PopToMark,
}

impl MIns {
    fn is_simple(&self) -> bool {
        match *self {
            MIns::FnDefBegin(..) |
            MIns::I64(..) |
            MIns::CallFn(..) |
            MIns::Jmp(..) |
            MIns::JmpIfFalse(..) |
            MIns::Label(..) => false,
            _ => true,
        }
    }

    fn emit(&self, code: &mut Vec<u8>) {
        match *self {
            MIns::Bool(val) => {
                code.push(if val {
                              Ins::LoadTrue
                          } else {
                              Ins::LoadFalse
                          }
                          .into())
            }
            MIns::Add(num_args) => unimplemented!(),
            MIns::LoadLocal(s) => unimplemented!(),
            MIns::SaveLocal(s) => unimplemented!(),
            MIns::CallFn(id) => unimplemented!(),
            MIns::Ret => unimplemented!(),
            MIns::Exit => code.push(Ins::Exit.into()),

            MIns::FnPtr(id) => unimplemented!(),
            MIns::CallPtr => unimplemented!(),

            MIns::MarkStack => unimplemented!(),
            MIns::PopToMark => unimplemented!(),

            _ => unreachable!(),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
#[repr(u8)]
enum Ins {
    Fail,

    Nop,

    LoadTrue,
    LoadFalse,
    LoadConst,

    FnPtr,
    CallPtr,

    Load,
    Save,

    Pop,
    Dup,

    Add,

    Call,
    Ret,

    Jmp,
    JmpF,

    Exit,

    MarkStack,
    PopToMark,
}

#[derive(Copy, Clone)]
enum Slot {
    Bool(bool),
    I64(i64),
}

impl Display for Slot {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let s = match *self {
            Slot::Bool(val) => val.to_string(),
            Slot::I64(val) => val.to_string(),
        };
        write!(f, "{}", s)
    }
}

pub struct Program {
    code: Vec<u8>,
    const_pool: Vec<i64>,
}

impl Program {
    pub fn exec(&self) -> String {
        let code = &*self.code;
        let const_pool = &*self.const_pool;

        let mut data_stack = vec![];
        // allocate local-var buffers in same stack?
        // let mut call_stack = vec![];

        let mut ins_ptr = 0;

        loop {
            let i = ins_ptr;
            ins_ptr += 1;

            match Ins::from(code[i]) {
                Ins::Fail => unreachable!(),
                Ins::Nop => {}
                Ins::LoadTrue => {
                    data_stack.push(Slot::Bool(true));
                }
                Ins::LoadFalse => {
                    data_stack.push(Slot::Bool(false));
                }
                Ins::LoadConst => {
                    let idx = Endianness::read_u16(&code[ins_ptr..ins_ptr + 2]) as usize;
                    ins_ptr += 2;
                    data_stack.push(Slot::I64(const_pool[idx]));
                }
                Ins::FnPtr => unimplemented!(),
                Ins::CallPtr => unimplemented!(),
                Ins::Load => unimplemented!(),
                Ins::Save => unimplemented!(),
                Ins::Pop => unimplemented!(),
                Ins::Dup => unimplemented!(),
                Ins::Add => unimplemented!(),
                Ins::Call => unimplemented!(),
                Ins::Ret => unimplemented!(),
                Ins::Jmp => {
                    ins_ptr = Endianness::read_u32(&code[ins_ptr..ins_ptr + 4]) as usize;
                }
                Ins::JmpF => {
                    let addr = Endianness::read_u32(&code[ins_ptr..ins_ptr + 4]) as usize;
                    ins_ptr += 4;
                    match data_stack.pop().unwrap() {
                        Slot::Bool(val) => {
                            if !val {
                                ins_ptr = addr;
                            }
                        }
                        _ => panic!("Expected bool"),
                    }
                }
                Ins::Exit => break,
                Ins::MarkStack => unimplemented!(),
                Ins::PopToMark => unimplemented!(),
            }
        }

        format!("{}", data_stack.pop().unwrap())
    }
}

macro_rules! def_id {
    ($name:ident, $ty:ty) => {
        #[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
        struct $name($ty);

        impl ::std::convert::From<$ty> for $name {
            #[inline]
            fn from(val: $ty) -> Self {
                $name(val)
            }
        }

        impl ::std::convert::From<$name> for $ty {
            #[inline]
            fn from(val: $name) -> Self {
                val.0
            }
        }
    }
}


def_id!(FnAddr, FnAddrT);
def_id!(FnDefId, FnDefIdT);
def_id!(LabelId, u64);

impl ::std::convert::From<u8> for Ins {
    #[inline]
    fn from(byte: u8) -> Self {
        unsafe { ::std::mem::transmute(byte) }
    }
}

impl ::std::convert::From<Ins> for u8 {
    #[inline]
    fn from(ins: Ins) -> Self {
        unsafe { ::std::mem::transmute(ins) }
    }
}

#[inline]
fn uninitialized_vec<T>(len: usize) -> Vec<T> {
    let mut v = Vec::with_capacity(len);
    unsafe { v.set_len(len) };
    v
}
