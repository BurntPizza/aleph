
use itertools::*;
use byteorder::*;

use std::io::Cursor;
use std::collections::HashMap;
use std::fmt::{self, Display, Debug, Formatter};

use symbol_table::*;

type Endianness = NativeEndian;
type Table<K, V> = HashMap<K, V>;
#[derive(Copy, Clone, PartialEq, Debug)]
enum Ins {
    Nop,

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

    Exit,

    MarkStack,
    PopToMark,
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum SlotType {
    UnInit,
    I64,
    FnPtr,
}

#[derive(Copy, Clone, Debug)]
struct Slot {
    val: u64,
    ty: SlotType,
}

struct StackFrame {
    locals: Vec<Slot>,
    return_addr: u64,
}

impl StackFrame {
    fn load_local(&self, idx: u8) -> Slot {
        self.locals[idx as usize]
    }

    fn save_local(&mut self, idx: u8, val: Slot) {
        self.locals[idx as usize] = val;
    }
}

impl Slot {
    fn uninitialized() -> Self {
        Slot {
            val: 0,
            ty: SlotType::UnInit,
        }
    }

    fn from_i64(val: i64) -> Self {
        Slot {
            val: val as u64,
            ty: SlotType::I64,
        }
    }

    fn from_fn_ptr(val: u32) -> Self {
        Slot {
            val: val as u64,
            ty: SlotType::FnPtr,
        }
    }

    fn as_i64(&self) -> i64 {
        assert_eq!(self.ty, SlotType::I64);
        self.val as i64
    }

    fn as_fn_ptr(&self) -> FnDefId {
        assert_eq!(self.ty, SlotType::FnPtr);
        FnDefId::from(self.val as u32)
    }
}

impl Display for Slot {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.ty {
            SlotType::I64 => write!(f, "{}", self.as_i64()),
            SlotType::FnPtr => {
                let fn_var_id = self.as_fn_ptr().0;
                // let ident = env.lookup_id(fn_var_id).unwrap().ident();
                write!(f, "<fn {}>", fn_var_id)
            }
            SlotType::UnInit => panic!(),
        }
    }
}

// TODO: return type
pub fn exec_program(program: Program) -> String {
    use self::Ins::*;

    let const_table = program.const_table;
    let fn_table = program.fn_table;
    let mut program = Cursor::new(program.instructions);

    let mut call_stack = vec![fn_table.new_stack_frame(0, FnAddr::from(0))];
    let mut data_stack = Vec::with_capacity(128);

    let mut stack_marker = data_stack.len();

    loop {
        match program.read_u8().unwrap().into() {
            Nop => {}
            LoadConst => {
                let const_idx = program.read_u8().unwrap();
                let val = const_table[const_idx as usize];
                data_stack.push(val);
            }
            FnPtr => {
                let fn_def_id = program.read_u32::<Endianness>().unwrap();
                data_stack.push(Slot::from_fn_ptr(fn_def_id));
            }
            CallPtr => {
                let slot = data_stack.pop().unwrap();
                let fn_def_id = slot.as_fn_ptr();
                let addr = fn_table.get_fn_addr(fn_def_id).unwrap();
                let new_frame = fn_table.new_stack_frame(program.position(), addr);
                call_stack.push(new_frame);
                program.set_position(u32::from(addr) as u64);
            }
            Load => {
                let idx = program.read_u8().unwrap();
                let val = call_stack.last().unwrap().load_local(idx);
                data_stack.push(val);
            }
            Save => {
                let idx = program.read_u8().unwrap();
                let val = data_stack.pop().unwrap();
                call_stack.last_mut().unwrap().save_local(idx, val);
            }
            Pop => {
                data_stack.pop().unwrap();
            }
            Dup => {
                let val = data_stack.pop().unwrap();
                data_stack.push(val);
                data_stack.push(val);
            }
            Add => {
                let num_args = program.read_u8().unwrap() as usize;
                assert!(data_stack.len() >= num_args, "stack underflow: add");
                let idx = data_stack.len() - num_args;
                let val = data_stack.drain(idx..)
                                    .map(|slot| slot.as_i64())
                                    .fold(0, |acc, e| acc + e);
                data_stack.push(Slot::from_i64(val));
            }
            Call => {
                let addr = FnAddr::from(program.read_u32::<Endianness>().unwrap());
                let new_frame = fn_table.new_stack_frame(program.position(), addr);
                call_stack.push(new_frame);
                program.set_position(u32::from(addr) as u64);
            }
            Ret => {
                let old_frame = call_stack.pop().unwrap();
                program.set_position(old_frame.return_addr);
            }
            Exit => {
                return match data_stack.pop() {
                    Some(slot) => format!("{}", slot),
                    None => "".into(),
                };
            }

            MarkStack => {
                stack_marker = data_stack.len();
            }
            PopToMark => {
                data_stack.truncate(stack_marker);
            }
        }
    }
}

#[derive(Debug)]
struct FnTable {
    num_locals_table: Table<FnAddr, usize>,
    fn_addr_to_fn_def_id_table: Table<FnAddr, FnDefId>,
    fn_def_id_to_addr_table: Table<FnDefId, FnAddr>,
}

impl FnTable {
    fn new_stack_frame(&self, ret_addr: u64, fn_addr: FnAddr) -> StackFrame {
        let num_locals = self.num_locals_table[&fn_addr];

        StackFrame {
            locals: vec![Slot::uninitialized(); num_locals],
            return_addr: ret_addr,
        }
    }

    fn fn_def_id_at_addr(&self, fn_addr: FnAddr) -> Option<FnDefId> {
        self.fn_addr_to_fn_def_id_table.get(&fn_addr).cloned()
    }

    fn get_fn_addr(&self, fn_def_id: FnDefId) -> Option<FnAddr> {
        self.fn_def_id_to_addr_table.get(&fn_def_id).cloned()
    }
}

pub struct Program {
    instructions: Vec<u8>,
    fn_table: FnTable,
    const_table: Vec<Slot>,
    env: SymbolTable,
}


#[derive(Default)]
struct FnDef {
    // var_id -> evaluation
    bindings: Table<u32, Vec<MIns>>,
    code: Vec<MIns>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AssemblerOptions {
    CoalesceIndirectCalls,
}

pub struct ProgramBuilder {
    current_fn_def: Vec<FnDefId>,
    fn_defs: Table<FnDefId, FnDef>,
}

pub fn new_program() -> ProgramBuilder {
    let main_id = FnDefId::from(0);
    let mut fn_defs = Table::new();
    let code = vec![MIns::FnDefBegin(main_id)];

    fn_defs.insert(main_id,
                   FnDef {
                       bindings: Table::new(),
                       code: code,
                   });

    ProgramBuilder {
        current_fn_def: vec![main_id],
        fn_defs: fn_defs,
    }
}

impl ProgramBuilder {
    fn current_fn_def(&mut self) -> &mut FnDef {
        self.fn_defs
            .get_mut(self.current_fn_def
                         .last()
                         .expect("stack underflow: current_fn_def"))
            .expect("current_fn_def")
    }

    pub fn mark_stack(&mut self) {
        self.current_fn_def().code.push(MIns::MarkStack);
    }

    pub fn pop_to_mark(&mut self) {
        self.current_fn_def().code.push(MIns::PopToMark);
    }

    pub fn begin_fn_def(&mut self, fn_var_id: u32) {
        let fn_def_id = FnDefId::from(fn_var_id);
        let mut fn_def = FnDef::default();

        fn_def.code.push(MIns::FnDefBegin(fn_def_id));

        self.fn_defs.insert(fn_def_id, fn_def);
        self.current_fn_def.push(fn_def_id);
    }

    pub fn ret(&mut self) {
        self.current_fn_def().code.push(MIns::Ret);
    }

    pub fn end_fn_def(&mut self) {
        self.current_fn_def.pop();
    }

    // load a fn_ptr onto the stack
    pub fn fn_ptr(&mut self, fn_def_id: u32) {
        self.current_fn_def().code.push(MIns::FnPtr(FnDefId::from(fn_def_id)));
    }

    // pop a fn_ptr off the stack and call it
    pub fn call_ptr(&mut self) {
        self.current_fn_def().code.push(MIns::CallPtr);
    }

    pub fn make_locals(&mut self, param_ids: &[u32]) {
        let mut cfd = self.current_fn_def();
        let bindings = &mut cfd.bindings;

        // note: reversed (popping off of stack)
        for (idx, &id) in param_ids.iter().enumerate().rev() {
            cfd.code.push(MIns::SaveLocal(idx as u8));
            bindings.insert(id, vec![MIns::LoadLocal(idx as u8)]);
        }
    }

    // not entirely sure about this
    pub fn eval_binding(&mut self, var_id: u32) {
        let fn_def = self.current_fn_def();
        fn_def.code.append(&mut fn_def.bindings[&var_id].clone());
    }

    pub fn static_call(&mut self, fn_def_id: u32) {
        self.current_fn_def().code.push(MIns::CallFn(FnDefId::from(fn_def_id)));
    }

    pub fn i64_const(&mut self, val: i64) {
        self.current_fn_def().code.push(MIns::I64(val));
    }

    pub fn add(&mut self, num_args: usize) {
        self.current_fn_def().code.push(MIns::Add(num_args));
    }

    pub fn exit(&mut self) {
        self.current_fn_def().code.push(MIns::Exit);
    }
    pub fn finish(self, env: SymbolTable, config: &[AssemblerOptions]) -> Program {
        use std::iter::Iterator;


        // concatenated m_instructions from all function defs
        let whole_program_code =
            self.fn_defs
                .iter()
                .sorted_by(|&(a_id, _), &(b_id, _)| a_id.0.cmp(&b_id.0))
                .into_iter()
                .flat_map(|(_, fn_def)| {
                    if config.contains(&AssemblerOptions::CoalesceIndirectCalls) {
                        Box::new(fn_def.code.iter().cloned().coalesce(|a, b| {
                            match (a, b) {
                                (MIns::FnPtr(fn_def_id), MIns::CallPtr) => {
                                    Ok(MIns::CallFn(fn_def_id))
                                }
                                (a, b) => Err((a, b)),
                            }
                        })) as Box<Iterator<Item = MIns>>
                    } else {
                        Box::new(fn_def.code.iter().cloned()) as Box<Iterator<Item = MIns>>
                    }
                });

        // fn_def_id -> fn_addr
        let mut fn_begin_table = Table::new();

        // callsite addr -> fn_def_id
        let mut callsites_todo = Table::new();

        let mut instructions = vec![];
        let mut const_table = vec![];


        for m_instruction in whole_program_code {
            match m_instruction {
                MIns::FnDefBegin(fn_def_id) => {
                    fn_begin_table.insert(fn_def_id, FnAddr::from(instructions.len() as u32));
                }
                MIns::MarkStack => {
                    instructions.push(Ins::MarkStack.into());
                }
                MIns::PopToMark => {
                    instructions.push(Ins::PopToMark.into());
                }

                MIns::I64(val) => {
                    let idx = const_table.len() as u16;
                    const_table.push(Slot::from_i64(val));

                    instructions.push(Ins::LoadConst.into());
                    instructions.write_u16::<Endianness>(idx).unwrap();
                }

                MIns::FnPtr(fn_def_id) => {
                    instructions.push(Ins::FnPtr.into());
                    instructions.write_u32::<Endianness>(fn_def_id.0).unwrap();
                }
                MIns::CallPtr => {
                    instructions.push(Ins::CallPtr.into());
                }

                MIns::Add(num_args) => {
                    instructions.push(Ins::Add.into());
                    instructions.push(num_args as u8)
                }

                MIns::SaveLocal(idx) => {
                    instructions.push(Ins::Save.into());
                    instructions.push(idx);
                }
                MIns::LoadLocal(idx) => {
                    instructions.push(Ins::Load.into());
                    instructions.push(idx);
                }

                MIns::CallFn(fn_def_id) => {
                    instructions.push(Ins::Call.into());

                    let current_idx = instructions.len();
                    callsites_todo.insert(current_idx, fn_def_id);

                    // space for writing in addr
                    instructions.write_u32::<Endianness>(0).unwrap();
                }
                MIns::Ret => {
                    instructions.push(Ins::Ret.into());
                }

                MIns::Exit => {
                    instructions.push(Ins::Exit.into());
                }
            }
        }

        // fill in callsites
        for (callsite_addr, fn_def_id) in callsites_todo.into_iter() {
            let fn_addr = fn_begin_table[&fn_def_id];

            Endianness::write_u32(&mut instructions[callsite_addr..callsite_addr +
                    ::std::mem::size_of::<FnAddr>()],
                fn_addr.into());
        }

        let num_locals_table = self.fn_defs
                                   .iter()
                                   .map(|(fn_def_id, fn_def)| {
                                       let fn_addr = fn_begin_table[fn_def_id];
                                       let num_locals = fn_def.bindings.len();
                                       (fn_addr, num_locals)
                                   })
                                   .collect();

        let fn_def_id_to_addr_table = fn_begin_table;

        let fn_addr_to_fn_def_id_table = transposed(&fn_def_id_to_addr_table);

        let fn_table = FnTable {
            num_locals_table: num_locals_table,
            fn_addr_to_fn_def_id_table: fn_addr_to_fn_def_id_table,
            fn_def_id_to_addr_table: fn_def_id_to_addr_table,
        };

        Program {
            instructions: instructions,
            fn_table: fn_table,
            const_table: const_table,
            env: env,
        }
    }
}

fn transposed<K, V>(input: &Table<K, V>) -> Table<V, K>
    where K: ::std::hash::Hash + ::std::cmp::Eq + Clone,
          V: ::std::hash::Hash + ::std::cmp::Eq + Clone
{
    input.iter().map(|(k, v)| (v.clone(), k.clone())).collect()
}

// macro assembler instructions
#[derive(Clone, Debug)]
enum MIns {
    I64(i64),
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

impl Ins {
    fn mnemonic(&self) -> &str {
        match *self {
            Ins::Add => "add",
            Ins::Call => "call",
            Ins::CallPtr => "pcall",
            Ins::Dup => "dup",
            Ins::Exit => "exit",
            Ins::FnPtr => "fptr",
            Ins::Load => "load",
            Ins::LoadConst => "ldc",
            Ins::MarkStack => "mksk",
            Ins::Nop => "nop",
            Ins::Pop => "pop",
            Ins::PopToMark => "ptmk",
            Ins::Ret => "ret",
            Ins::Save => "save",
        }
    }
}

impl Debug for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use self::Ins::*;

        struct Line(usize, String, String); // line_num, instruction, args

        impl Display for Line {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                match *self {
                    Line(idx, ref ins, ref args) => write!(f, "{}: \t{:<6} {}", idx, ins, args),
                }
            }
        }

        enum LineType {
            Label(String, Line),
            Ins(Line),
        }

        fn fmt_label(self_: &Program, fn_def_id: FnDefId) -> String {
            self_.env.lookup_id(fn_def_id.into()).unwrap().ident().to_owned()
        }

        try!(writeln!(f, "Constant table:"));

        self.const_table
            .iter()
            .enumerate()
            .foreach(|(idx, val)| writeln!(f, "[{}] {:<}", idx, val.as_i64()).unwrap());

        try!(writeln!(f, ""));

        self.instructions
            .iter()
            .map(|&byte| Ins::from(byte))
            .enumerate()
            .batching(|mut it| {
                match it.next() {
                    None => None,
                    Some((idx, ins)) => {
                        let args_string = {
                            match ins {
                                Nop |
                                Ret |
                                Dup |
                                Pop |
                                Exit |
                                CallPtr |
                                MarkStack |
                                PopToMark => "".into(),
                                Add => {
                                    let num_args: u8 = it.next().unwrap().1.into();
                                    format!("{}", num_args)
                                }
                                Call => {
                                    let bytes = [it.next().unwrap().1.into(),
                                                 it.next().unwrap().1.into(),
                                                 it.next().unwrap().1.into(),
                                                 it.next().unwrap().1.into()];

                                    let fn_addr = FnAddr::from(Endianness::read_u32(&bytes));
                                    let fn_def_id = self.fn_table
                                                        .fn_def_id_at_addr(fn_addr)
                                                        .unwrap();
                                    fmt_label(self, fn_def_id)
                                }
                                Load => {
                                    let idx: u8 = it.next().unwrap().1.into();
                                    format!("%{}", idx)
                                }
                                Save => {
                                    let idx: u8 = it.next().unwrap().1.into();
                                    format!("%{}", idx)
                                }
                                LoadConst => {
                                    let bytes = [it.next().unwrap().1.into(),
                                                 it.next().unwrap().1.into()];

                                    let idx = Endianness::read_u16(&bytes);
                                    let val = self.const_table[idx as usize].as_i64();

                                    format!("{}", val)
                                }
                                FnPtr => {
                                    let bytes = [it.next().unwrap().1.into(),
                                                 it.next().unwrap().1.into(),
                                                 it.next().unwrap().1.into(),
                                                 it.next().unwrap().1.into()];

                                    let fn_def_id = FnDefId::from(Endianness::read_u32(&bytes));
                                    fmt_label(self, fn_def_id)
                                }
                            }
                        };

                        let line = Line(idx, ins.mnemonic().into(), args_string);
                        let main_id = FnDefId::from(0);

                        match self.fn_table.fn_def_id_at_addr(FnAddr::from(idx as u32)) {
                            Some(id) if id != main_id => {
                                Some(LineType::Label(fmt_label(self, id), line))
                            }
                            _ => Some(LineType::Ins(line)),
                        }
                    }
                }
            })
            .map(|line| {
                match line {
                    LineType::Label(label, line) => writeln!(f, "  {}:\n{}", label, line),
                    LineType::Ins(line) => writeln!(f, "{}", line),
                }

            })
            .fold_results((), |_, _| ())
    }
}

impl ::std::convert::From<u8> for Ins {
    fn from(byte: u8) -> Self {
        unsafe { ::std::mem::transmute(byte) }
    }
}

impl ::std::convert::From<Ins> for u8 {
    fn from(ins: Ins) -> Self {
        unsafe { ::std::mem::transmute(ins) }
    }
}

macro_rules! def_id {
    ($name:ident, $ty:ty) => {
        #[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
        struct $name($ty);

        impl ::std::convert::From<$ty> for $name {
            fn from(val: $ty) -> Self {
                $name(val)
            }
        }

        impl ::std::convert::From<$name> for $ty {
            fn from(val: $name) -> Self {
                val.0
            }
        }
    }
}

def_id!(FnAddr, u32);
def_id!(FnDefId, u32);
