
use itertools::*;
use byteorder::*;

use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};

use symbol_table::*;

type Endianness = LittleEndian;
type Table<K, V> = HashMap<K, V>;

macro_rules! read_u32 {
    ($instr:ident, $ip:ident) => {{
        let val = Endianness::read_u32(&$instr[$ip..$ip + 4]);
        $ip += 4;
        val
    }}
}

macro_rules! read_u16 {
    ($instr:ident, $ip:ident) => {{
        let val = Endianness::read_u16(&$instr[$ip..$ip + 2]);
        $ip += 2;
        val
    }}
}

macro_rules! read_u8 {
    ($instr:ident, $ip:ident) => {{
        let val = $instr[$ip];
        $ip += 1;
        val
    }}
}

macro_rules! read_i64 {
    ($instr:ident, $ip:ident) => {{
        let val = Endianness::read_i64(&$instr[$ip..$ip + 8]);
        $ip += 8;
        val
    }}
}

macro_rules! peek {
    ($stack:ident) => {
        $stack.last().unwrap()
    }
}

macro_rules! peek_mut {
    ($stack:ident) => {
        $stack.last_mut().unwrap()
    }
}

macro_rules! pop {
    ($stack:ident) => {
        $stack.pop().unwrap()
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum Ins {
    LoadConst,
    Load,
    Save,
    Pop,
    Dup,
    Add,
    Call,
    Ret,
    Exit,
}

#[derive(Copy, Clone, Default)]
struct Slot {
    val: u64,
}

impl Slot {
    fn as_i64(&self) -> i64 {
        self.val as i64
    }
}

struct FnPrototype {
    
}

type FnAddr = u32;

struct FnTable {}

impl FnTable {
    fn new_stack_frame(&self, ret_addr: usize, fn_addr: FnAddr) -> StackFrame {
        //
        unimplemented!()
    }

    // get info via prototype_idx
}

struct StackFrame {
    locals: Vec<Slot>,
    return_addr: u32,
    prototype_idx: u32
}

impl StackFrame {
    fn load_local(&self, idx: u8) -> Slot {
        self.locals[idx as usize]
    }

    fn save_local(&mut self, idx: u8, val: Slot) {
        self.locals[idx as usize] = val;
    }

    fn slots(&self) -> &[Slot] {
        &*self.locals
    }

    fn slots_mut(&mut self) -> &mut [Slot] {
        &mut *self.locals
    }
}

pub fn exec_program(program: Program) -> i64 {
    use self::Ins::*;

    let const_table = program.const_table;
    let fn_table = program.fn_table;
    let program = program.instructions;

    let mut call_stack = vec![fn_table.new_stack_frame(0, 0)]; // correct?
    let mut data_stack = Vec::with_capacity(128);
    let mut ip = 0;

    loop {
        let i = ip;
        ip += 1;

        let inst = program[i].into();

        match inst {
            LoadConst => {
                let const_idx = read_u16!(program, ip);
                let val = const_table[&const_idx];
                data_stack.push(val);
            }
            Load => {
                let idx = read_u8!(program, ip);
                let val = peek!(call_stack).load_local(idx);
                data_stack.push(val);
            }
            Save => {
                let idx = read_u8!(program, ip);
                let val = pop!(data_stack);
                peek_mut!(call_stack).save_local(idx, val);
            }
            Pop => { pop!(data_stack); }
            Dup => {
                let val = pop!(data_stack);
                data_stack.push(val);
                data_stack.push(val);
            }
            Add => {
                let num_args = read_u8!(program, ip) as usize;
                assert!(data_stack.len() >= num_args, "stack underflow: add");
                let idx = data_stack.len() - num_args;
                //let val = data_stack.drain(idx..).fold(0, |acc, e| acc + e);
                //data_stack.push(val);
                unimplemented!();
            }
            Call => {
                let addr = read_u32!(program, ip);
                let new_frame = fn_table.new_stack_frame(ip, addr);
                call_stack.push(new_frame);
                ip = addr as usize;
            }
            Ret => {
                let old_frame = pop!(call_stack);
                ip = old_frame.return_addr as usize;
            }
            Exit => return pop!(data_stack).as_i64(),
        }
    }
}



pub struct Program {
    instructions: Vec<u8>,
    fn_table: FnTable,
    const_table: Table<u16, Slot>,
}

pub fn new_program() -> ProgramBuilder {
    ProgramBuilder {

    }
}


// macro assembler instructions
#[derive(Debug)]
enum MIns {
    Add {
        num_args: usize,
    },
    LoadLocal,
    SaveLocal,
    Call, // TODO: what info?
    Ret,
    Exit,
}

pub struct ProgramBuilder {
    
}

impl ProgramBuilder {}

impl Debug for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use self::Ins::*;

        unimplemented!()
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

#[cfg(test)]
mod test {
    use super::*;
    use symbol_table::*;

}
