
use itertools::*;
use byteorder::*;

use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};

pub type RegisterT = i64;
type Endianness = LittleEndian;

pub fn exec_program(program: Program) -> RegisterT {
    use self::Instruction::*;

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

    let mut ip = 0; // instruction pointer
    let mut data_stack = Vec::with_capacity(128);
    let mut ret_stack = Vec::with_capacity(128);
    let mut var_slots = vec![0; program.num_vars];
    let instructions = program.instructions;

    loop {
        let i = ip;
        ip += 1;
        let instr = as_inst(instructions[i]);
        match instr {
            ConstI64 => {
                let val = read_i64!(instructions, ip);
                data_stack.push(val);
            }
            Load => {
                let var_slot = read_u16!(instructions, ip) as usize;
                data_stack.push(var_slots[var_slot]);
            }
            Store => {
                let var_slot = read_u16!(instructions, ip) as usize;
                var_slots[var_slot] = data_stack.pop().expect("stack underflow: store");
            }
            Add => {
                let num_args = read_u8!(instructions, ip) as usize;
                assert!(data_stack.len() >= num_args, "stack underflow: add");
                let idx = data_stack.len() - num_args;
                let val = data_stack.drain(idx..).fold(0, |acc, e| acc + e);
                data_stack.push(val);
            }
            Drop => {
                data_stack.pop().expect("stack underflow: drop");
            }
            Call => {
                let jmp_addr = read_u16!(instructions, ip) as usize;
                ret_stack.push(ip);
                ip = jmp_addr;
            }
            Ret => {
                ip = ret_stack.pop().expect("return stack underflow: ret");
            }
            Exit => return data_stack.pop().expect("stack underflow: exit"),
        }
    }
}

fn as_byte(i: Instruction) -> u8 {
    unsafe { ::std::mem::transmute(i) }
}

fn as_inst(b: u8) -> Instruction {
    unsafe { ::std::mem::transmute(b) }
}

pub struct Program {
    instructions: Vec<u8>,
    num_vars: usize,
}

pub fn new_program() -> ProgramBuilder {
    ProgramBuilder {
        fn_cursor: vec![],
        fn_defs: Default::default(),
        instructions: vec![],
        num_vars: 0,
    }
}

pub struct ProgramBuilder {
    // stack of ids
    fn_cursor: Vec<u32>,
    // var_id -> fn_def
    fn_defs: HashMap<u32, Vec<u8>>,
    instructions: Vec<u8>,
    num_vars: u16,
}

macro_rules! current_def {
    ($e:ident) => {
        match $e.fn_cursor.len() {
            0 => &mut $e.instructions,
            n => $e.fn_defs.get_mut(&$e.fn_cursor[n - 1]).unwrap(),
        }
    }
}

impl ProgramBuilder {
    pub fn begin_fn_def(&mut self, id: u32) {
        self.fn_defs.insert(id, vec![]);
        self.fn_cursor.push(id);
    }

    pub fn end_fn_def(&mut self) {
        self.fn_cursor.pop();
    }

    pub fn def_var(&mut self) -> u16 {
        let tmp = self.num_vars;
        self.num_vars += 1;
        tmp
    }

    pub fn load_var(&mut self, var_slot_idx: u16) {
        assert!(self.num_vars >= var_slot_idx + 1);
        let def = current_def!(self);
        def.push(as_byte(Instruction::Load));
        def.write_u16::<Endianness>(var_slot_idx).unwrap();
    }

    pub fn store_var(&mut self, var_slot_idx: u16) {
        assert!(self.num_vars >= var_slot_idx + 1);
        let def = current_def!(self);
        def.push(as_byte(Instruction::Store));
        def.write_u16::<Endianness>(var_slot_idx).unwrap();
    }

    pub fn load_const(&mut self, val: RegisterT) {
        let def = current_def!(self);
        def.push(as_byte(Instruction::ConstI64));
        def.write_i64::<Endianness>(val).unwrap();
    }

    pub fn add(&mut self, num_args: u8) {
        let def = current_def!(self);
        def.push(as_byte(Instruction::Add));
        def.push(num_args);
    }

    pub fn drop(&mut self) {
        let def = current_def!(self);
        def.push(as_byte(Instruction::Drop));
    }

    pub fn exit(&mut self) {
        let def = current_def!(self);
        def.push(as_byte(Instruction::Exit));
    }

    pub fn finish(self) -> Program {
        // assemble fn_defs
        Program {
            instructions: self.instructions,
            num_vars: self.num_vars as usize,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum Instruction {
    ConstI64,
    Load,
    Store,
    Add,
    Drop,
    Call,
    Ret,
    Exit,
}



impl Debug for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use self::Instruction::*;

        fn format_inst(idx: usize, i: Instruction, argn: &ToString) -> Option<String> {
            let mnemonic = match i {
                ConstI64 => "i64",
                Load => "load",
                Store => "stor",
                Call => "call",
                Add => "add",
                Drop => "drop",
                Exit => "exit",
                Ret => "ret",
            };
            Some(format!("{:>4}:\t{:>4} {:>3}", idx, mnemonic, argn.to_string()))
        }

        let lines = self.instructions
                        .iter()
                        .enumerate()
                        .batching(|mut it| {
                            match it.next() {
                                None => None,
                                Some((idx, &b)) => {
                                    let inst = as_inst(b);
                                    match inst {
                                        ConstI64 => {
                                            let mut raw = vec![];
                                            for _ in 0..8 {
                                                match it.next() {
                                                    Some((_, &b)) => {
                                                        raw.push(b);
                                                    }
                                                    _ => return None,
                                                }
                                            }
                                            let val = Endianness::read_i64(&*raw);
                                            format_inst(idx, inst, &val)
                                        }
                                        Load => {
                                            let bytes = [*it.next().unwrap().1,
                                                         *it.next().unwrap().1];
                                            let val = Endianness::read_u16(&bytes[..]);
                                            format_inst(idx, inst, &val)
                                        }
                                        Store => {
                                            let bytes = [*it.next().unwrap().1,
                                                         *it.next().unwrap().1];
                                            let val = Endianness::read_u16(&bytes[..]);
                                            format_inst(idx, inst, &val)
                                        }
                                        Call => {
                                            let bytes = [*it.next().unwrap().1,
                                                         *it.next().unwrap().1];
                                            let val = Endianness::read_u16(&bytes[..]);
                                            format_inst(idx, inst, &val)
                                        }
                                        Add => format_inst(idx, inst, it.next().unwrap().1),
                                        Ret => format_inst(idx, inst, &""),
                                        Exit => format_inst(idx, inst, &""),
                                        Drop => format_inst(idx, inst, &""),
                                    }
                                }
                            }
                        })
                        .join("\n");

        writeln!(f, "\n{}", lines)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn raw_program_addition() {
        // (+ 1 2 3)

        let mut p = new_program();

        p.load_const(1);
        p.load_const(2);
        p.load_const(3);
        p.add(3);
        p.exit();

        let p = p.finish();

        assert_eq!(exec_program(p), 6);
    }

    #[test]
    fn var() {
        // (def x 7)
        // (+ x 3)

        let mut p = new_program();

        let x = p.def_var();
        p.load_const(7);
        p.store_var(x);
        p.load_const(3);
        p.load_var(x);
        p.add(2);
        p.exit();

        let p = p.finish();

        assert_eq!(exec_program(p), 10);
    }
}
