
use itertools::*;
use byteorder::*;

use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};

pub type RegisterT = i64;


pub fn exec_program(program: Program) -> RegisterT {
    use self::Instruction::*;

    fn pop_usize(ds: &mut Vec<RegisterT>) -> usize {
        ds.pop().expect("stack underflow") as usize
    }

    let mut ip = 0; // instruction pointer
    let mut data_stack = Vec::with_capacity(128);
    // let ret_stack = Vec::with_capacity(128);

    loop {
        let i = ip;
        ip += 1;
        let instr = as_inst(program.instructions[i]);
        match instr {
            LoadConst => {
                let val = LittleEndian::read_i64(&program.instructions[ip..ip + 8]);
                ip += 8;
                data_stack.push(val);
            }
            Add => {
                let num_args = program.instructions[ip] as usize;
                ip += 1;
                assert!(data_stack.len() >= num_args, "stack underflow: add");
                let idx = data_stack.len() - num_args;
                let val = data_stack.drain(idx..).fold(0, |acc, e| acc + e);
                data_stack.push(val);
            }
            Drop => {
                data_stack.pop().expect("stack underflow: drop");
            }
            CallSpecial => unimplemented!(),
            CallNormal => unimplemented!(),
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
}

pub fn new_program() -> ProgramBuilder {
    ProgramBuilder {
        fn_cursor: None,
        fn_defs: Default::default(),
        instructions: vec![],
    }
}

pub struct ProgramBuilder {
    fn_cursor: Option<u32>,
    // var_id -> fn_def
    fn_defs: HashMap<u32, Vec<u8>>,
    instructions: Vec<u8>,
}

macro_rules! current_def {
    ($e:ident) => {
        match $e.fn_cursor {
            Some(id) => $e.fn_defs.get_mut(&id).unwrap(),
            _ => &mut $e.instructions,
        }
    }
}

impl ProgramBuilder {
    pub fn begin_fn_def(&mut self, id: u32) {
        self.fn_defs.insert(id, vec![]);
        self.fn_cursor = Some(id);
    }

    pub fn load_const(&mut self, val: RegisterT) {
        let def = current_def!(self);

        def.push(as_byte(Instruction::LoadConst));
        let mut tmp = [0; 8];
        LittleEndian::write_i64(&mut tmp, val);
        def.extend_from_slice(&tmp);
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
        Program { instructions: self.instructions }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum Instruction {
    LoadConst,
    Add,
    Drop,
    CallSpecial,
    CallNormal,
    Exit,
}

impl Debug for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use self::Instruction::*;

        let lines = self.instructions
                        .iter()
                        .enumerate()
                        .batching(|mut it| {
                            match it.next() {
                                None => None,
                                Some((idx, &b)) => {
                                    match as_inst(b) {
                                        LoadConst => {
                                            let mut raw = vec![];
                                            for _ in 0..8 {
                                                match it.next() {
                                                    Some((_, &b)) => {
                                                        raw.push(b);
                                                    }
                                                    _ => return None,
                                                }
                                            }
                                            let val = LittleEndian::read_i64(&*raw);
                                            Some(format!("{:>4}:\t ldc {:>3}", idx, val))
                                        }
                                        Add => {
                                            Some(format!("{:>4}:\t add {:>3}",
                                                         idx,
                                                         it.next().unwrap().1))
                                        }
                                        Exit => Some(format!("{:>4}:\texit", idx)),
                                        Drop => Some(format!("{:>4}:\tdrop", idx)),
                                        _ => unimplemented!(),
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
        // (+ 1 2 3) == 6

        let mut p = new_program();
        p.load_const(1);
        p.load_const(2);
        p.load_const(3);
        p.add(3);
        p.exit();

        let p = p.finish();

        assert_eq!(exec_program(p), 6);
    }
}
