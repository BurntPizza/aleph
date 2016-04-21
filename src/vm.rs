
use byteorder::*;

type RegisterT = i64;


fn exec_program(program: Program) -> RegisterT {
    use self::Instruction::*;

    let mut ip = 0; // instruction pointer
    let mut data_stack = Vec::with_capacity(128);
    // let ret_stack = Vec::with_capacity(128);

    loop {
        let i = ip;
        ip += 1;
        match as_inst(program.instructions[i]) {
            LoadConst => {
                let val = LittleEndian::read_i64(&program.instructions[ip..ip + 8]);
                ip += 8;
                data_stack.push(val);
            }
            Add => {
                let num_args = data_stack.pop().expect("stack underflow: add") as usize;
                assert!(data_stack.len() >= num_args, "stack underflow: add");
                let idx = data_stack.len() - num_args;
                let val = data_stack.drain(idx..).fold(0, |acc, e| acc + e);
                data_stack.push(val);
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

#[derive(Debug, PartialEq)]
struct Program {
    instructions: Vec<u8>,
}

fn new_program() -> ProgramBuilder {
    ProgramBuilder { instructions: vec![] }
}

struct ProgramBuilder {
    instructions: Vec<u8>,
}

impl ProgramBuilder {
    fn load_const(mut self, val: RegisterT) -> Self {
        self.instructions.push(as_byte(Instruction::LoadConst));
        let mut tmp = [0; 8];
        LittleEndian::write_i64(&mut tmp, val);
        self.instructions.extend_from_slice(&tmp);
        self
    }

    fn add(mut self, num_args: u8) -> Self {
        self = self.load_const(num_args as i64);
        self.instructions.push(as_byte(Instruction::Add));
        self
    }

    fn exit(mut self) -> Self {
        self.instructions.push(as_byte(Instruction::Exit));
        self
    }

    fn finish(self) -> Program {
        Program { instructions: self.instructions }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum Instruction {
    LoadConst,
    Add,
    CallSpecial,
    CallNormal,
    Exit,
}

#[cfg(test)]
mod test {

    use super::{new_program, exec_program};

    #[test]
    fn raw_program_addition() {
        // (+ 1 2 3) == 6

        let p = new_program()
                    .load_const(1)
                    .load_const(2)
                    .load_const(3)
                    .add(3)
                    .exit()
                    .finish();

        assert_eq!(exec_program(p), 6);
    }
}
