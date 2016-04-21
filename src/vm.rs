
type RegisterT = i64;

struct Vm {
    registers: Vec<RegisterT>,
    program_counter: usize,
    program: Program,
}

impl Vm {
    fn run(&mut self) -> RegisterT {
        use self::Instruction::*;

        let mut stack = vec![StackFrame { ret_addr: self.program_counter }];

        loop {
            let instr = self.program.instructions[self.program_counter];

            match instr {
                LoadConstImm(val) => {
                    self.push_reg(val);
                }
                Move { src_idx, num } => {
                    for i in 0..num as usize {
                        let val = self.registers[src_idx as usize + i];
                        self.push_reg(val);
                    }
                }
                CallSpecial { builtin_idx, begin_args_idx, num_args } => {
                    match builtin_idx {
                        0 => {
                            let begin = begin_args_idx as usize;
                            let end = begin + num_args as usize;
                            let ret_val = self.registers[begin..end]
                                              .iter()
                                              .fold(0, |acc, e| acc + e);

                            self.push_reg(ret_val);
                        }
                        _ => unimplemented!(),
                    }
                }
                CallNormal { .. } => unimplemented!(),
                Exit(reg_idx) => return self.registers[reg_idx as usize],
            }
            self.program_counter += 1;
        }
    }

    fn push_reg(&mut self, val: RegisterT) {
        self.registers.push(val);
    }
}

#[derive(Debug, PartialEq)]
struct Program {
    instructions: Vec<Instruction>,
}

fn new_program() -> ProgramBuilder {
    ProgramBuilder {
        reg_counter: 0,
        instructions: vec![],
    }
}

struct ProgramBuilder {
    reg_counter: usize,
    instructions: Vec<Instruction>,
}

impl ProgramBuilder {
    fn load_const(mut self, val: RegisterT) -> Self {
        self.instructions.push(Instruction::LoadConstImm(val));
        self.reg_counter += 1;
        self
    }

    fn call_special(mut self, builtin_idx: u32, arg_idx: u16, num_args: u16) -> Self {
        self.instructions.push(Instruction::CallSpecial {
            builtin_idx: builtin_idx,
            begin_args_idx: arg_idx,
            num_args: num_args,
        });
        self.reg_counter += 1;
        self
    }

    fn pop_exit(mut self) -> Self {
        let reg_idx = self.reg_counter - 1;
        self.instructions.push(Instruction::Exit(reg_idx as u16));
        self
    }

    fn finish(self) -> Program {
        Program { instructions: self.instructions }
    }
}

struct StackFrame {
    ret_addr: usize,
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum Instruction {
    /// Load imm into a fresh register
    LoadConstImm(i64),
    /// Call builtin and put result into a fresh register
    CallSpecial {
        builtin_idx: u32,
        begin_args_idx: u16,
        num_args: u16,
    },
    /// Call fn and put result into a fresh register
    CallNormal {
        target_addr: u32,
        begin_args_idx: u16,
        num_args: u16,
    },
    /// Copy range of registers into range of fresh registers
    Move {
        src_idx: u16,
        num: u16,
    },
    /// Exit the program, returning value in indexed register
    Exit(u16),
}

#[cfg(test)]
mod test {
    use super::{Vm, new_program};

    #[test]
    fn raw_program_addition() {
        // (+ 1 2 3) == 6

        let mut vm = Vm {
            program_counter: 0,
            registers: vec![],
            program: new_program()
                         .load_const(1)
                         .load_const(2)
                         .load_const(3)
                         .call_special(0, 0, 3)
                         .pop_exit()
                         .finish(),
        };

        assert_eq!(vm.run(), 6);
    }
}
