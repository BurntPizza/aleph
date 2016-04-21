
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

struct Program {
    instructions: Vec<Instruction>,
}

struct StackFrame {
    ret_addr: usize,
}

#[derive(Copy, Clone)]
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
    use super::{Program, Vm};
    use super::Instruction::*;

    #[test]
    fn raw_program_addition() {
        // (+ 1 2 3) == 6

        let mut vm = Vm {
            program_counter: 0,
            registers: vec![],
            program: Program {
                instructions: vec![LoadConstImm(1),
                                   LoadConstImm(2),
                                   LoadConstImm(3),
                                   CallSpecial {
                                       builtin_idx: 0, // TODO
                                       begin_args_idx: 0,
                                       num_args: 3,
                                   },
                                   Exit(3)],
            },
        };

        assert_eq!(vm.run(), 6);
    }
}
