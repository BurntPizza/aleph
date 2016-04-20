
pub type RegisterT = i64;

pub struct Vm {
    registers: Vec<RegisterT>,
    program_counter: usize,
    program: Program,
}

impl Vm {
    pub fn run(&mut self) -> RegisterT {
        use self::Instruction::*;

        let mut stack = vec![StackFrame { ret_addr: self.program_counter }];

        loop {
            let instr = self.program.instructions[self.program_counter];

            match instr {
                LoadConst(reg_idx, const_idx) => {
                    self.ensure_idx(reg_idx as usize);
                    self.registers[reg_idx as usize] =
                        self.program.int_constants[const_idx as usize];
                }
                Move(src_idx, dst_idx) => {
                    self.registers[dst_idx as usize] = self.registers[src_idx as usize];
                }
                Call(ref fn_desc) => {
                    match *fn_desc {
                        FnDesc::Special(ref fn_desc) => {
                            match fn_desc.builtin_idx {
                                0 => {
                                    let begin = fn_desc.begin_args_idx as usize;
                                    let end = begin + fn_desc.num_args as usize;
                                    let ret_val = self.registers[begin..end]
                                                      .iter()
                                                      .fold(0, |acc, e| acc + e);
                                    let ret_reg = self.fresh_reg();
                                    self.registers[ret_reg] = ret_val;
                                }
                                _ => unimplemented!(),
                            }
                        }
                        FnDesc::Normal(_) => unimplemented!(),
                    }
                }
                Return(reg_idx) => return self.registers[reg_idx as usize],
            }
            self.program_counter += 1;
        }
    }

    fn ensure_idx(&mut self, idx: usize) {
        let curr_len = self.registers.len();
        let num = (idx + 1).saturating_sub(curr_len);
        self.registers.reserve(num);

        for _ in 0..num {
            self.registers.push(0);
        }
    }

    fn fresh_reg(&mut self) -> usize {
        self.registers.push(0);
        self.registers.len() - 1
    }
}

struct Program {
    int_constants: Vec<i64>,
    instructions: Vec<Instruction>,
}

struct StackFrame {
    ret_addr: usize,
}

#[derive(Copy, Clone)]
struct UserFnDesc {
    begin_args_idx: u16,
    num_args: u16,
    begin_inst_idx: u32,
}

#[derive(Copy, Clone)]
struct BuiltinFnDesc {
    begin_args_idx: u16,
    num_args: u16,
    builtin_idx: u32,
}

#[derive(Copy, Clone)]
enum FnDesc {
    Special(BuiltinFnDesc),
    Normal(UserFnDesc),
}

#[derive(Copy, Clone)]
enum Instruction {
    // reg_idx, const_idx
    LoadConst(u32, u32),
    //
    Call(FnDesc),
    // src_reg_idx, dst_reg_idx
    Move(u32, u32),
    // reg_idx
    Return(u32),
}

#[cfg(test)]
mod test {
    use super::{Program, Vm, FnDesc, BuiltinFnDesc};
    use super::Instruction::*;

    #[test]
    fn raw_program_addition() {
        // (+ 1 2 3) == 6

        let mut vm = Vm {
            program_counter: 0,
            registers: vec![],
            program: Program {
                int_constants: vec![1, 2, 3],
                instructions: vec![LoadConst(0, 0),
                                   LoadConst(1, 1),
                                   LoadConst(2, 2),
                                   Call(FnDesc::Special(BuiltinFnDesc {
                                       builtin_idx: 0, // TODO
                                       begin_args_idx: 0,
                                       num_args: 3,
                                   })),
                                   Return(3)],
            },
        };

        assert_eq!(vm.run(), 6);
    }
}
