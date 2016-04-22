
use itertools::*;
use byteorder::*;

use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};

use symbol_table::*;

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
            I64 => {
                let val = read_i64!(instructions, ip);
                data_stack.push(val);
            }
            U16 => {
                let val = read_u16!(instructions, ip);
                data_stack.push(val as RegisterT);
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
            Pop => {
                data_stack.pop().expect("stack underflow: pop");
            }
            Call => {
                let jmp_addr = data_stack.pop().expect("stack underflow: call") as usize;
                assert!(jmp_addr <= ::std::u16::MAX as usize);
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
    debug_info: DebugInfo,
}

pub fn new_program() -> ProgramBuilder {
    ProgramBuilder {
        in_prologue: false,
        fn_cursor: vec![],
        fn_defs: Default::default(),
        constants: Default::default(),
        prologue: vec![],
        instructions: vec![],
        var_defs: Default::default(),
    }
}


// preliminary instructions
// essentially assembler macros
#[derive(Debug)]
enum PIns {
    I64(i64),
    Load(u16),
    Store(u16),
    Add(u8),
    Pop,
    Call,
    Ret,
    Exit,
    // fn_id
    LoadFnAddrPlaceholder(u32),
    // fn_id
    FnHeaderPlaceholder(u32),
    PrologueEnd,
}

macro_rules! current_def {
    ($e:ident) => {
        if $e.in_prologue { 
            &mut $e.prologue
        } else {
            match $e.fn_cursor.len() {
            0 => &mut $e.instructions,
                n => $e.fn_defs.get_mut(&$e.fn_cursor[n - 1]).unwrap(),
            }
        }
    }
}


pub struct ProgramBuilder {
    in_prologue: bool,
    // stack of fn_ids
    fn_cursor: Vec<u32>,
    // fn_id -> fn_def
    fn_defs: HashMap<u32, Vec<PIns>>,
    constants: HashMap<u32, i64>,
    prologue: Vec<PIns>,
    instructions: Vec<PIns>,
    // var_ast_id -> var_slot_idx
    var_defs: HashMap<u32, u16>,
}

impl ProgramBuilder {
    pub fn in_prologue(&mut self) {
        self.in_prologue = true;
    }

    pub fn out_prologue(&mut self) {
        self.in_prologue = false;
    }

    pub fn begin_fn_def(&mut self, fn_id: u32) {
        self.fn_defs.insert(fn_id, vec![PIns::FnHeaderPlaceholder(fn_id)]);
        self.fn_cursor.push(fn_id);
    }

    pub fn end_fn_def(&mut self) {
        let fn_id = self.fn_cursor.pop().unwrap();
        self.prologue.push(PIns::LoadFnAddrPlaceholder(fn_id));
    }

    pub fn def_const(&mut self, var_ast_id: u32, val: i64) {
        self.constants.insert(var_ast_id, val);
    }

    pub fn call(&mut self) {
        current_def!(self).push(PIns::Call);
    }

    pub fn ret(&mut self) {
        current_def!(self).push(PIns::Ret);
    }

    pub fn def_var(&mut self, var_ast_id: u32) {
        let new_idx = self.var_defs.len();
        self.var_defs.insert(var_ast_id, new_idx as u16);
    }

    pub fn load_named_constant(&mut self, constant_ast_id: u32) {
        let val = self.constants[&constant_ast_id];
        self.load_i64(val);
    }

    pub fn load_var(&mut self, var_ast_id: u32) {
        let var_slot_idx = self.var_defs[&var_ast_id];
        assert!(self.var_defs.len() >= var_slot_idx as usize + 1);
        current_def!(self).push(PIns::Load(var_slot_idx));
    }

    pub fn store_var(&mut self, var_ast_id: u32) {
        let var_slot_idx = self.var_defs[&var_ast_id];
        assert!(self.var_defs.len() >= var_slot_idx as usize + 1);
        current_def!(self).push(PIns::Store(var_slot_idx));
    }

    pub fn load_i64(&mut self, val: RegisterT) {
        current_def!(self).push(PIns::I64(val));
    }

    pub fn add(&mut self, num_args: u8) {
        current_def!(self).push(PIns::Add(num_args));
    }

    pub fn pop(&mut self) {
        current_def!(self).push(PIns::Pop);
    }

    pub fn exit(&mut self) {
        current_def!(self).push(PIns::Exit);
    }

    pub fn finish(mut self, env: &SymbolTable) -> Program {
        use self::PIns::*;

        let fn_addr_load_placeholder = 255;
        let mut prologue_len = None;
        self.prologue.push(PrologueEnd);

        // fn_id -> call addr
        let mut fn_addrs: HashMap<u32, u16> = HashMap::new();

        // fn_id -> addr to write in fn_load
        let mut callsites_to_link = HashMap::new();
        let mut assembled = vec![];

        let itr = ::std::iter::once((::std::u32::MAX, self.prologue))
                      .chain(::std::iter::once((::std::u32::MAX, self.instructions)))
                      .chain(self.fn_defs.into_iter())
                      .flat_map(|(_, instructions)| instructions.into_iter());

        for pin in itr {
            match pin {
                I64(val) => {
                    assembled.push(as_byte(Instruction::I64));
                    assembled.write_i64::<Endianness>(val).unwrap();
                }
                Load(var_slot_idx) => {
                    assembled.push(as_byte(Instruction::Load));
                    assembled.write_u16::<Endianness>(var_slot_idx).unwrap();
                }
                Store(var_slot_idx) => {
                    assembled.push(as_byte(Instruction::Store));
                    assembled.write_u16::<Endianness>(var_slot_idx).unwrap();
                }
                Add(num_args) => {
                    assembled.push(as_byte(Instruction::Add));
                    assembled.push(num_args);
                }
                Pop => assembled.push(as_byte(Instruction::Pop)),
                Call => assembled.push(as_byte(Instruction::Call)),
                Ret => assembled.push(as_byte(Instruction::Ret)),
                Exit => assembled.push(as_byte(Instruction::Exit)),
                LoadFnAddrPlaceholder(fn_id) => {
                    let current_position = assembled.len();
                    callsites_to_link.insert(fn_id, current_position);
                    assembled.push(fn_addr_load_placeholder);
                    assembled.write_u16::<Endianness>(0).unwrap();
                }
                FnHeaderPlaceholder(fn_id) => {
                    fn_addrs.insert(fn_id, assembled.len() as u16);
                }
                PrologueEnd => {
                    prologue_len = Some(assembled.len() as u16);
                }
            }
        }

        for (fn_id, idx) in callsites_to_link {
            let fn_addr = fn_addrs[&fn_id];
            assembled[idx] = as_byte(Instruction::U16);
            Endianness::write_u16(&mut assembled[idx + 1..idx + 3], fn_addr);
        }

        // debug info

        let var_names = self.var_defs
                            .iter()
                            .map(|(&var_id, &var_addr)| {
                                (var_addr,
                                 env.lookup_id(var_id)
                                    .map_or_else(|| format!("<var {}>", var_id),
                                                 |r| r.ident().into()))
                            })
                            .collect();

        let labels = fn_addrs.iter()
                             .map(|(_, &fn_addr)| (fn_addr, format!("L{}", fn_addr)))
                             .chain(::std::iter::once((prologue_len.unwrap(), format!("main"))))
                             .collect();

        Program {
            instructions: assembled,
            num_vars: self.var_defs.len(),
            debug_info: DebugInfo {
                var_names: var_names,
                labels: labels,
            },
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum Instruction {
    I64,
    U16,
    Load,
    Store,
    Add,
    Pop,
    Call,
    Ret,
    Exit,
}


#[derive(Default)]
struct DebugInfo {
    // fn_addr -> name
    labels: HashMap<u16, String>,
    // var_slot_idx -> name
    var_names: HashMap<u16, String>,
}

impl Debug for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use self::Instruction::*;

        fn format_inst(label: Option<&str>,
                       idx: usize,
                       i: Instruction,
                       args: &ToString)
                       -> Option<String> {
            let mnemonic = match i {
                I64 => "i64",
                U16 => "u16",
                Load => "load",
                Store => "stor",
                Call => "call",
                Add => "add",
                Pop => "pop",
                Exit => "exit",
                Ret => "ret",
            };

            let label = match label {
                Some(s) => format!("{}:\n", s),
                None => "".into(),
            };

            Some(format!("{}{:>6}:\t{:>4} {}", label, idx, mnemonic, args.to_string()))
        }

        let lines = self.instructions
                        .iter()
                        .enumerate()
                        .batching(|mut it| {
                            match it.next() {
                                None => None,
                                Some((idx, &b)) => {
                                    let label = match self.debug_info
                                                          .labels
                                                          .get(&(idx as u16)) {
                                        Some(name) => Some(&**name),
                                        _ => None,
                                    };

                                    let inst = as_inst(b);

                                    match inst {
                                        I64 => {
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
                                            format_inst(label, idx, inst, &val)
                                        }
                                        U16 => {
                                            let mut raw = vec![];
                                            for _ in 0..2 {
                                                match it.next() {
                                                    Some((_, &b)) => {
                                                        raw.push(b);
                                                    }
                                                    _ => return None,
                                                }
                                            }
                                            let val = Endianness::read_u16(&*raw);
                                            format_inst(label, idx, inst, &val)
                                        }
                                        Load => {
                                            let bytes = [*it.next().unwrap().1,
                                                         *it.next().unwrap().1];
                                            let var_slot_idx = Endianness::read_u16(&bytes[..]);
                                            let name = &self.debug_info.var_names[&var_slot_idx];
                                            format_inst(label, idx, inst, name)
                                        }
                                        Store => {
                                            let bytes = [*it.next().unwrap().1,
                                                         *it.next().unwrap().1];
                                            let var_slot_idx = Endianness::read_u16(&bytes[..]);
                                            let name = &self.debug_info.var_names[&var_slot_idx];
                                            format_inst(label, idx, inst, name)
                                        }
                                        Call => format_inst(label, idx, inst, &""),
                                        Add => format_inst(label, idx, inst, it.next().unwrap().1),
                                        Ret => format_inst(label, idx, inst, &""),
                                        Exit => format_inst(label, idx, inst, &""),
                                        Pop => format_inst(label, idx, inst, &""),
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
    use symbol_table::*;

    #[test]
    fn raw_program_addition() {
        // (+ 1 2 3)

        let mut p = new_program();

        p.load_i64(1);
        p.load_i64(2);
        p.load_i64(3);
        p.add(3);
        p.exit();

        let p = p.finish(&SymbolTable::empty());
        println!("{:?}", p);
        assert_eq!(exec_program(p), 6);
    }

    #[test]
    fn var() {
        // (def x 7)
        // (+ x 3)

        let mut p = new_program();

        let x = 0; // 'ast' id, not slot number or value
        p.def_var(x);
        p.load_i64(7);
        p.store_var(x);
        p.load_i64(3);
        p.load_var(x);
        p.add(2);
        p.exit();

        let p = p.finish(&SymbolTable::empty());
        println!("{:?}", p);
        assert_eq!(exec_program(p), 10);
    }

    // TODO: static calls
    // #[test]
    // fn calls() {
    //     // (defn f [x]
    //     //   (+ 2 x))
    //     // (defn g [x]
    //     //   (f x))
    //     // (g (f 4))

    // let mut p = new_program();

    //     p.load_i64(4);
    //     p.call(16);
    //     p.call(28);
    //     p.exit();
    //     p.load_i64(2); // addr 16
    //     p.add(2);
    //     p.ret();
    //     p.call(16); // addr 28
    //     p.ret();

    //     let p = p.finish();
    //     println!("{:?}", p);
    //     assert_eq!(exec_program(p), 8);
    // }
}
