
extern crate test;
use self::test::Bencher;

use super::*;

const PLUS: &str = "(+ 1 2 3 4 5 6 7 8 9 10)";
const PLUS_TREE: &str = "(+ 1 2 (+ 7 3 9 2) 4 (+ (+) 3
                             (+ 5 (+ 4 6 7) 7)) 6 7 8 (+ 3 3 3 (+ (+))) 10)";

fn add_fn_ptr(args: &[Value]) -> Value {
    Value::Int(args.into_iter().fold(0, |acc, e| match *e {
        Value::Int(v) => acc + v,
        _ => unreachable!(),
    }))
}

fn register_add(b: &mut ModuleBuilder) {
    let add = Extern::Fn {
        // TODO vararg-like args
        ty: Type::Fun(vec![Type::Int; 3], Box::new(Type::Int)),
        ptr: ExternFnPtr(add_fn_ptr),
    };

    b.register_extern("a", add);
}


#[bench]
fn bench_add_ast(b: &mut Bencher) {
    let input = PLUS;
    let m = ModuleBuilder::default().parse_mod(input);
    let mut interp = Interpreter::default();
    assert_eq!(interp.exec_module(&m), Value::Int(55));
    let exp = &m.top_level[0];

    b.iter(|| interp.eval(exp));
}

#[bench]
fn bench_add_extern(b: &mut Bencher) {
    let input = PLUS.replace("+", "a");
    let mut builder = ModuleBuilder::default();
    register_add(&mut builder);
    let m = builder.parse_mod(input);
    let mut interp = Interpreter::default();
    assert_eq!(interp.exec_module(&m), Value::Int(55));
    let exp = &m.top_level[0];

    b.iter(|| interp.eval(exp));
}

#[bench]
fn bench_add_ast_tree(b: &mut Bencher) {
    let input = PLUS_TREE;
    let m = ModuleBuilder::default().parse_mod(input);
    let mut interp = Interpreter::default();
    assert_eq!(interp.exec_module(&m), Value::Int(100));
    let exp = &m.top_level[0];

    b.iter(|| interp.eval(exp));
}

#[bench]
fn bench_add_extern_tree(b: &mut Bencher) {
    let input = PLUS_TREE.replace("+", "a");
    let mut builder = ModuleBuilder::default();
    register_add(&mut builder);
    let m = builder.parse_mod(input);
    let mut interp = Interpreter::default();
    assert_eq!(interp.exec_module(&m), Value::Int(100));
    let exp = &m.top_level[0];

    b.iter(|| interp.eval(exp));
}
