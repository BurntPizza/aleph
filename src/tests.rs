
use super::*;
use read::Sexp;

macro_rules! test_eq {
        ($name:ident; $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let input: &str = $input;
                let expected: Value = $expected;

                let module = ModuleBuilder::default().parse_mod(input);
                let mut interp = Interpreter::default();

                assert_eq!(interp.exec_module(&module), expected);
            }
        };
    }

#[test]
fn test_extern() {
    fn five_fn_ptr(args: &[Value]) -> Value {
        assert!(args.is_empty());
        Value::Int(5)
    }

    let five = Extern::Fn {
        ty: Type::Fun(vec![], Box::new(Type::Int)),
        ptr: ExternFnPtr(five_fn_ptr),
    };

    let input = "5";
    let expected = Value::Int(5);

    let mut builder = ModuleBuilder::default();
    builder.register_extern("five", five);

    let module = builder.parse_mod(input);
    let mut interp = Interpreter::default();

    assert_eq!(interp.exec_module(&module), expected);
}

test_eq!(test_higher_order; "(let (f (fn (x) (x 4)) \
                                       a (fn (b) b))    \
                                   (f a))", Value::Int(4));

test_eq!(test_closure; "(let (x 1
                                  a (fn (b) (+ x b)))
                              (a 1))", Value::Int(2));

test_eq!(test_module_def; "(def foo 5) foo", Value::Int(5));

#[test]
fn test_module_import() {
    let m1 = "(pub foo 5)";
    let m2 = "(use m1 *) foo";
    let expected = Value::Int(5);

    let m1 = ModuleBuilder::default().parse_mod(m1);
    let mut m2b = ModuleBuilder::default();
    m2b.link_module("m1", &m1);

    let m2 = m2b.parse_mod(m2);
    let mut interp = Interpreter::default();

    assert_eq!(interp.exec_module(&m2), expected);
}

#[test]
fn test_if() {
    fn print_fn_ptr(args: &[Value]) -> Value {
        assert!(args.is_empty());
        println!("Hello from 'print'");
        Value::Unit
    }

    let print = Extern::Fn {
        ty: Type::Fun(vec![], Box::new(Type::Unit)),
        ptr: ExternFnPtr(print_fn_ptr),
    };

    let mut builder = ModuleBuilder::default();
    builder.register_extern("print", print);

    let input = "(let (b true) (if b (print)))";
    let expected = Value::Unit;

    let module = builder.parse_mod(input);
    let mut interp = Interpreter::default();

    assert_eq!(interp.exec_module(&module), expected);
}

#[test]
fn test_quote() {
    let input = "(quote hello)";
    let expected = Value::Sexp(Sexp::Atom("hello".into()));

    let m = ModuleBuilder::default().parse_mod(input);
    let mut interp = Interpreter::default();
    assert_eq!(interp.exec_module(&m), expected);
}

#[test]
fn test_defmacro() {
    let input = "(defmacro do2 (&forms) '(let () forms)) (do2 1 2 3)";
    let expected = Value::Int(3);

    let m = ModuleBuilder::default().parse_mod(input);
    let mut interp = Interpreter::default();
    assert_eq!(interp.exec_module(&m), expected);
}
