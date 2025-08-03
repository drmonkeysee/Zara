use zara::{Evaluation, Interpreter, RunMode, Value, src::StringSource};

struct TestRunner {
    input: StringSource,
    zara: Interpreter,
}

impl TestRunner {
    fn new() -> Self {
        Self {
            input: StringSource::empty("<integration>"),
            zara: Interpreter::new(RunMode::Evaluate, []),
        }
    }

    fn run_for_val(&mut self, src: impl Into<String>) -> Value {
        self.input.set(src);
        let r = self.zara.run(&mut self.input);

        assert!(r.is_ok());
        let ev = r.unwrap();
        assert!(matches!(ev, Evaluation::Val(_)));
        if let Evaluation::Val(v) = ev {
            return v;
        } else {
            unreachable!("{:?}", ev);
        }
    }
}

#[test]
fn basic_intrinsic() {
    let mut t = TestRunner::new();

    let v = t.run_for_val("(list-ref '(a b c) 1)");

    assert_eq!(v.to_string(), "b");
}

#[test]
fn global_scope() {
    let mut t = TestRunner::new();

    t.run_for_val(concat!(
        "(define x 1)",
        "(define five (lambda () (set! x 5)))"
    ));

    let v = t.run_for_val("x");
    assert_eq!(v.to_string(), "1");

    let v = t.run_for_val(concat!("(five)", "x"));
    assert_eq!(v.to_string(), "5");
}

#[test]
fn overwrite_self() {
    let mut t = TestRunner::new();

    let v = t.run_for_val(concat!("(define x 10)", "x"));
    assert_eq!(v.to_string(), "10");

    let v = t.run_for_val(concat!("(set! x (lambda () (set! x 20) x))", "x"));
    assert_eq!(v.to_string(), "#<procedure>");

    let v = t.run_for_val(concat!("(x)", "x"));
    assert_eq!(v.to_string(), "20");
}
