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

        assert!(r.is_ok(), "r is err: {r:?}");
        let ev = r.unwrap();
        if let Evaluation::Val(v) = ev {
            v
        } else {
            panic!("{ev:?}");
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

#[test]
fn parameter_closure() {
    let mut t = TestRunner::new();

    let v = t.run_for_val(concat!(
        "(define capture-five ((lambda (x) (lambda () x)) 5))",
        "capture-five"
    ));
    assert_eq!(v.to_string(), "#<procedure>");

    let v = t.run_for_val("(capture-five)");
    assert_eq!(v.to_string(), "5");
}

#[test]
fn closure_per_eval() {
    let mut t = TestRunner::new();

    t.run_for_val(concat!(
        "(define capture (lambda (x) (lambda () x)))",
        "(define one (capture 1))",
        "(define ten (capture 10))"
    ));

    let v = t.run_for_val("(one)");
    assert_eq!(v.to_string(), "1");

    let v = t.run_for_val("(ten)");
    assert_eq!(v.to_string(), "10");

    let v = t.run_for_val("(one)");
    assert_eq!(v.to_string(), "1");

    let v = t.run_for_val("(ten)");
    assert_eq!(v.to_string(), "10");
}

#[test]
fn closure_writes_per_eval() {
    let mut t = TestRunner::new();

    t.run_for_val(concat!(
        "(define capture (lambda (x) (lambda () (set! x (+ x 1)) x)))",
        "(define one (capture 1))",
        "(define ten (capture 10))"
    ));

    let v = t.run_for_val(concat!("(one)", "(one)"));
    assert_eq!(v.to_string(), "3");

    let v = t.run_for_val(concat!("(ten)", "(ten)"));
    assert_eq!(v.to_string(), "12");

    let v = t.run_for_val("(one)");
    assert_eq!(v.to_string(), "4");

    let v = t.run_for_val("(ten)");
    assert_eq!(v.to_string(), "13");
}

#[test]
fn call_frame_write_does_not_affect_closure() {
    let mut t = TestRunner::new();

    t.run_for_val(concat!(
        "(define x 10)",
        "(define foo (lambda () (define x 20) x))"
    ));

    let v = t.run_for_val("x");
    assert_eq!(v.to_string(), "10");

    let v = t.run_for_val("(foo)");
    assert_eq!(v.to_string(), "20");

    let v = t.run_for_val("x");
    assert_eq!(v.to_string(), "10");
}

#[test]
fn redefine_quote_affects_shorthand_syntax() {
    let mut t = TestRunner::new();

    t.run_for_val("(define 'a 10)");
    let v = t.run_for_val("(quote 1)");
    assert_eq!(v.to_string(), "10");

    let v = t.run_for_val("(quote 5)");
    assert_eq!(v.to_string(), "10");

    let v = t.run_for_val("'3");
    assert_eq!(v.to_string(), "10");
}

#[test]
fn all_input_is_parsed_before_evaled() {
    let mut t = TestRunner::new();

    // TODO: whole sequence is parsed before evaled so quote is not replaced
    // until the next complete sequence; should it work this way?
    let v = t.run_for_val(concat!("(define 'a 10)", "(quote 1)"));
    assert_eq!(v.to_string(), "1");

    let v = t.run_for_val("(quote 5)");
    assert_eq!(v.to_string(), "10");
}
