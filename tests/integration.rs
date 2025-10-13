use zara::{Error, Evaluation, Interpreter, Result, RunMode, Value, src::StringSource};

struct TestRunner {
    cont: bool,
    input: StringSource,
    zara: Interpreter,
}

impl TestRunner {
    fn new() -> Self {
        Self {
            cont: false,
            input: StringSource::empty("<integration>"),
            zara: Interpreter::new(RunMode::Evaluate, []),
        }
    }

    fn run_for_val(&mut self, src: impl Into<String>) -> Value {
        let ev = self.run_eval(src);
        if let Evaluation::Val(v) = ev {
            v
        } else {
            panic!("{ev:?}");
        }
    }

    fn run_for_cont(&mut self, src: impl Into<String>) {
        let ev = self.run_eval(src);
        self.cont = true;
        assert!(matches!(ev, Evaluation::Continuation));
    }

    fn run_for_err(&mut self, src: impl Into<String>) -> Error {
        let r = self.run(src);
        assert!(r.is_err(), "r is ok: {r:?}");
        r.unwrap_err()
    }

    fn run_eval(&mut self, src: impl Into<String>) -> Evaluation {
        let r = self.run(src);
        assert!(r.is_ok(), "r is err: {r:?}");
        r.unwrap()
    }

    fn run(&mut self, src: impl Into<String>) -> Result {
        if self.cont {
            self.input.cont(src);
        } else {
            self.input.set(src);
        }
        self.cont = false;
        self.zara.run(&mut self.input)
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

    t.run_for_val(concat!("(define x 1)", "(define (five) (set! x 5))"));

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
    assert_eq!(v.to_string(), "#<procedure x>");

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
    assert_eq!(v.to_string(), "#<procedure capture-five>");

    let v = t.run_for_val("(capture-five)");
    assert_eq!(v.to_string(), "5");
}

#[test]
fn closure_per_eval() {
    let mut t = TestRunner::new();

    t.run_for_val(concat!(
        "(define (capture x) (lambda () x))",
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
        "(define (capture s) (lambda () (set! s (string-append s s)) s))",
        "(define a (capture \"a\"))",
        "(define xy (capture \"xy\"))"
    ));

    let v = t.run_for_val(concat!("(a)", "(a)"));
    assert_eq!(v.to_string(), "\"aaaa\"");

    let v = t.run_for_val(concat!("(xy)", "(xy)"));
    assert_eq!(v.to_string(), "\"xyxyxyxy\"");

    let v = t.run_for_val("(a)");
    assert_eq!(v.to_string(), "\"aaaaaaaa\"");

    let v = t.run_for_val("(xy)");
    assert_eq!(v.to_string(), "\"xyxyxyxyxyxyxyxy\"");
}

#[test]
#[ignore = "nested define not yet implemented"]
fn call_frame_write_does_not_affect_closure() {
    let mut t = TestRunner::new();

    t.run_for_val(concat!("(define x 10)", "(define (foo) (define x 20) x)"));

    let v = t.run_for_val("x");
    assert_eq!(v.to_string(), "10");

    let v = t.run_for_val("(foo)");
    assert_eq!(v.to_string(), "20");

    let v = t.run_for_val("x");
    assert_eq!(v.to_string(), "10");
}

#[test]
#[ignore = "quote transformer not yet implemented"]
fn redefine_quote_affects_shorthand_syntax() {
    let mut t = TestRunner::new();

    t.run_for_val("(define 'a 10)");
    let v = t.run_for_val("(quote 1)");
    assert_eq!(v.to_string(), "10");

    let v = t.run_for_val("(quote 5)");
    assert_eq!(v.to_string(), "10");

    // TODO: ' as a macro to fix this? '<expr> -> (quote <expr>)
    let v = t.run_for_val("'3");
    assert_eq!(v.to_string(), "10");
}

#[test]
fn all_input_is_parsed_before_evaled() {
    let mut t = TestRunner::new();

    // TODO: whole sequence is parsed before evaled so quote is not replaced
    // until the next complete sequence; should it work this way?
    // this may have an impact on macros defined and used within the same module.
    let v = t.run_for_val(concat!("(define 'a 10)", "(quote 1)"));
    assert_eq!(v.to_string(), "1");

    let v = t.run_for_val("(quote 5)");
    assert_eq!(v.to_string(), "10");
}

#[test]
fn procedure_retains_name() {
    let mut t = TestRunner::new();

    let v = t.run_for_val(concat!("(define (foo x) x)", "foo"));
    assert_eq!(v.to_string(), "#<procedure foo (x)>");

    let v = t.run_for_val(concat!("(define bar foo)", "bar"));
    assert_eq!(v.to_string(), "#<procedure foo (x)>");

    let v = t.run_for_val(concat!("(define baz 10)", "baz"));
    assert_eq!(v.to_string(), "10");

    let v = t.run_for_val(concat!("(set! baz foo)", "baz"));
    assert_eq!(v.to_string(), "#<procedure foo (x)>");
}

#[test]
fn nested_procedure_gets_name() {
    let mut t = TestRunner::new();

    let v = t.run_for_val(concat!(
        "(define foo ((lambda (x) (lambda () x)) 5))",
        "foo"
    ));
    assert_eq!(v.to_string(), "#<procedure foo>");
    let v = t.run_for_val("(foo)");
    assert_eq!(v.to_string(), "5");

    t.run_for_val(concat!(
        "(define (capture x) (lambda () x))",
        "(define one (capture 1))",
        "(define ten (capture 10))"
    ));

    let v = t.run_for_val("one");
    assert_eq!(v.to_string(), "#<procedure one>");
    let v = t.run_for_val("(one)");
    assert_eq!(v.to_string(), "1");

    let v = t.run_for_val("ten");
    assert_eq!(v.to_string(), "#<procedure ten>");
    let v = t.run_for_val("(ten)");
    assert_eq!(v.to_string(), "10");
}

// TODO: guile/chez return #t; identical lambdas with no captures appear to be
// resolved to the same underlying object.
#[test]
fn nested_procedure_new_on_each_call() {
    let mut t = TestRunner::new();

    let v = t.run_for_val(concat!("(define (make) (lambda () 'a))", "make"));
    assert_eq!(v.to_string(), "#<procedure make>");

    let v = t.run_for_val(concat!("(define first (make))", "first"));
    assert_eq!(v.to_string(), "#<procedure first>");

    let v = t.run_for_val(concat!("(define second (make))", "second"));
    assert_eq!(v.to_string(), "#<procedure second>");

    let v = t.run_for_val("(eq? first second)");
    assert_eq!(v.to_string(), "#f");
}

// NOTE: these two tests were bugs with StringSource rather than the Interpreter
#[test]
fn lexical_error_on_continuation() {
    let mut t = TestRunner::new();

    t.run_for_cont("\"foo");
    t.run_for_cont("bar");
    let err = t.run_for_err("baz\" #\\sdf");

    assert_eq!(
        err.display_message().to_string(),
        "Lexical Error\n<integration>:3\n\tbaz\" #\\sdf\n\t     ^^^^^\n6: expected character literal\n"
    );
}

#[test]
fn syntax_error_on_continuation() {
    let mut t = TestRunner::new();

    t.run_for_cont("(if (< x y)");
    let err = t.run_for_err(")");

    assert_eq!(
        err.display_message().to_string(),
        "Syntax Error\n<integration>:1\n\t(if (< x y)\n\t^^^^^^^^^^^\n1: invalid form, expected: (if <test> <consequent> [alternate])\n"
    );
}

#[test]
fn several_layers_of_circular_lists() {
    let mut t = TestRunner::new();

    let v = t.run_for_val(concat!(
        "(define c (list 1 2 3))",
        "(set-cdr! (cddr c) c)",
        "(define d (list 4 5 6))",
        "(set-cdr! (cddr d) d)",
        "(define foo (list 'a 'b c c d 'z))",
        "(set-cdr! (cdr (cddddr foo)) foo)",
        "(define bar (list 'x 'y foo 'z))",
        "(set-cdr! (cdddr bar) bar)",
        "bar",
    ));

    assert_eq!(
        v.to_string(),
        "#0=(x y #1=(a b #2=(1 2 3 . #2#) #2# #3=(4 5 6 . #3#) z . #1#) z . #0#)"
    );
}

#[test]
fn very_large_vector() {
    let mut t = TestRunner::new();

    t.run_for_val("(define v (make-vector 1000000 'a))");

    let v = t.run_for_val("(vector-length v)");
    assert_eq!(v.to_string(), "1000000");

    let v = t.run_for_val(concat!(
        "(vector-set! v 999999 'b)",
        "(vector-ref v 999999)"
    ));
    assert_eq!(v.to_string(), "b");
}

#[test]
#[ignore = "stack overflow on list drop"]
fn very_large_list() {
    let mut t = TestRunner::new();

    t.run_for_val("(define l (make-list 1000000 'a))");

    let v = t.run_for_val("(length l)");
    assert_eq!(v.to_string(), "1000000");

    let v = t.run_for_val(concat!(
        "(set-car! (list-tail l 999999) 'b)",
        "(list-tail l 999995)"
    ));
    assert_eq!(v.to_string(), "(a a a a b)");
}
