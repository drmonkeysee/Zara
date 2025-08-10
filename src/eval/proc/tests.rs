use super::*;
use crate::{
    lex::TokenKind,
    number::Number,
    string::SymbolTable,
    testutil::{
        TestEnv, empty_procedure_body, err_or_fail, ok_or_fail, procedure_body, some_or_fail,
    },
};

#[test]
fn display_max_formals() {
    let e = InvalidFormal::MaxFormals;

    assert_eq!(
        e.to_string(),
        "lambda definition exceeds formal arguments limit: 256"
    );
}

#[test]
fn display_duplicate_formal() {
    let sym = SymbolTable::default();
    let e = InvalidFormal::DuplicateFormal(sym.get("x"));

    assert_eq!(e.to_string(), "duplicate formal: x");
}

#[test]
fn intrinsic_zero_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 0..0,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert_eq!(p.to_string(), "#<intrinsic foo>");
}

#[test]
fn intrinsic_single_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 1..1,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert_eq!(p.to_string(), "#<intrinsic foo (_)>");
}

#[test]
fn intrinsic_multi_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 3..3,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert_eq!(p.to_string(), "#<intrinsic foo (_ _ _)>");
}

#[test]
fn intrinsic_optional() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 0..1,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert_eq!(p.to_string(), "#<intrinsic foo (?)>");
}

#[test]
fn intrinsic_multi_optional() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 1..3,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert_eq!(p.to_string(), "#<intrinsic foo (_ ? ?)>");

    assert!(!p.matches_arity(0));
    assert!(p.matches_arity(1));
    assert!(p.matches_arity(2));
    assert!(p.matches_arity(3));
    assert!(!p.matches_arity(4));
}

#[test]
fn intrinsic_open_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 0..255,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert_eq!(p.to_string(), "#<intrinsic foo (…)>");
}

#[test]
fn intrinsic_required_params_with_open_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 2..255,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert_eq!(p.to_string(), "#<intrinsic foo (_ _ …)>");
}

#[test]
fn lambda_zero_arity_no_name() {
    let lm = ok_or_fail!(Lambda::new([], None, empty_procedure_body()));
    let p = Procedure::new(lm, Binding::default(), None);

    assert_eq!(p.to_string(), "#<procedure>");
}

#[test]
fn lambda_zero_arity() {
    let sym = SymbolTable::default();
    let lm = ok_or_fail!(Lambda::new([], None, empty_procedure_body()));
    let p = Procedure::new(lm, Binding::default(), Some(sym.get("bar")));

    assert_eq!(p.to_string(), "#<procedure bar>");
}

#[test]
fn lambda_single_arity() {
    let sym = SymbolTable::default();
    let lm = ok_or_fail!(Lambda::new([sym.get("x")], None, empty_procedure_body()));
    let p = Procedure::new(lm, Binding::default(), Some(sym.get("bar")));

    assert_eq!(p.to_string(), "#<procedure bar (x)>");
}

#[test]
fn lambda_multi_arity() {
    let sym = SymbolTable::default();
    let lm = ok_or_fail!(Lambda::new(
        [sym.get("x"), sym.get("y"), sym.get("z")],
        None,
        empty_procedure_body()
    ));
    let p = Procedure::new(lm, Binding::default(), Some(sym.get("bar")));

    assert_eq!(p.to_string(), "#<procedure bar (x y z)>");
}

#[test]
fn lambda_variadic_arity() {
    let sym = SymbolTable::default();
    let lm = ok_or_fail!(Lambda::new(
        [],
        Some(sym.get("any")),
        empty_procedure_body()
    ));
    let p = Procedure::new(lm, Binding::default(), Some(sym.get("bar")));

    assert_eq!(p.to_string(), "#<procedure bar any…>");
}

#[test]
fn lambda_rest_arity() {
    let sym = SymbolTable::default();
    let lm = ok_or_fail!(Lambda::new(
        [sym.get("x"), sym.get("y"), sym.get("z")],
        Some(sym.get("rest")),
        empty_procedure_body()
    ));
    let p = Procedure::new(lm, Binding::default(), Some(sym.get("bar")));

    assert_eq!(p.to_string(), "#<procedure bar (x y z rest…)>");
}

#[test]
fn matches_zero_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 0..0,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert!(p.matches_arity(0));
}

#[test]
fn matches_single_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 1..1,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert!(p.matches_arity(1));
}

#[test]
fn matches_max_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 255..255,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert!(p.matches_arity(MAX_ARITY as usize));
}

#[test]
fn matches_min_variable_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 0..3,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert!(p.matches_arity(0));
}

#[test]
fn matches_max_variable_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 0..3,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert!(p.matches_arity(3));
}

#[test]
fn matches_exceeds_variable_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 0..3,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert!(!p.matches_arity(4));
}

#[test]
fn matches_variable_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 2..5,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert!(!p.matches_arity(1));
    assert!(p.matches_arity(2));
    assert!(p.matches_arity(3));
    assert!(p.matches_arity(4));
    assert!(p.matches_arity(5));
    assert!(!p.matches_arity(6));
}

#[test]
fn exceeds_max_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 255..255,
        def: |_, _| Ok(Value::Unspecified),
        name: sym.get("foo"),
    };

    assert!(!p.matches_arity(256));
}

#[test]
fn lambda_matches_zero_arity() {
    let sym = SymbolTable::default();
    let lm = ok_or_fail!(Lambda::new([], None, empty_procedure_body()));
    let p = Procedure::new(lm, Binding::default(), Some(sym.get("bar")));

    assert!(p.matches_arity(0));
}

#[test]
fn lambda_matches_single_arity() {
    let sym = SymbolTable::default();
    let lm = ok_or_fail!(Lambda::new([sym.get("x")], None, empty_procedure_body()));
    let p = Procedure::new(lm, Binding::default(), Some(sym.get("bar")));

    assert!(p.matches_arity(1));
}

#[test]
fn lambda_matches_multi_arity() {
    let sym = SymbolTable::default();
    let lm = ok_or_fail!(Lambda::new(
        [sym.get("x"), sym.get("y"), sym.get("z")],
        None,
        empty_procedure_body()
    ));
    let p = Procedure::new(lm, Binding::default(), Some(sym.get("bar")));

    assert!(p.matches_arity(3));
    assert!(!p.matches_arity(2));
    assert!(!p.matches_arity(4));
}

#[test]
fn lambda_matches_variadic_arity() {
    let sym = SymbolTable::default();
    let lm = ok_or_fail!(Lambda::new(
        [],
        Some(sym.get("any")),
        empty_procedure_body()
    ));
    let p = Procedure::new(lm, Binding::default(), Some(sym.get("bar")));

    assert!(p.matches_arity(0));
    assert!(p.matches_arity(MAX_ARITY as usize));
    assert!(!p.matches_arity(256));
}

#[test]
fn lambda_matches_rest_arity() {
    let sym = SymbolTable::default();
    let lm = ok_or_fail!(Lambda::new(
        [sym.get("x"), sym.get("y")],
        Some(sym.get("any")),
        empty_procedure_body()
    ));
    let p = Procedure::new(lm, Binding::default(), Some(sym.get("bar")));

    assert!(!p.matches_arity(0));
    assert!(!p.matches_arity(1));
    assert!(p.matches_arity(2));
    assert!(p.matches_arity(MAX_ARITY as usize));
    assert!(!p.matches_arity(256));
}

#[test]
fn lambda_max_arity() {
    let sym = SymbolTable::default();
    let params = (0..MAX_ARITY)
        .into_iter()
        .map(|i| sym.get(format!("x{i}")))
        .collect::<Vec<_>>();

    let lm = ok_or_fail!(Lambda::new(params, None, empty_procedure_body()));
    let p = Procedure::new(lm, Binding::default(), Some(sym.get("bar")));

    assert!(p.matches_arity(MAX_ARITY as usize));
}

#[test]
fn lambda_max_arity_with_rest() {
    let sym = SymbolTable::default();
    let params = (0..MAX_ARITY - 1)
        .into_iter()
        .map(|i| sym.get(format!("x{i}")))
        .collect::<Vec<_>>();

    let lm = ok_or_fail!(Lambda::new(
        params,
        Some(sym.get("rest")),
        empty_procedure_body()
    ));
    let p = Procedure::new(lm, Binding::default(), Some(sym.get("bar")));

    assert!(p.matches_arity(MAX_ARITY as usize));
}

#[test]
fn lambda_too_many_params() {
    let sym = SymbolTable::default();
    let params = (0..MAX_ARITY as usize + 1)
        .into_iter()
        .map(|i| sym.get(format!("x{i}")))
        .collect::<Vec<_>>();

    let lm = Lambda::new(params, None, empty_procedure_body());

    let err = err_or_fail!(lm);

    assert_eq!(err.len(), 1);
    assert!(matches!(&err[0], InvalidFormal::MaxFormals));
}

#[test]
fn lambda_too_many_params_with_rest() {
    let sym = SymbolTable::default();
    let params = (0..MAX_ARITY)
        .into_iter()
        .map(|i| sym.get(format!("x{i}")))
        .collect::<Vec<_>>();

    let lm = Lambda::new(params, Some(sym.get("rest")), empty_procedure_body());

    let err = err_or_fail!(lm);

    assert_eq!(err.len(), 1);
    assert!(matches!(&err[0], InvalidFormal::MaxFormals));
}

#[test]
fn lambda_duplicate_params() {
    let sym = SymbolTable::default();
    let params = [
        sym.get("x"),
        sym.get("y"),
        sym.get("z"),
        sym.get("y"),
        sym.get("x"),
    ];

    let lm = Lambda::new(params, None, empty_procedure_body());

    let err = err_or_fail!(lm);

    assert_eq!(err.len(), 2);
    assert!(matches!(&err[0], InvalidFormal::DuplicateFormal(s) if s.as_ref() == "y"));
    assert!(matches!(&err[1], InvalidFormal::DuplicateFormal(s) if s.as_ref() == "x"));
}

#[test]
fn apply_zero_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 0..0,
        def: |_, _| Ok(Value::string("bar")),
        name: sym.get("foo"),
    };
    let env = TestEnv::default();
    let f = env.new_frame();
    let args = [];

    let r = p.apply(&args, &f);

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::String(s) if s.as_ref() == "bar"));
}

#[test]
fn apply_single_arity() {
    let sym = SymbolTable::default();
    let p = Intrinsic {
        arity: 1..1,
        def: |args, _| {
            let Value::String(s) = &args[0] else {
                unreachable!();
            };
            Ok(Value::string(format!("bar {s}")))
        },
        name: sym.get("foo"),
    };
    let env = TestEnv::default();
    let f = env.new_frame();
    let args = [Value::string("baz")];

    let r = p.apply(&args, &f);

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::String(s) if s.as_ref() == "bar baz"));
}

#[test]
fn apply_zero_arity_lambda() {
    let sym = SymbolTable::default();
    let env = TestEnv::default();
    let f = env.new_frame();
    let params = [];
    let lm = ok_or_fail!(Lambda::new(
        params,
        None,
        procedure_body([TokenKind::String("bar".to_owned())]),
    ));
    let p = Procedure::new(lm, Rc::clone(&f.scope), Some(sym.get("bar")));
    let args = [];

    let r = p.apply(&args, &f);

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::String(s) if s.as_ref() == "bar"));
}

#[test]
fn apply_single_arity_lambda() {
    let sym = SymbolTable::default();
    let env = TestEnv::default();
    let f = env.new_frame();
    let params = [sym.get("x")];
    let lm = ok_or_fail!(Lambda::new(
        params,
        None,
        procedure_body([TokenKind::Identifier("x".to_owned())]),
    ));
    let p = Procedure::new(lm, Rc::clone(&f.scope), Some(sym.get("bar")));
    let args = [Value::Number(Number::real(5))];

    let r = p.apply(&args, &f);

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::Number(_)));
    assert_eq!(v.to_string(), "5");
    assert!(!env.binding.bound("x"));
}

#[test]
fn apply_single_arity_lambda_with_closure() {
    let sym = SymbolTable::default();
    let env = TestEnv::default();
    let global_func = Intrinsic {
        arity: 1..1,
        def: |args, _| Ok(Value::string(format!("bar {}", args[0]))),
        name: sym.get("stringify"),
    };
    env.binding
        .bind(sym.get("stringify"), Value::Intrinsic(global_func.into()));
    let f = env.new_frame();
    let params = [sym.get("x")];
    let lm = ok_or_fail!(Lambda::new(
        params,
        None,
        procedure_body([
            TokenKind::ParenLeft,
            TokenKind::Identifier("stringify".to_owned()),
            TokenKind::Identifier("x".to_owned()),
            TokenKind::ParenRight
        ]),
    ));
    let p = Procedure::new(lm, Rc::clone(&f.scope), Some(sym.get("bar")));
    let args = [Value::Number(Number::real(5))];

    let r = p.apply(&args, &f);

    let v = ok_or_fail!(r);
    assert!(matches!(v, Value::String(s) if s.as_ref() == "bar 5"));
    assert!(!env.binding.bound("x"));
}

#[test]
fn lambda_set_name() {
    let sym = SymbolTable::default();
    let lm = ok_or_fail!(Lambda::new([], None, empty_procedure_body()));
    let mut p = Procedure::new(lm, Binding::default(), None);

    p.set_name(sym.get("bar"));

    assert_eq!(some_or_fail!(p.name()), "bar");
}

#[test]
fn lambda_does_not_set_name_if_already_named() {
    let sym = SymbolTable::default();
    let lm = ok_or_fail!(Lambda::new([], None, empty_procedure_body()));
    let mut p = Procedure::new(lm, Binding::default(), Some(sym.get("foo")));

    p.set_name(sym.get("bar"));

    assert_eq!(some_or_fail!(p.name()), "foo");
}
