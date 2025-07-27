use super::{Binding, EvalResult, Frame};
use crate::{string::Symbol, syntax::Sequence, value::Value};
use std::{
    collections::HashSet,
    fmt::{self, Display, Formatter, Write},
    iter,
    ops::Range,
    rc::Rc,
};

pub(crate) const MAX_ARITY: u8 = u8::MAX;

pub(crate) type IntrinsicFn = fn(&[Value], &mut Frame) -> EvalResult;
pub(crate) type Arity = Range<u8>;
pub(crate) type LambdaResult = Result<Procedure, Vec<InvalidFormal>>;

#[derive(Debug)]
pub(crate) struct Procedure {
    arity: Arity,
    def: Definition,
    name: Option<Symbol>,
}

impl Procedure {
    pub(crate) fn intrinsic(name: Symbol, arity: Arity, def: IntrinsicFn) -> Self {
        Self {
            arity,
            def: Definition::Intrinsic(def),
            name: Some(name),
        }
    }

    pub(crate) fn lambda(
        named_args: impl IntoIterator<Item = Symbol>,
        variadic_arg: Option<Symbol>,
        body: Sequence,
        closure: impl Into<Rc<Binding>>,
        name: Option<Symbol>,
    ) -> LambdaResult {
        let formals = into_formals(named_args)?;
        let min = formals.len();
        let (max, param_count) = if variadic_arg.is_some() {
            (MAX_ARITY as usize, min + 1)
        } else {
            (min, min)
        };
        if param_count > MAX_ARITY.into() {
            Err(vec![InvalidFormal::MaxFormals])
        } else {
            Ok(Self {
                #[allow(clippy::cast_possible_truncation, reason = "truncation guarded above")]
                arity: min as u8..max as u8,
                def: Definition::Lambda {
                    body,
                    closure: closure.into(),
                    formals: formals.into(),
                    variadic: variadic_arg,
                },
                name,
            })
        }
    }

    pub(crate) fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    pub(crate) fn arity(&self) -> &Arity {
        &self.arity
    }

    pub(crate) fn matches_arity(&self, args_len: usize) -> bool {
        self.arity.start as usize <= args_len && args_len <= self.arity.end as usize
    }

    pub(crate) fn apply(&self, args: &[Value], env: &mut Frame) -> EvalResult {
        self.def.apply(args, env)
    }

    fn write_arity(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.arity.start == 0 && self.arity.is_empty() {
            Ok(())
        } else {
            self.def.write_arity(&self.arity, f)
        }
    }
}

impl Display for Procedure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("#<procedure")?;
        if let Some(n) = &self.name {
            write!(f, " {}", n.as_datum())?;
        }
        self.write_arity(f)?;
        f.write_char('>')
    }
}

#[derive(Debug)]
pub(crate) enum InvalidFormal {
    DuplicateFormal(Symbol),
    MaxFormals,
}

impl Display for InvalidFormal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::DuplicateFormal(n) => write!(f, "duplicate formal: {}", n.as_datum()),
            Self::MaxFormals => write!(
                f,
                "lambda definition exceeds formal arguments limit: {}",
                MAX_ARITY as usize + 1
            ),
        }
    }
}

#[derive(Debug)]
enum Definition {
    Intrinsic(IntrinsicFn),
    Lambda {
        body: Sequence,
        // TODO: make this optional for pure functions
        closure: Rc<Binding>,
        formals: Box<[Symbol]>,
        variadic: Option<Symbol>,
    },
}

impl Definition {
    fn apply(&self, args: &[Value], env: &mut Frame) -> EvalResult {
        match self {
            Self::Intrinsic(func) => func(args, env),
            Self::Lambda {
                body,
                closure,
                formals,
                variadic,
            } => call_lambda(args, env, body, closure, formals, variadic.as_ref()),
        }
    }

    fn write_arity(&self, arity: &Arity, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Intrinsic(_) => write_intrinsics(arity, f),
            Self::Lambda {
                formals, variadic, ..
            } => write_formals(formals, variadic.as_ref(), f),
        }
    }
}

fn write_formals(args: &[Symbol], variadic: Option<&Symbol>, f: &mut Formatter<'_>) -> fmt::Result {
    f.write_char(' ')?;
    let mut buf = args.join(" ");
    if let Some(v) = variadic {
        if !buf.is_empty() {
            buf.push(' ');
        }
        buf.push_str(v);
        buf.push('…');
        if args.is_empty() {
            return f.write_str(&buf);
        }
    }
    write!(f, "({buf})")
}

fn write_intrinsics(arity: &Arity, f: &mut Formatter<'_>) -> fmt::Result {
    let params = iter::repeat_n("_", arity.start.into())
        .chain(if arity.end == MAX_ARITY {
            iter::repeat_n("…", 1)
        } else {
            iter::repeat_n("?", arity.len())
        })
        .collect::<Vec<_>>()
        .join(" ");
    write!(f, " ({params})")
}

fn into_formals(
    named_args: impl IntoIterator<Item = Symbol>,
) -> Result<Vec<Symbol>, Vec<InvalidFormal>> {
    let mut names = HashSet::new();
    let (singles, duplicates): (Vec<_>, Vec<_>) = named_args
        .into_iter()
        .map(|n| {
            if names.contains(&n) {
                Err(InvalidFormal::DuplicateFormal(n))
            } else {
                names.insert(n.clone());
                Ok(n)
            }
        })
        .partition(Result::is_ok);
    if duplicates.is_empty() {
        Ok(singles.into_iter().flatten().collect::<Vec<_>>())
    } else {
        Err(duplicates.into_iter().filter_map(Result::err).collect())
    }
}

fn call_lambda(
    args: &[Value],
    env: &mut Frame,
    body: &Sequence,
    closure: &Rc<Binding>,
    formals: &[Symbol],
    variadic: Option<&Symbol>,
) -> EvalResult {
    // TODO: tail-call optimization does not create a new frame
    // TODO: this doesn't seem quite right, particularly for recursion
    // is env.scope actually irrelevant with lexical scoping?
    let mut call_frame = Frame {
        scope: Binding::new(Rc::clone(closure)).into(),
        sym: env.sym,
        sys: env.sys,
    };
    let args_it = args.iter();
    formals
        .iter()
        .zip(args_it.take(formals.len()))
        .for_each(|(var, val)| {
            call_frame.scope.bind(var.clone(), val.clone());
        });
    if variadic.is_some() {
        todo!("support variadic args");
    }
    body.eval(&mut call_frame)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lex::TokenKind,
        number::Number,
        string::SymbolTable,
        syntax::{ExpressionTree, Parser, ParserOutput},
        testutil::{TestEnv, err_or_fail, extract_or_fail, make_tokenline, ok_or_fail},
    };

    fn body(tokens: impl IntoIterator<Item = TokenKind>) -> Sequence {
        let mut et = ExpressionTree::default();
        let mut env = TestEnv::default();

        let r = et.parse([make_tokenline(tokens)].into(), env.new_namespace());

        extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete)
    }

    fn empty_body() -> Sequence {
        body([])
    }

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
        let mut sym = SymbolTable::default();
        let e = InvalidFormal::DuplicateFormal(sym.get("x"));

        assert_eq!(e.to_string(), "duplicate formal: x");
    }

    #[test]
    fn intrinsic_zero_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 0..0, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo>");
    }

    #[test]
    fn intrinsic_single_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 1..1, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (_)>");
    }

    #[test]
    fn intrinsic_multi_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 3..3, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (_ _ _)>");
    }

    #[test]
    fn intrinsic_optional() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 0..1, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (?)>");
    }

    #[test]
    fn intrinsic_multi_optional() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 1..3, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (_ ? ?)>");

        assert!(!p.matches_arity(0));
        assert!(p.matches_arity(1));
        assert!(p.matches_arity(2));
        assert!(p.matches_arity(3));
        assert!(!p.matches_arity(4));
    }

    #[test]
    fn intrinsic_open_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 0..255, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (…)>");
    }

    #[test]
    fn intrinsic_required_params_with_open_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 2..255, |_, _| Ok(Value::Unspecified));

        assert_eq!(p.to_string(), "#<procedure foo (_ _ …)>");
    }

    #[test]
    fn lambda_zero_arity_no_name() {
        let p = ok_or_fail!(Procedure::lambda(
            [],
            None,
            empty_body(),
            Binding::default(),
            None
        ));

        assert_eq!(p.to_string(), "#<procedure>");
    }

    #[test]
    fn lambda_zero_arity() {
        let mut sym = SymbolTable::default();
        let p = ok_or_fail!(Procedure::lambda(
            [],
            None,
            empty_body(),
            Binding::default(),
            Some(sym.get("bar"))
        ));

        assert_eq!(p.to_string(), "#<procedure bar>");
    }

    #[test]
    fn lambda_single_arity() {
        let mut sym = SymbolTable::default();
        let p = ok_or_fail!(Procedure::lambda(
            [sym.get("x")],
            None,
            empty_body(),
            Binding::default(),
            Some(sym.get("bar"))
        ));

        assert_eq!(p.to_string(), "#<procedure bar (x)>");
    }

    #[test]
    fn lambda_multi_arity() {
        let mut sym = SymbolTable::default();
        let p = ok_or_fail!(Procedure::lambda(
            [sym.get("x"), sym.get("y"), sym.get("z")],
            None,
            empty_body(),
            Binding::default(),
            Some(sym.get("bar")),
        ));

        assert_eq!(p.to_string(), "#<procedure bar (x y z)>");
    }

    #[test]
    fn lambda_variadic_arity() {
        let mut sym = SymbolTable::default();
        let p = ok_or_fail!(Procedure::lambda(
            [],
            Some(sym.get("any")),
            empty_body(),
            Binding::default(),
            Some(sym.get("bar"))
        ));

        assert_eq!(p.to_string(), "#<procedure bar any…>");
    }

    #[test]
    fn lambda_rest_arity() {
        let mut sym = SymbolTable::default();
        let p = ok_or_fail!(Procedure::lambda(
            [sym.get("x"), sym.get("y"), sym.get("z")],
            Some(sym.get("rest")),
            empty_body(),
            Binding::default(),
            Some(sym.get("bar")),
        ));

        assert_eq!(p.to_string(), "#<procedure bar (x y z rest…)>");
    }

    #[test]
    fn matches_zero_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 0..0, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(0));
    }

    #[test]
    fn matches_single_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 1..1, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(1));
    }

    #[test]
    fn matches_max_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 255..255, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(MAX_ARITY as usize));
    }

    #[test]
    fn matches_min_variable_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 0..3, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(0));
    }

    #[test]
    fn matches_max_variable_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 0..3, |_, _| Ok(Value::Unspecified));

        assert!(p.matches_arity(3));
    }

    #[test]
    fn matches_exceeds_variable_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 0..3, |_, _| Ok(Value::Unspecified));

        assert!(!p.matches_arity(4));
    }

    #[test]
    fn matches_variable_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 2..5, |_, _| Ok(Value::Unspecified));

        assert!(!p.matches_arity(1));
        assert!(p.matches_arity(2));
        assert!(p.matches_arity(3));
        assert!(p.matches_arity(4));
        assert!(p.matches_arity(5));
        assert!(!p.matches_arity(6));
    }

    #[test]
    fn exceeds_max_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 255..255, |_, _| Ok(Value::Unspecified));

        assert!(!p.matches_arity(256));
    }

    #[test]
    fn lambda_matches_zero_arity() {
        let mut sym = SymbolTable::default();
        let p = ok_or_fail!(Procedure::lambda(
            [],
            None,
            empty_body(),
            Binding::default(),
            Some(sym.get("bar"))
        ));

        assert!(p.matches_arity(0));
    }

    #[test]
    fn lambda_matches_single_arity() {
        let mut sym = SymbolTable::default();
        let p = ok_or_fail!(Procedure::lambda(
            [sym.get("x")],
            None,
            empty_body(),
            Binding::default(),
            Some(sym.get("bar"))
        ));

        assert!(p.matches_arity(1));
    }

    #[test]
    fn lambda_matches_multi_arity() {
        let mut sym = SymbolTable::default();
        let p = ok_or_fail!(Procedure::lambda(
            [sym.get("x"), sym.get("y"), sym.get("z")],
            None,
            empty_body(),
            Binding::default(),
            Some(sym.get("bar")),
        ));

        assert!(p.matches_arity(3));
        assert!(!p.matches_arity(2));
        assert!(!p.matches_arity(4));
    }

    #[test]
    fn lambda_matches_variadic_arity() {
        let mut sym = SymbolTable::default();
        let p = ok_or_fail!(Procedure::lambda(
            [],
            Some(sym.get("any")),
            empty_body(),
            Binding::default(),
            Some(sym.get("bar"))
        ));

        assert!(p.matches_arity(0));
        assert!(p.matches_arity(MAX_ARITY as usize));
        assert!(!p.matches_arity(256));
    }

    #[test]
    fn lambda_matches_rest_arity() {
        let mut sym = SymbolTable::default();
        let p = ok_or_fail!(Procedure::lambda(
            [sym.get("x"), sym.get("y")],
            Some(sym.get("any")),
            empty_body(),
            Binding::default(),
            Some(sym.get("bar")),
        ));

        assert!(!p.matches_arity(0));
        assert!(!p.matches_arity(1));
        assert!(p.matches_arity(2));
        assert!(p.matches_arity(MAX_ARITY as usize));
        assert!(!p.matches_arity(256));
    }

    #[test]
    fn lambda_max_arity() {
        let mut sym = SymbolTable::default();
        let params = (0..MAX_ARITY)
            .into_iter()
            .map(|i| sym.get(format!("x{i}")))
            .collect::<Vec<_>>();

        let p = ok_or_fail!(Procedure::lambda(
            params,
            None,
            empty_body(),
            Binding::default(),
            Some(sym.get("bar"))
        ));

        assert!(p.matches_arity(MAX_ARITY as usize));
    }

    #[test]
    fn lambda_max_arity_with_rest() {
        let mut sym = SymbolTable::default();
        let params = (0..MAX_ARITY - 1)
            .into_iter()
            .map(|i| sym.get(format!("x{i}")))
            .collect::<Vec<_>>();

        let p = ok_or_fail!(Procedure::lambda(
            params,
            Some(sym.get("rest")),
            empty_body(),
            Binding::default(),
            Some(sym.get("bar"))
        ));

        assert!(p.matches_arity(MAX_ARITY as usize));
    }

    #[test]
    fn lambda_too_many_params() {
        let mut sym = SymbolTable::default();
        let params = (0..MAX_ARITY as usize + 1)
            .into_iter()
            .map(|i| sym.get(format!("x{i}")))
            .collect::<Vec<_>>();

        let p = Procedure::lambda(
            params,
            None,
            empty_body(),
            Binding::default(),
            Some(sym.get("bar")),
        );

        let err = err_or_fail!(p);

        assert_eq!(err.len(), 1);
        assert!(matches!(&err[0], InvalidFormal::MaxFormals));
    }

    #[test]
    fn lambda_too_many_params_with_rest() {
        let mut sym = SymbolTable::default();
        let params = (0..MAX_ARITY)
            .into_iter()
            .map(|i| sym.get(format!("x{i}")))
            .collect::<Vec<_>>();

        let p = Procedure::lambda(
            params,
            Some(sym.get("rest")),
            empty_body(),
            Binding::default(),
            Some(sym.get("bar")),
        );

        let err = err_or_fail!(p);

        assert_eq!(err.len(), 1);
        assert!(matches!(&err[0], InvalidFormal::MaxFormals));
    }

    #[test]
    fn lambda_duplicate_params() {
        let mut sym = SymbolTable::default();
        let params = [
            sym.get("x"),
            sym.get("y"),
            sym.get("z"),
            sym.get("y"),
            sym.get("x"),
        ];

        let p = Procedure::lambda(
            params,
            None,
            empty_body(),
            Binding::default(),
            Some(sym.get("bar")),
        );

        let err = err_or_fail!(p);

        assert_eq!(err.len(), 2);
        assert!(matches!(&err[0], InvalidFormal::DuplicateFormal(s) if s.as_ref() == "y"));
        assert!(matches!(&err[1], InvalidFormal::DuplicateFormal(s) if s.as_ref() == "x"));
    }

    #[test]
    fn apply_zero_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 0..0, |_, _| Ok(Value::string("bar")));
        let mut env = TestEnv::default();
        let mut f = env.new_frame();
        let args = [];

        let r = p.apply(&args, &mut f);

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::String(s) if s.as_ref() == "bar"));
    }

    #[test]
    fn apply_single_arity() {
        let mut sym = SymbolTable::default();
        let p = Procedure::intrinsic(sym.get("foo"), 1..1, |args, _| {
            let Value::String(s) = &args[0] else {
                unreachable!()
            };
            Ok(Value::string(format!("bar {s}")))
        });
        let mut env = TestEnv::default();
        let mut f = env.new_frame();
        let args = [Value::string("baz")];

        let r = p.apply(&args, &mut f);

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::String(s) if s.as_ref() == "bar baz"));
    }

    #[test]
    fn apply_zero_arity_lambda() {
        let mut sym = SymbolTable::default();
        let mut env = TestEnv::default();
        let mut f = env.new_frame();
        let params = [];
        let p = ok_or_fail!(Procedure::lambda(
            params,
            None,
            body([TokenKind::String("bar".to_owned())]),
            Rc::clone(&f.scope),
            Some(sym.get("bar")),
        ));
        let args = [];

        let r = p.apply(&args, &mut f);

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::String(s) if s.as_ref() == "bar"));
    }

    #[test]
    fn apply_single_arity_lambda() {
        let mut sym = SymbolTable::default();
        let mut env = TestEnv::default();
        let mut f = env.new_frame();
        let params = [sym.get("x")];
        let p = ok_or_fail!(Procedure::lambda(
            params,
            None,
            body([TokenKind::Identifier("x".to_owned())]),
            Rc::clone(&f.scope),
            Some(sym.get("bar")),
        ));
        let args = [Value::Number(Number::real(5))];

        let r = p.apply(&args, &mut f);

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::Number(_)));
        assert_eq!(v.to_string(), "5");
        assert!(!env.binding.bound("x"));
    }

    #[test]
    fn apply_single_arity_lambda_with_closure() {
        let mut sym = SymbolTable::default();
        let mut env = TestEnv::default();
        let global_func = Procedure::intrinsic(sym.get("stringify"), 1..1, |args, _| {
            Ok(Value::string(format!("bar {}", args[0])))
        });
        env.binding
            .bind(sym.get("stringify"), Value::Procedure(global_func.into()));
        let mut f = env.new_frame();
        let params = [sym.get("x")];
        let p = ok_or_fail!(Procedure::lambda(
            params,
            None,
            body([
                TokenKind::ParenLeft,
                TokenKind::Identifier("stringify".to_owned()),
                TokenKind::Identifier("x".to_owned()),
                TokenKind::ParenRight
            ]),
            Rc::clone(&f.scope),
            Some(sym.get("bar")),
        ));
        let args = [Value::Number(Number::real(5))];

        let r = p.apply(&args, &mut f);

        let v = ok_or_fail!(r);
        assert!(matches!(v, Value::String(s) if s.as_ref() == "bar 5"));
        assert!(!env.binding.bound("x"));
    }
}
