# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Zara is an R7RS-small Scheme interpreter written in Rust, built from scratch with minimal 3rd-party dependencies (only `rustyline`, for the REPL). The project's stated purpose is exploring Rust while implementing a Scheme interpreter, so idiomatic, dependency-light Rust is preferred over pulling in crates for things that can reasonably be hand-rolled.

The package produces two targets from one crate: a `lib` (the interpreter, `src/lib.rs`) and a `bin` (the CLI/REPL, `src/main.rs`).

## Commands

- Build: `cargo build`
- Run the REPL: `cargo run`
- Run a script file: `cargo run -- path/to/file.scm`
- Run a program string / stdin: `cargo run -- - "(+ 1 2)"` or `cargo run -- -`
- Show CLI usage: `cargo run -- -h`
- Print tokens instead of evaluating: `cargo run -- -T ...`
- Print the syntax tree instead of evaluating: `cargo run -- -S ...` (combine with `-T`/`-ST`/`-TS` for both)
- Run all tests (unit + integration): `cargo test`
- Run a single test: `cargo test <test_name>` (e.g. `cargo test closure_per_eval`)
- Run tests in one module: `cargo test lex::tokenize::tests::`
- Run only the integration suite: `cargo test --test integration`
- Run ignored tests: `cargo test -- --ignored`
- Lint: `cargo clippy`

There is no CI config, rustfmt config, or clippy config checked into the repo — rely on `cargo fmt`/`cargo clippy` defaults, and match existing style/`#[allow(clippy::...)]` patterns already in the code (see `src/number.rs`, `src/value.rs`) rather than introducing new lint suppressions casually.

Stray `.scm`/`.txt`/`.bin` files in the repo root (e.g. `sample.scm`, `foo.txt`, `bytes.txt`) are ad hoc manual-testing fixtures for exercising the REPL/file runner by hand, not part of the build.

## Architecture

Zara evaluates source through a four-stage pipeline, each stage supporting partial/multi-line input (needed for the REPL):

```
TextSource -> Lexer -> Parser -> Evaluator -> Evaluation
 (txt/src)   (lex)    (syntax)   (eval/core)
```

1. **`src/txt.rs` + `src/src.rs`** — `TextSource` is a trait over an iterator of text lines with position/context info (`TextContext`, `TextLine`), abstracting REPL input, files, and strings (`StringSource`) uniformly. All error messages carry back to a `TextLine`/span so they can render a caret-pointed snippet.
2. **`src/lex.rs`** (+ `lex/token.rs`, `lex/tokenize.rs`, `lex/tokenize/lexers/*`) — turns `TextLine`s into `TokenLine`s. Lexing is line-oriented but token constructs can span lines (block comments, multi-line strings); `Lexer` tracks a `TokenContinuation` and returns `LexerOutput::Continuation` when more input is needed rather than erroring, which is how the REPL knows to keep prompting.
3. **`src/syntax.rs`** (+ `syntax/expr.rs`, `syntax/parse.rs`, `syntax/parse/form.rs`) — turns tokens into an expression tree (`Sequence`). Two parser implementations share the token stream: `ExprParser<PrgBasis>` (aka `ExpressionTree`) builds real program forms; `ExprParser<DataBasis>` (aka `DataTree`) builds quoted/literal data. `TokenList` is a pass-through "parser" used for `-T`/token-dump mode. Like the lexer, parsing can signal `ParserOutput::Continuation` for incomplete forms (e.g. an unclosed `(if ...`).
4. **`src/eval.rs`** (+ `eval/env.rs`, `eval/proc.rs`) — evaluates a parsed `Sequence` against an `Environment<T>`, generic over an `Evaluator` driver: `EvalDriver` (aka `Eval`) actually evaluates; `AstDriver` (aka `Ast`) just returns the parsed AST as data (backs `-S`/syntax-tree mode). `Environment` owns the global `Binding` scope, the interned `SymbolTable`, and `System` (interactive/args state), and builds a `Frame` (borrowed scope + symbols + system) per evaluation.

`src/lib.rs`'s `Interpreter` ties these stages together and picks the right `Executor`/`Engine<Parser, Evaluator>` combination based on `RunMode` (`Evaluate`, `SyntaxTree`, `Tokenize`, `TokenTree` — these compose via `Add`/`AddAssign` so e.g. `SyntaxTree + Tokenize = TokenTree`). `Interpreter::run` returns early with `Evaluation::Continuation` if any stage needs more input, and `unsupported_continuation` lets non-interactive callers (a script that ends mid-expression) surface a hard error instead of waiting for more lines.

### Values and builtins

- **`src/value.rs`** defines the runtime `Value` enum (the public `eval::Value` wraps it) and `Condition`/error-condition machinery, plus `value/display.rs` for datum printing (including shared/circular structure via `#0=`/`#0#` labels — see the `several_layers_of_circular_lists` integration test) and `value/port.rs` for ports.
- **`src/number.rs`** implements the numeric tower (exact/inexact, integer/rational/real, complex support layered in via `src/core/complex.rs`).
- **`src/core.rs`** and its submodules (`core/base.rs` + `core/base/{collections,num}.rs`, `core/charuni.rs`, `core/complex.rs`, `core/cxr.rs`, `core/ext.rs`, `core/file.rs`, `core/inexact.rs`, `core/procctx.rs`, `core/time.rs`) implement the standard library, organized roughly by R7RS library section. Each submodule exposes a `load(env: &Frame)` that binds its intrinsics; `core::load` calls them all. New builtins are added via `bind_intrinsic(env, name, arity, fn_ptr)` where `fn_ptr: IntrinsicFn = fn(&[Value], &Frame) -> EvalResult`. `core.rs` also defines shared macros (`predicate!`, `try_predicate!`, `cadr_func!`/`cadr_compose!`, `num_convert!`) used across the builtin modules to cut down on repetitive intrinsic boilerplate — prefer extending these over writing new one-off intrinsic functions by hand when the shape fits.
- Argument validation inside intrinsics assumes the evaluator already checked arity, so intrinsic bodies index/pattern-match args directly rather than re-checking length (see the comment above `FIRST_ARG_LABEL` in `core.rs`).

### CLI layer (`src/main.rs` + friends)

`args.rs` hand-rolls CLI parsing (no `clap`) into an `Args`/`Cmd`/`Input` model, `cli.rs` defines the process exit-code/error wiring, `repl.rs` drives the interactive rustyline loop, and `run.rs` dispatches file/stdin/prg/REPL execution into the `Interpreter`. `build.rs` bakes the rustc version and `cargo tree` dependency list into env vars (`ZARA_COMPILER_VERSION`, `ZARA_DEPENDENCIES`) consumed by `args::version`.

## Testing conventions

- Unit tests live alongside the code they test, either inline (`mod tests { ... }` at the bottom of a file) or, for larger suites, split into a sibling `tests.rs`/`tests/` module pulled in via `#[cfg(test)] mod tests;` (e.g. `src/lex/tokenize/tests/tokenizer/*.rs` breaks tokenizer tests out by token kind). Follow whichever pattern the file already uses.
- `src/testutil.rs` (test-only, `#[cfg(test)]`) provides shared helpers/macros: `extract_or_fail!`, `ok_or_fail!`, `err_or_fail!`, `some_or_fail!` (assert-and-unwrap in one step) and `zlist_mut!` (build a mutable Scheme list literal for test fixtures), plus constructors like `make_textline()`.
- `tests/integration.rs` drives the whole pipeline end-to-end through the public `zara` crate API (`Interpreter`, `StringSource`) via a small `TestRunner` harness with `run_for_val`/`run_for_err`/`run_for_cont` helpers — use this style for tests that exercise full program evaluation (multi-line/continuation behavior, closures, circular structures) rather than a single pipeline stage.
- Tests marked `#[ignore = "..."]` document known-missing features or open language-design questions (e.g. nested `define`, quote-as-transformer) — check for one of these before assuming a failing behavior is a bug to fix outright; read the ignore reason and any nearby `TODO` first.
