#!/usr/bin/env python3
"""
Scans src/core/ for bound intrinsic procedures and compares them against the
R7RS-small Appendix A "Standard Libraries" export list (errata-corrected R7RS,
Appendix A, all sections except "R5RS Library").

Reports, grouped by library:
  1. Procedures bound in src/core/
  2. Procedures not yet bound
  3. Extended (Zara-specific) procedures bound in src/core/ that are not in
     Appendix A at all, grouped by the source file that defines them.

Appendix A also exports syntax keywords (e.g. `if`, `let`, `define`,
`syntax-rules`, `delay`, `case-lambda`, ...). Those are handled by the
lexer/parser/evaluator, not bound as intrinsics in src/core/, so they are
intentionally excluded from the tables below -- this script only tracks
*procedures*.
"""

import re
import glob
import os

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(SCRIPT_DIR)
CORE_DIR = os.path.join(REPO_ROOT, "src", "core")

# ---------------------------------------------------------------------------
# R7RS Appendix A, by library (procedures only -- syntax keywords omitted).
# Source: errata-corrected-r7rs.pdf, Appendix A "Standard Libraries", pages 73-76.
# The "R5RS Library" section is excluded per request.
# ---------------------------------------------------------------------------

APPENDIX_A = {
    "Base (scheme base)": [
        "*", "+", "-", "/", "<", "<=", "=", ">", ">=",
        "abs", "append", "apply", "assoc", "assq", "assv",
        "binary-port?", "boolean=?", "boolean?", "bytevector", "bytevector-append",
        "bytevector-copy", "bytevector-copy!", "bytevector-length", "bytevector-u8-ref",
        "bytevector-u8-set!", "bytevector?",
        "caar", "cadr", "call-with-current-continuation", "call-with-port",
        "call-with-values", "call/cc", "car", "cdar", "cddr", "cdr", "ceiling",
        "char->integer", "char-ready?", "char<=?", "char<?", "char=?", "char>=?",
        "char>?", "char?", "close-input-port", "close-output-port", "close-port",
        "complex?", "cons", "current-error-port", "current-input-port",
        "current-output-port",
        "denominator", "dynamic-wind",
        "eof-object", "eof-object?", "eq?", "equal?", "eqv?", "error",
        "error-object-irritants", "error-object-message", "error-object?", "even?",
        "exact", "exact-integer-sqrt", "exact-integer?", "exact?", "expt",
        "features", "file-error?", "floor", "floor-quotient", "floor-remainder",
        "floor/", "flush-output-port", "for-each",
        "gcd", "get-output-bytevector", "get-output-string",
        "inexact", "inexact?", "input-port-open?", "input-port?", "integer->char",
        "integer?",
        "lcm", "length", "list", "list->string", "list->vector", "list-copy",
        "list-ref", "list-set!", "list-tail", "list?",
        "make-bytevector", "make-list", "make-parameter", "make-string",
        "make-vector", "map", "max", "member", "memq", "memv", "min", "modulo",
        "negative?", "newline", "not", "null?", "number->string", "number?",
        "numerator",
        "odd?", "open-input-bytevector", "open-input-string", "open-output-bytevector",
        "open-output-string", "output-port-open?", "output-port?",
        "pair?", "peek-char", "peek-u8", "port?", "positive?", "procedure?",
        "quotient",
        "raise", "raise-continuable", "rational?", "rationalize", "read-bytevector",
        "read-bytevector!", "read-char", "read-error?", "read-line", "read-string",
        "read-u8", "real?", "remainder", "reverse",
        "set-car!", "set-cdr!", "square", "string", "string->list", "string->number",
        "string->symbol", "string->utf8", "string->vector", "string-append",
        "string-copy", "string-copy!", "string-fill!", "string-for-each",
        "string-length", "string-map", "string-ref", "string-set!", "string<=?",
        "string<?", "string=?", "string>=?", "string>?", "string?", "substring",
        "symbol->string", "symbol=?", "symbol?",
        "textual-port?", "truncate", "truncate-quotient", "truncate-remainder",
        "truncate/",
        "u8-ready?", "utf8->string",
        "values", "vector", "vector->list", "vector->string", "vector-append",
        "vector-copy", "vector-copy!", "vector-fill!", "vector-for-each",
        "vector-length", "vector-map", "vector-ref", "vector-set!", "vector?",
        "with-exception-handler", "write-bytevector", "write-char", "write-string",
        "write-u8",
        "zero?",
    ],
    # (scheme case-lambda) exports only the `case-lambda` syntax keyword -- no procedures.
    "Case-Lambda (scheme case-lambda)": [],
    "Char (scheme char)": [
        "char-alphabetic?", "char-ci<=?", "char-ci<?", "char-ci=?", "char-ci>=?",
        "char-ci>?", "char-downcase", "char-foldcase", "char-lower-case?",
        "char-numeric?", "char-upcase", "char-upper-case?", "char-whitespace?",
        "digit-value",
        "string-ci<=?", "string-ci<?", "string-ci=?", "string-ci>=?", "string-ci>?",
        "string-downcase", "string-foldcase", "string-upcase",
    ],
    "Complex (scheme complex)": [
        "angle", "imag-part", "magnitude", "make-polar", "make-rectangular", "real-part",
    ],
    "CxR (scheme cxr)": [
        "caaaar", "caaadr", "caaar", "caadar", "caaddr", "caadr", "cadaar", "cadadr",
        "cadar", "caddar", "cadddr", "caddr",
        "cdaaar", "cdaadr", "cdaar", "cdadar", "cdaddr", "cdadr", "cddaar", "cddadr",
        "cddar", "cdddar", "cddddr", "cdddr",
    ],
    "Eval (scheme eval)": [
        "environment", "eval",
    ],
    "File (scheme file)": [
        "call-with-input-file", "call-with-output-file", "delete-file", "file-exists?",
        "open-binary-input-file", "open-binary-output-file", "open-input-file",
        "open-output-file", "with-input-from-file", "with-output-to-file",
    ],
    "Inexact (scheme inexact)": [
        "acos", "asin", "atan", "cos", "exp", "finite?", "infinite?", "log", "nan?",
        "sin", "sqrt", "tan",
    ],
    # (scheme lazy) also exports `delay`/`delay-force` syntax keywords -- omitted here.
    "Lazy (scheme lazy)": [
        "force", "make-promise", "promise?",
    ],
    "Load (scheme load)": [
        "load",
    ],
    "Process-Context (scheme process-context)": [
        "command-line", "emergency-exit", "exit", "get-environment-variable",
        "get-environment-variables",
    ],
    "Read (scheme read)": [
        "read",
    ],
    "Repl (scheme repl)": [
        "interaction-environment",
    ],
    "Time (scheme time)": [
        "current-jiffy", "current-second", "jiffies-per-second",
    ],
    "Write (scheme write)": [
        "display", "write", "write-shared", "write-simple",
    ],
}

ALL_APPENDIX_PROCS = set(p for procs in APPENDIX_A.values() for p in procs)

# ---------------------------------------------------------------------------
# Scan src/core/ for bind_intrinsic(env, "name", ...) calls.
# ---------------------------------------------------------------------------

BIND_PATTERN = re.compile(
    r'bind_intrinsic\(\s*env\s*,\s*"([^"]+)"\s*,\s*[^,]+,\s*([A-Za-z_][A-Za-z0-9_]*)\s*,?\s*\)'
)

# A stub body is exactly one statement: a bare `todo!(...)` macro call,
# e.g. `todo!();` or `todo!("need char_fold");`. Todos buried inside a
# macro-generated closure (e.g. `try_predicate!(..., |c| todo!(...))`) are
# NOT flagged, since the *bound function's* body there is the macro's
# generated dispatch code, not a lone todo!.
STUB_BODY_PATTERN = re.compile(r'^todo!\(.*\)\s*;?$', re.DOTALL)


def find_function_body(text, fn_name):
    """Returns the source text between the braces of `fn fn_name(...) { ... }`,
    or None if no such top-level function definition is found."""
    for m in re.finditer(r'\bfn\s+' + re.escape(fn_name) + r'\s*\(', text):
        i = m.end() - 1  # index of the '('
        depth = 0
        while i < len(text):
            if text[i] == '(':
                depth += 1
            elif text[i] == ')':
                depth -= 1
                if depth == 0:
                    break
            i += 1
        else:
            continue
        brace_start = text.find('{', i)
        if brace_start == -1:
            continue
        depth = 0
        j = brace_start
        while j < len(text):
            if text[j] == '{':
                depth += 1
            elif text[j] == '}':
                depth -= 1
                if depth == 0:
                    return text[brace_start + 1:j].strip()
            j += 1
    return None


def scan_core():
    """Returns (bindings, stubs):
    bindings: procedure name -> list of (relative file path, fn identifier)
    stubs: set of (relative file path, fn identifier) whose body is only todo!(...)
    """
    bindings = {}
    stubs = set()
    rs_files = sorted(glob.glob(os.path.join(CORE_DIR, "**", "*.rs"), recursive=True))
    for path in rs_files:
        rel = os.path.relpath(path, REPO_ROOT)
        if os.path.basename(os.path.dirname(path)) == "tests" or path.endswith("tests.rs"):
            continue
        with open(path, "r") as fh:
            text = fh.read()
        for name, fn_ident in BIND_PATTERN.findall(text):
            bindings.setdefault(name, []).append((rel, fn_ident))
            body = find_function_body(text, fn_ident)
            if body is not None and STUB_BODY_PATTERN.match(body):
                stubs.add((rel, fn_ident))
    return bindings, stubs


def is_stub(bindings, stubs, name):
    """A bound name is a stub if every (file, fn) binding it resolves to is a
    bare todo!() body. (In practice each name binds to exactly one function.)"""
    locs = bindings[name]
    return locs and all(loc in stubs for loc in locs)


def main():
    bindings, stubs = scan_core()
    bound_names = set(bindings)

    print("=" * 78)
    print("1. BOUND PROCEDURES (implemented in src/core/), grouped by R7RS library")
    print("   [STUB] = bound but body is only todo!(...) -- not really implemented")
    print("=" * 78)
    total_bound = 0
    total_stub = 0
    for lib, procs in APPENDIX_A.items():
        bound_in_lib = sorted(p for p in procs if p in bound_names)
        if not procs:
            continue
        stub_count = sum(1 for p in bound_in_lib if is_stub(bindings, stubs, p))
        print(f"\n{lib}  ({len(bound_in_lib)}/{len(procs)}, {stub_count} stub)")
        for p in bound_in_lib:
            files = ", ".join(f for f, _fn in bindings[p])
            tag = "  [STUB]" if is_stub(bindings, stubs, p) else ""
            print(f"    {p}    [{files}]{tag}")
        total_bound += len(bound_in_lib)
        total_stub += stub_count
    print(f"\n--> Total bound: {total_bound}/{len(ALL_APPENDIX_PROCS)} ({total_stub} are stubs)")

    print()
    print("=" * 78)
    print("2. NOT YET BOUND (missing from src/core/), grouped by R7RS library")
    print("=" * 78)
    total_missing = 0
    for lib, procs in APPENDIX_A.items():
        missing = sorted(p for p in procs if p not in bound_names)
        if not missing:
            continue
        print(f"\n{lib}  ({len(missing)}/{len(procs)} missing)")
        for p in missing:
            print(f"    {p}")
        total_missing += len(missing)
    print(f"\n--> Total missing: {total_missing}/{len(ALL_APPENDIX_PROCS)}")

    print()
    print("=" * 78)
    print("3. EXTENDED PROCEDURES (bound in src/core/, not in Appendix A),")
    print("   grouped by defining source file")
    print("=" * 78)
    extended = {name: locs for name, locs in bindings.items() if name not in ALL_APPENDIX_PROCS}
    by_file = {}
    for name, locs in extended.items():
        for f, _fn in locs:
            by_file.setdefault(f, []).append(name)
    total_extended_stub = 0
    for f in sorted(by_file):
        names = sorted(by_file[f])
        print(f"\n{f}  ({len(names)})")
        for n in names:
            tag = "  [STUB]" if is_stub(bindings, stubs, n) else ""
            if tag:
                total_extended_stub += 1
            print(f"    {n}{tag}")
    print(f"\n--> Total extended: {len(extended)} ({total_extended_stub} are stubs)")

    print()
    print("=" * 78)
    print("4. ALL STUBS (bound names whose body is only todo!(...))")
    print("=" * 78)
    all_stub_names = sorted(n for n in bound_names if is_stub(bindings, stubs, n))
    for n in all_stub_names:
        files = ", ".join(f for f, _fn in bindings[n])
        print(f"    {n}    [{files}]")
    print(f"\n--> Total stubs: {len(all_stub_names)}")

    print()
    print("=" * 78)
    print("SUMMARY")
    print("=" * 78)
    print(f"Appendix A procedures (excl. R5RS library): {len(ALL_APPENDIX_PROCS)}")
    print(f"  bound:             {total_bound}  ({total_stub} are stubs)")
    print(f"  missing:           {total_missing}")
    print(f"Extended (non-Appendix A) procedures bound: {len(extended)}  ({total_extended_stub} are stubs)")
    print(f"Total intrinsics bound in src/core/:         {len(bound_names)}")
    print(f"Total stubs (bound, body is only todo!):     {len(all_stub_names)}")
    print(f"Truly implemented (bound and not a stub):    {len(bound_names) - len(all_stub_names)}")


if __name__ == "__main__":
    main()
