#!/usr/bin/env python3
"""Differential arithmetic tester: Zara vs Chez Scheme vs Guile.

Generates a battery of R7RS numeric-tower edge cases (signed zeros,
infinities, NaN, exact/inexact contagion, rounding, bignums, transcendentals,
...), runs the same expressions under all three interpreters, and prints an
agreement matrix. Also doubles as a live gap report for Zara's numeric tower,
since unimplemented procedures show up as their own row category.

No third-party dependencies (stdlib only), matching the rest of this repo's
tooling conventions (see tools/r7rs_coverage.py).

Runnable from the repo root or from tools/ -- all paths are anchored on the
script's own location, not the current working directory.

Usage:
    python3 tools/arith_compare.py
    python3 tools/arith_compare.py --only "Signed zeros" --keep /tmp/zg
    python3 tools/arith_compare.py --md /tmp/report.md
"""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent
if not (REPO_ROOT / "Cargo.toml").exists():
    print(
        f"error: expected {REPO_ROOT} to be the Zara repo root (no Cargo.toml found). "
        "This script must live one directory below the repo root.",
        file=sys.stderr,
    )
    raise SystemExit(1)
DEFAULT_ZARA_BIN = REPO_ROOT / "target" / "debug" / "zara"

# ---------------------------------------------------------------------------
# 1. Case table
# ---------------------------------------------------------------------------


@dataclass
class Case:
    section: str
    expr: str
    note: str = ""


CASES: list[Case] = []


def sec(name: str, exprs: list[str], notes: dict[str, str] | None = None) -> None:
    notes = notes or {}
    for e in exprs:
        CASES.append(Case(name, e, notes.get(e, "")))


sec(
    "Signed zeros",
    [
        "-0.0",
        "(- 0.0)",
        "(+ 0.0 -0.0)",
        "(+ -0.0 -0.0)",
        "(* -1.0 0.0)",
        "(/ 1 -0.0)",
        "(/ -0.0 1)",
        "(= 0.0 -0.0)",
        "(eqv? 0.0 -0.0)",
        "(equal? 0.0 -0.0)",
        "(abs -0.0)",
        "(max -0.0 0.0)",
        "(min 0.0 -0.0)",
        "(exact -0.0)",
        "(negative? -0.0)",
        "(zero? -0.0)",
        "(sqrt -0.0)",
        "(atan 0.0 -0.0)",
        "(floor -0.0)",
        "(truncate -0.0)",
        "(expt -0.0 3)",
        "(number->string -0.0)",
    ],
)

sec(
    "Infinities",
    [
        "(/ 1.0 0.0)",
        "(/ -1.0 0.0)",
        "(/ 1 0.0)",
        "(/ -1 0.0)",
        "(- +inf.0 +inf.0)",
        "(+ +inf.0 -inf.0)",
        "(* 0.0 +inf.0)",
        "(* 0 +inf.0)",
        "(/ +inf.0 +inf.0)",
        "(/ 1.0 +inf.0)",
        "(exact +inf.0)",
        "(inexact +inf.0)",
        "(max 1 +inf.0)",
        "(min 1 -inf.0)",
        "(floor +inf.0)",
        "(expt +inf.0 0)",
        "(atan +inf.0)",
        "(log +inf.0)",
        "(finite? +inf.0)",
        "(infinite? +inf.0)",
        "(integer? +inf.0)",
        "(rational? +inf.0)",
        "(< -inf.0 +inf.0)",
    ],
)

sec(
    "NaN",
    [
        "(/ 0.0 0.0)",
        "(= +nan.0 +nan.0)",
        "(eqv? +nan.0 +nan.0)",
        "(equal? +nan.0 +nan.0)",
        "(< +nan.0 1)",
        "(> +nan.0 1)",
        "(max +nan.0 1.0)",
        "(min 1.0 +nan.0)",
        "(+ +nan.0 1)",
        "(* +nan.0 0)",
        "(expt +nan.0 0)",
        "(nan? +nan.0)",
        "(nan? (- +inf.0 +inf.0))",
        "(exact +nan.0)",
        "(abs +nan.0)",
        "(zero? +nan.0)",
        "(negative? +nan.0)",
    ],
)

sec(
    "Exact/inexact contagion",
    [
        "(+ 1 2.0)",
        "(+ 1/2 0.5)",
        "(* 1/3 3)",
        "(* 0 1.5)",
        "(max 1 2.0)",
        "(min 1 2.0)",
        "(exact? (+ 1 2.0))",
        "(exact->inexact 1/3)",
        "(inexact 1/3)",
        "(exact 0.1)",
        "(exact 1e20)",
        "(inexact->exact 0.5)",
        "(exact (/ 1.0 3.0))",
        "(= 1/3 (/ 1.0 3.0))",
        "(eqv? 2 2.0)",
        "(equal? 2 2.0)",
        "(= 2 2.0)",
        "(integer? 2.0)",
        "(exact-integer? 2.0)",
        "(exact-integer? 2)",
        "(rational? 0.5)",
        "(exact (expt 2.0 53))",
    ],
    notes={
        "(max 1 2.0)": "R7RS: result must be inexact even though 2.0 is the max",
        "(min 1 2.0)": "R7RS: result must be inexact",
        "(* 0 1.5)": "R7RS permits exact 0 * inexact -> exact 0",
    },
)

sec(
    "Exact division & rationals",
    [
        "(/ 1 3)",
        "(/ 6 3)",
        "(exact? (/ 6 3))",
        "(/ 4 2 2)",
        "(/ 1 0)",
        "(/ 0 0)",
        "(/ 1.0 0)",
        "(/ 0 0.0)",
        "(/ 0.0 0)",
        "(/ 2)",
        "(numerator 6/4)",
        "(denominator 6/4)",
        "(numerator 0.5)",
        "(denominator 0.5)",
        "(numerator 2)",
        "(denominator 2.0)",
        "(gcd)",
        "(lcm)",
        "(gcd 0 0)",
        "(gcd -4 6)",
        "(lcm 0 5)",
        "(gcd 4.0 6)",
    ],
)

sec(
    "Rounding",
    [
        "(round 0.5)",
        "(round 1.5)",
        "(round 2.5)",
        "(round -0.5)",
        "(round -2.5)",
        "(round 7/2)",
        "(round 5/2)",
        "(round -7/2)",
        "(floor -1.5)",
        "(ceiling -1.5)",
        "(truncate -1.5)",
        "(floor 1.5)",
        "(ceiling 1.5)",
        "(round 2.675)",
        "(floor -7/2)",
        "(round +nan.0)",
        "(floor +inf.0)",
    ],
    notes={
        "(round 0.5)": "banker's rounding: round to even",
        "(round 1.5)": "banker's rounding: round to even",
        "(round 2.5)": "banker's rounding: round to even",
        "(round -0.5)": "banker's rounding: round to even",
        "(round -2.5)": "banker's rounding: round to even",
        "(round 7/2)": "banker's rounding on exact rationals",
        "(round 5/2)": "banker's rounding on exact rationals",
        "(round -7/2)": "banker's rounding on exact rationals",
    },
)

sec(
    "Integer division with negatives",
    [
        "(quotient -7 2)",
        "(remainder -7 2)",
        "(modulo -7 2)",
        "(quotient 7 -2)",
        "(remainder 7 -2)",
        "(modulo 7 -2)",
        "(modulo -7 -2)",
        "(quotient -7.0 2)",
        "(remainder -7 2.0)",
        "(modulo 0 5)",
        "(quotient 1 0)",
        "(modulo 1 0)",
        "(floor-quotient -7 2)",
        "(floor-remainder -7 2)",
        "(truncate-quotient -7 2)",
        "(call-with-values (lambda () (floor/ -7 2)) list)",
        "(call-with-values (lambda () (truncate/ -7 2)) list)",
    ],
)

sec(
    "expt corners",
    [
        "(expt 0 0)",
        "(expt 0.0 0)",
        "(expt 0 0.0)",
        "(expt 0.0 0.0)",
        "(expt 0 -1)",
        "(expt 0.0 -1)",
        "(expt 2 -1)",
        "(expt 2 10)",
        "(expt 2 100)",
        "(expt 2.0 1024)",
        "(expt -8 1/3)",
        "(expt -1 0.5)",
        "(expt -1 1/2)",
        "(expt 1 +inf.0)",
        "(expt +nan.0 0)",
        "(expt 1/2 2)",
        "(expt 2 0.5)",
    ],
)

sec(
    "Float precision & limits",
    [
        "(+ 0.1 0.2)",
        "(= (+ 0.1 0.2) 0.3)",
        "(* 1e308 10)",
        "(+ 1e16 1.0)",
        "(+ 1e16 2.0)",
        "(- 1e16 (+ 1e16 1.0))",
        "5e-324",
        "(/ 5e-324 2)",
        "(* 5e-324 0.5)",
        "1e21",
        "1e20",
        "1e-7",
        "(exact 5e-324)",
        "(inexact (expt 2 1024))",
        "(inexact 1/3)",
        "(- 1.0 1.0)",
        "(- 0.0 0.0)",
        "(square 1e200)",
    ],
)

sec(
    "Comparison / predicate semantics",
    [
        "(= 1 1 1)",
        "(< 1 2 3)",
        "(< 1 1)",
        "(<= 1 1)",
        "(=)",
        "(<)",
        "(= 1)",
        "(+)",
        "(*)",
        "(- 5)",
        "(max)",
        "(exact? 1.0)",
        "(inexact? 1)",
        "(real? +nan.0)",
        "(complex? 1)",
        "(number? 1/2)",
        "(odd? -3)",
        "(even? 0)",
        "(odd? 3.0)",
    ],
    notes={"(=)": "0-arg comparisons are R7RS-legal (vacuously true)"},
)

sec(
    "Transcendentals",
    [
        "(sqrt 4)",
        "(sqrt 4.0)",
        "(sqrt 1/4)",
        "(sqrt 2)",
        "(sqrt -1)",
        "(sqrt -4)",
        "(sqrt -4.0)",
        "(call-with-values (lambda () (exact-integer-sqrt 17)) list)",
        "(log 0)",
        "(log 0.0)",
        "(log -1)",
        "(log 1)",
        "(log 8 2)",
        "(exp 0)",
        "(exp 1)",
        "(sin 0)",
        "(cos 0)",
        "(asin 2)",
        "(acos 2)",
        "(atan 1 1)",
        "(atan 0 -1)",
        "(atan -0.0 -1.0)",
        "(atan +inf.0 +inf.0)",
    ],
)

sec(
    "Complex",
    [
        "(make-rectangular 3 4)",
        "(make-rectangular 1 0)",
        "(make-rectangular 1.0 0)",
        "(make-rectangular 0 1)",
        "(magnitude -5)",
        "(magnitude (make-rectangular 3 4))",
        "(angle -1)",
        "(angle 1)",
        "(real-part 5)",
        "(imag-part 5)",
        "(make-polar 1 0)",
    ],
)

sec(
    "Bignum / big rational",
    [
        "(* 99999999999 99999999999)",
        "(expt 10 30)",
        "(quotient (expt 10 30) 7)",
        "(+ (expt 2 62) (expt 2 62))",
        "(- (expt 2 63))",
        "(/ (expt 10 30) 3)",
        "(inexact (expt 10 400))",
    ],
)

sec(
    "String <-> number",
    [
        "(number->string 1/3)",
        "(number->string 255 16)",
        "(number->string 1e21)",
        "(number->string -0.0)",
        "(number->string +inf.0)",
        '(string->number "1e3")',
        '(string->number "#e1.5")',
        '(string->number "#i1/2")',
        '(string->number "1/0")',
        '(string->number "+inf.0")',
        '(string->number "abc")',
        '(string->number "ff" 16)',
    ],
)

# ---------------------------------------------------------------------------
# 2. Generation
# ---------------------------------------------------------------------------

# Chez lacks these R7RS names at the top level (no r7rs libs installed);
# shim them in plain Scheme so the comparison is meaningful. Also used to
# annotate affected rows in the report.
CHEZ_SHIMMED = [
    "exact-integer?",
    "square",
    "floor-quotient",
    "floor-remainder",
    "floor/",
    "truncate-quotient",
    "truncate-remainder",
    "truncate/",
]

CHEZ_PRELUDE = """
(define (zflush) (flush-output-port))
(define (exact-integer? x) (and (integer? x) (exact? x)))
(define (square x) (* x x))
(define (floor-quotient a b) (floor (/ a b)))
(define (floor-remainder a b) (- a (* b (floor-quotient a b))))
(define (floor/ a b) (values (floor-quotient a b) (floor-remainder a b)))
(define (truncate-quotient a b) (quotient a b))
(define (truncate-remainder a b) (remainder a b))
(define (truncate/ a b) (values (truncate-quotient a b) (truncate-remainder a b)))
""".strip()

GUILE_PRELUDE = """
(import (scheme base) (scheme write) (scheme inexact) (scheme complex))
(define (zflush) (force-output))
""".strip()

ZARA_PRELUDE = "(define (zflush) (flush-output-port))"

NEUTRAL_PRELUDE = """
;; Prelude-free canonical form. Chez needs R7RS-name shims (no r7rs libs
;; installed) and Guile needs (import ...) + --r7rs; see tools/arith_compare.py.
(define (zflush) (if #f #f))
""".strip()


def build_case_script(prelude: str, case: Case) -> str:
    """One case per process (see module docstring / section 3 below for why):
    just the prelude plus a single display -- no @@N marker is needed since
    the whole process's stdout belongs to this one case, and nothing else
    can run after it to require a resume point.
    """
    return f"{prelude}\n\n(display {case.expr})(newline)\n"


def write_neutral_scm(path: Path, cases: list[Case]) -> None:
    lines = [NEUTRAL_PRELUDE, ""]
    section = None
    for c in cases:
        if c.section != section:
            section = c.section
            lines.append(f";; --- {section} ---")
        lines.append(f"(display {c.expr})(newline)")
    path.write_text("\n".join(lines) + "\n")


# ---------------------------------------------------------------------------
# 3. Running - one isolated process per case
#
# Each case gets its own subprocess rather than being batched into one script
# with the rest of its section. Zara (like most Schemes run non-interactively)
# aborts the whole file on the first unhandled top-level exception, so a
# batched run's later cases silently never execute; a crash classifier that
# then scans the *whole* accumulated output for clues ends up blaming the
# next case for a completely unrelated earlier failure. One process per case
# means classify_crash's input is always exactly that case's own output --
# slower (up to len(cases) * 3 process spawns), but unambiguous.
# ---------------------------------------------------------------------------


@dataclass
class Result:
    kind: str  # OK, UNBOUND, PANIC, CRASH, TIMEOUT, NA
    text: str = ""


# Zara doesn't crash the process on an unhandled unbound-variable reference --
# it prints the condition object as an ordinary displayed value and exits 0
# (see tools/arith_compare.py's run_one_case). That means it never reaches
# classify_crash's "unbound variable" pattern match below, which only fires
# on a genuine crash (empty stdout). Recognize the same case from the
# displayed value itself so it's still counted/reported as UNBOUND rather
# than as a same-answer-format "OK" value that merely disagrees.
DISPLAYED_UNBOUND_RE = re.compile(r'^#<environment-error "(unbound variable: .+)">$')


def classify_crash(stdout: str, stderr: str, returncode: int, timed_out: bool) -> Result:
    if timed_out:
        return Result("TIMEOUT")
    blob = stderr + "\n" + stdout
    if "panicked at" in blob or returncode == 101:
        m = re.search(r"panicked at [^\n]*:\s*\n?\s*(.*)", stderr)
        msg = m.group(1).strip() if m else "panic"
        return Result("PANIC", msg[:120])
    for pat in ("unbound variable", "Unbound variable", "not bound", "is not bound"):
        if pat.lower() in blob.lower():
            return Result("UNBOUND", blob.strip().splitlines()[-1][:120] if blob.strip() else "unbound")
    tail = (stderr.strip() or stdout.strip()).splitlines()
    msg = tail[-1][:120] if tail else f"exit {returncode}"
    return Result("CRASH", msg)


def run_one(cmd: list[str], cwd: Path, timeout: float) -> tuple[str, str, int, bool]:
    try:
        proc = subprocess.run(
            cmd, cwd=cwd, capture_output=True, text=True, timeout=timeout
        )
        return proc.stdout, proc.stderr, proc.returncode, False
    except subprocess.TimeoutExpired as e:
        return (e.stdout or ""), (e.stderr or ""), -1, True


def run_one_case(
    name: str,
    cmd_prefix: list[str],
    prelude: str,
    case: Case,
    idx: int,
    workdir: Path,
    timeout: float,
) -> Result:
    script = build_case_script(prelude, case)
    scm_path = workdir / f"gen_{name}_{idx}.scm"
    scm_path.write_text(script)

    stdout, stderr, rc, timed_out = run_one(cmd_prefix + [str(scm_path)], workdir, timeout)
    if not timed_out:
        # first non-empty line is the result; extra lines ignored
        nonempty = [ln for ln in stdout.splitlines() if ln.strip() != ""]
        if nonempty:
            text = nonempty[0]
            m = DISPLAYED_UNBOUND_RE.match(text)
            if m:
                return Result("UNBOUND", m.group(1))
            return Result("OK", text)
    return classify_crash(stdout, stderr, rc, timed_out)


def run_interpreter(
    name: str,
    cmd_prefix: list[str],
    prelude: str,
    cases: list[Case],
    workdir: Path,
    timeout: float,
) -> list[Result]:
    return [
        run_one_case(name, cmd_prefix, prelude, case, i, workdir, timeout)
        for i, case in enumerate(cases)
    ]


# ---------------------------------------------------------------------------
# 4. Reporting
# ---------------------------------------------------------------------------

FLOAT_RE = re.compile(r"^[+-]?(\d+\.\d*|\.\d+|\d+)(e[+-]?\d+)?$", re.IGNORECASE)


def same_float_value(a: str, b: str) -> bool:
    try:
        return float(a) == float(b)
    except ValueError:
        return False


def cell_text(r: Result) -> str:
    if r.kind == "OK":
        return r.text
    if r.kind == "NA":
        return "n/a"
    if r.text:
        return f"{r.kind}({r.text})"
    return r.kind


def truncate(s: str, width: int) -> str:
    if len(s) <= width:
        return s
    return s[: width - 1] + "\u2026"


LETTER = {"zara": "Z", "chez": "C", "guile": "G"}


def verdict(z: Result, c: Result, g: Result, have) -> tuple[str, bool, bool]:
    """Return (tag, is_float_only_diff, is_gap).

    `have` is the set of interpreters actually run this session (globally
    available). Within that set, a per-row failure (UNBOUND/PANIC/CRASH/
    TIMEOUT) is a *gap*, distinct from an interpreter never being run at
    all: a gap must never be silently absorbed into a "===" agreement just
    because the interpreters that *did* answer happen to agree.
    """
    results = {"zara": z, "chez": c, "guile": g}
    avail = [l for l in ("zara", "chez", "guile") if l in have]
    errored = [l for l in avail if results[l].kind != "OK"]
    ok = [l for l in avail if results[l].kind == "OK"]

    if len(avail) < 2:
        return "--", False, False

    if len(ok) < 2:
        return "--", False, bool(errored)

    vals = {l: results[l].text for l in ok}
    uniq = set(vals.values())
    float_only = len(uniq) > 1 and all(
        same_float_value(a, b) for a in uniq for b in uniq
    )

    if len(uniq) == 1:
        if errored:
            # the ones that answered agree, but someone else errored on
            # this row -- that is still a real gap, not full agreement.
            tag = "".join(sorted(LETTER[l] for l in errored)) + "!"
            return tag, float_only, True
        return "===", False, False

    gap_suffix = ("+" + "".join(sorted(LETTER[l] for l in errored)) + "!") if errored else ""

    if len(ok) == 3:
        from collections import Counter

        cnt = Counter(vals.values())
        if len(cnt) == 2:
            minority_val = min(cnt, key=lambda k: cnt[k])
            minority = [k for k, v in vals.items() if v == minority_val]
            if len(minority) == 1:
                tag = f"{LETTER[minority[0]]}\u2260{gap_suffix}"
                return tag, float_only, bool(errored)
        return f"***{gap_suffix}", float_only, bool(errored)

    # exactly two answered, disagree
    return f"\u2260\u2260{gap_suffix}", float_only, bool(errored)


def make_report(
    cases: list[Case],
    results: dict[str, list[Result]],
    have: set[str],
    width: int,
) -> str:
    out = []
    RULE = "=" * 78

    def hdr(title: str) -> None:
        out.append(RULE)
        out.append(title)
        out.append(RULE)

    cols = [c for c in ("zara", "chez", "guile") if True]

    hdr("1. PER-EXPRESSION MATRIX")
    section = None
    counts = {"===": 0, "--": 0, "gap": 0, "diff": 0}
    row_verdicts: list[tuple[Case, Result, Result, Result, str, bool, bool]] = []

    for i, c in enumerate(cases):
        z = results["zara"][i]
        ch = results["chez"][i]
        g = results["guile"][i]
        v, float_only, is_gap = verdict(z, ch, g, have)
        row_verdicts.append((c, z, ch, g, v, float_only, is_gap))
        if v == "===":
            counts["==="] += 1
        elif v == "--":
            counts["--"] += 1
        elif is_gap:
            counts["gap"] += 1
        else:
            counts["diff"] += 1

    for c, z, ch, g, v, float_only, is_gap in row_verdicts:
        if c.section != section:
            section = c.section
            out.append("")
            out.append(f"-- {section} --")
        tag = v + ("~" if float_only else "")
        expr = truncate(c.expr, width)
        zt = truncate(cell_text(z), width) if "zara" in have else "n/a"
        ct = truncate(cell_text(ch), width) if "chez" in have else "n/a"
        gt = truncate(cell_text(g), width) if "guile" in have else "n/a"
        out.append(f"    {tag:>4}  {expr}")
        out.append(f"          zara={zt}  chez={ct}  guile={gt}")

    out.append("")
    out.append(
        f"--> Total: {counts['===']}/{len(cases)} agree, "
        f"{counts['diff']} value-disagree, {counts['gap']} gap "
        f"(an interpreter errored while others answered), "
        f"{counts['--']} unusable (<2 answered)"
    )

    hdr("2. DISAGREEMENTS")
    any_diff = False
    for c, z, ch, g, v, float_only, is_gap in row_verdicts:
        if v in ("===", "--"):
            continue
        any_diff = True
        note = f"  [{c.note}]" if c.note else ""
        tag = v + ("~ (same float value)" if float_only else "")
        out.append(f"    [{tag}] {c.expr}{note}")
        for label, r in (("zara", z), ("chez", ch), ("guile", g)):
            if label not in have:
                continue
            out.append(f"        {label:6}: {cell_text(r)}")
    if not any_diff:
        out.append("    (none)")
    out.append("")
    out.append(
        f"--> Total non-agreeing rows: {counts['diff'] + counts['gap']} "
        f"({counts['diff']} value-disagree, {counts['gap']} gap)"
    )

    hdr("3. ZARA GAPS (UNBOUND / PANIC)")
    if "zara" in have:
        gaps: dict[str, list[str]] = {"UNBOUND": [], "PANIC": []}
        for i, c in enumerate(cases):
            r = results["zara"][i]
            if r.kind in gaps:
                gaps[r.kind].append(c.expr)
        for kind in ("UNBOUND", "PANIC"):
            out.append(f"\n{kind} ({len(gaps[kind])})")
            for e in gaps[kind]:
                out.append(f"    {e}")
        out.append("")
        out.append(
            f"--> Total gaps: {len(gaps['UNBOUND']) + len(gaps['PANIC'])}/{len(cases)}"
        )
    else:
        out.append("    zara not run")

    hdr("SUMMARY")
    total = len(cases)

    def pct(n: int) -> str:
        return f"{100 * n / total:.1f}%" if total else "0.0%"

    out.append(f"total cases:             {total}")
    out.append(f"all agree:               {counts['===']} ({pct(counts['==='])})")
    out.append(f"value-disagree:          {counts['diff']} ({pct(counts['diff'])})")
    out.append(f"gap (partial errored):   {counts['gap']} ({pct(counts['gap'])})")
    out.append(f"unusable (<2 answered):  {counts['--']} ({pct(counts['--'])})")

    for pair in (("zara", "chez"), ("zara", "guile"), ("chez", "guile")):
        a, b = pair
        if a not in have or b not in have:
            continue
        both_ok = 0
        agree = 0
        for i in range(total):
            ra = results[a][i]
            rb = results[b][i]
            if ra.kind == "OK" and rb.kind == "OK":
                both_ok += 1
                if ra.text == rb.text:
                    agree += 1
        rate = f"{100 * agree / both_ok:.1f}%" if both_ok else "n/a"
        out.append(f"{a}<->{b} agreement (of {both_ok} both-OK rows): {agree} ({rate})")

    out.append("")
    for name in ("zara", "chez", "guile"):
        if name not in have:
            out.append(f"{name}: not available (n/a)")

    if "chez" in have:
        out.append("")
        out.append("chez shim legend (\u2020 not natively bound, defined in prelude):")
        out.append("    " + ", ".join(CHEZ_SHIMMED))

    return "\n".join(out) + "\n"


def make_markdown(
    cases: list[Case],
    results: dict[str, list[Result]],
    have: set[str],
) -> str:
    lines = ["# Arithmetic edge-case comparison: Zara vs Chez vs Guile", ""]
    lines.append("| Expr | Zara | Chez | Guile | Verdict |")
    lines.append("|---|---|---|---|---|")
    section = None
    for i, c in enumerate(cases):
        if c.section != section:
            section = c.section
            lines.append(f"| **{section}** | | | | |")
        z = results["zara"][i]
        ch = results["chez"][i]
        g = results["guile"][i]
        v, float_only, _is_gap = verdict(z, ch, g, have)
        tag = v + ("~" if float_only else "")

        def md_cell(label, r):
            if label not in have:
                return "n/a"
            return cell_text(r).replace("|", "\\|").replace("\n", " ")

        expr = c.expr.replace("|", "\\|")
        lines.append(
            f"| `{expr}` | {md_cell('zara', z)} | {md_cell('chez', ch)} | "
            f"{md_cell('guile', g)} | {tag} |"
        )
    return "\n".join(lines) + "\n"


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------


def find_binary(name: str) -> str | None:
    return shutil.which(name)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--md", type=Path, help="also write a Markdown report here")
    ap.add_argument(
        "--only", action="append", default=[], help="only run sections matching this substring (repeatable)"
    )
    ap.add_argument("--keep", type=Path, help="keep generated per-case .scm files in this dir")
    ap.add_argument("--no-build", action="store_true", help="skip cargo build")
    ap.add_argument("--zara", help="path to zara binary")
    ap.add_argument("--chez", default="chez", help="chez command/path")
    ap.add_argument("--guile", default="guile", help="guile command/path")
    ap.add_argument("--timeout", type=float, default=20.0)
    ap.add_argument("--width", type=int, default=200)
    args = ap.parse_args()

    cases = CASES
    if args.only:
        cases = [c for c in cases if any(s.lower() in c.section.lower() for s in args.only)]
        if not cases:
            print("no cases matched --only filter(s)", file=sys.stderr)
            return 1

    # Resolve interpreters
    zara_bin = args.zara
    if not zara_bin:
        if not args.no_build:
            print("building zara (cargo build)...", file=sys.stderr)
            build = subprocess.run(
                ["cargo", "build"], cwd=REPO_ROOT, capture_output=True, text=True
            )
            if build.returncode != 0:
                print(build.stdout, file=sys.stderr)
                print(build.stderr, file=sys.stderr)
                print("cargo build failed", file=sys.stderr)
                return 1
        zara_bin = str(DEFAULT_ZARA_BIN)
        if not Path(zara_bin).exists():
            zara_bin = None

    chez_bin = find_binary(args.chez) or (args.chez if Path(args.chez).exists() else None)
    guile_bin = find_binary(args.guile) or (args.guile if Path(args.guile).exists() else None)

    have: set[str] = set()
    if zara_bin and Path(zara_bin).exists():
        have.add("zara")
    if chez_bin:
        have.add("chez")
    if guile_bin:
        have.add("guile")

    for name in ("zara", "chez", "guile"):
        if name not in have:
            print(f"warning: {name} not available, that column will be n/a", file=sys.stderr)

    workdir = Path(args.keep) if args.keep else Path(tempfile.mkdtemp(prefix="arith_compare_"))
    workdir.mkdir(parents=True, exist_ok=True)

    # Canonical human-readable artifact always reflects the full case set,
    # even when --only narrows what this particular run compares. Lives
    # beside the generator, not the repo root.
    write_neutral_scm(SCRIPT_DIR / "arith_edge.scm", CASES)

    results: dict[str, list[Result]] = {}

    if "zara" in have:
        print("running zara...", file=sys.stderr)
        results["zara"] = run_interpreter(
            "zara", [zara_bin], ZARA_PRELUDE, cases, workdir, args.timeout
        )
    else:
        results["zara"] = [Result("NA") for _ in cases]

    if "chez" in have:
        print("running chez...", file=sys.stderr)
        results["chez"] = run_interpreter(
            "chez", [chez_bin, "--script"], CHEZ_PRELUDE, cases, workdir, args.timeout
        )
    else:
        results["chez"] = [Result("NA") for _ in cases]

    if "guile" in have:
        print("running guile...", file=sys.stderr)
        results["guile"] = run_interpreter(
            "guile",
            [guile_bin, "--r7rs", "--no-auto-compile", "-s"],
            GUILE_PRELUDE,
            cases,
            workdir,
            args.timeout,
        )
    else:
        results["guile"] = [Result("NA") for _ in cases]

    report = make_report(cases, results, have, args.width)
    print(report)

    if args.md:
        md = make_markdown(cases, results, have)
        args.md.write_text(md)
        print(f"markdown report written to {args.md}", file=sys.stderr)

    if not args.keep:
        shutil.rmtree(workdir, ignore_errors=True)
    else:
        print(f"generated scripts kept in {workdir}", file=sys.stderr)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
