use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use miko::compile::compile_str;
use miko::typeinfer::TypeError;

fn miko_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_miko"))
}

fn compile_and_run_file(gs_path: &Path, extra_env: &[(&str, &str)]) -> String {
    let dir = tempfile::tempdir().expect("tempdir");
    let bin_path = dir.path().join("prog_out");

    let output = Command::new(miko_bin())
        .arg("-o")
        .arg(&bin_path)
        .arg(gs_path)
        .output()
        .expect("failed to invoke miko");

    assert!(
        output.status.success(),
        "miko failed compiling {}:\nstdout: {}\nstderr: {}",
        gs_path.display(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let mut run_cmd = Command::new(&bin_path);
    for (k, v) in extra_env {
        run_cmd.env(k, v);
    }
    let run_out = run_cmd.output().expect("failed to execute compiled binary");
    assert!(
        run_out.status.success(),
        "binary crashed for {} (status {:?}):\nstderr: {}",
        gs_path.display(),
        run_out.status.code(),
        String::from_utf8_lossy(&run_out.stderr)
    );
    String::from_utf8(run_out.stdout).expect("utf8 stdout")
}

fn compile_and_run_src(src: &str) -> String {
    compile_and_run_src_with_env(src, &[])
}

fn compile_and_run_src_with_env(src: &str, extra_env: &[(&str, &str)]) -> String {
    let dir = tempfile::tempdir().expect("tempdir");
    let src_path = dir.path().join("main.gs");
    fs::write(&src_path, src).expect("write source");
    compile_and_run_file(&src_path, extra_env)
}

#[test]
fn e2e_all_repository_test_programs() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let cases = [
        ("test/adt_basic.gs", "42"),
        ("test/adt_bool.gs", "1"),
        ("test/adt_either.gs", "42"),
        ("test/adt_list.gs", "3"),
        ("test/adt_maybe.gs", "42"),
        ("test/adt_multi_field.gs", "42"),
        ("test/adt_nat.gs", "5"),
        ("test/adt_nested.gs", "42"),
        ("test/adt_pair.gs", "40"),
        ("test/adt_tree.gs", "10"),
        ("test/cls.gs", "5"),
        ("test/concept_eq.gs", "42"),
        ("test/factorial.gs", "3628800"),
        ("test/fib_cls.gs", "55"),
        ("test/fibonacci.gs", "34"),
        ("test/match_literal.gs", "100"),
        (
            "test/fizzbuzz.gs",
            "1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizzBuzz\n",
        ),
    ];

    for (rel_path, expected) in cases {
        let full_path = manifest_dir.join(rel_path);
        let actual = compile_and_run_file(&full_path, &[]);
        assert_eq!(actual, expected, "Mismatch in {}", rel_path);
    }
}

#[test]
fn e2e_strings_floats_chars_negatives_and_short_circuit() {
    let src = r#"
// Test comments, strings with escapes, floats, chars, signed division, and short-circuit && / ||
def safeCheck(d: Int, n: Int): Bool =
    d != 0 && (n / d) > 2

def safeOr(d: Int, n: Int): Bool =
    d == 0 || (n / d) > 2

def main() = {
    printLn("Hello, \"Geisha\"\\World!");
    putNumber(-42);
    printLn("");
    putNumber(-17 / 5);
    printLn("");
    putNumber(-17 % 5);
    printLn("");
    putChar('G');
    putChar('\n');
    let f = 1.5 + 2.25;
    if (f > 3.7 && f < 3.8 && "abc" == "abc" && "abc" < "abd") {
        // Division by zero on RHS would crash if && or || were eager!
        if (!safeCheck(0, 100) && safeOr(0, 100)) {
            putFloat(f);
            printLn("")
        } else printLn("bad-short-circuit")
    } else printLn("bad-float-or-str")
}
"#;
    let out = compile_and_run_src(src);
    assert_eq!(out, "Hello, \"Geisha\"\\World!\n-42\n-3\n-2\nG\n3.75\n");
}

#[test]
fn e2e_multi_arg_vs_first_class_tuples_and_currying() {
    let src = r#"
type Pair a b = (a, b)

// 0-arg function
def getBase(): Int = 10

// 2-arg function: (Int, Int) -> Int
def add(x: Int, y: Int): Int = x + y

// 1-arg function taking a first-class tuple: ((Int, Int)) -> Int
def sumTuple(p: Pair Int Int): Int = match p {
    (a, b) -> a + b
}

// Curried function using right-associative `->`: Int -> Int -> Int
makeAdder: Int -> Int -> Int
def makeAdder(n) = (x) -> n + x

// Higher-order 0-arg function call
def callZero(f: () -> Int): Int = f()

// Higher-order function passing a top-level polymorphic function as a first-class value
def applyTwice(f: a -> a, x: a): a = f(f(x))
def id(x) = x

// Transitive monomorphization: generic function calling generic `id` and returning a lifted generic closure
def pairWith(x: a): b -> (a, b) = (y) -> (id(x), id(y))

def main() = {
    let localId = (x) -> x;
    let tup = (callZero(getBase), callZero(() -> 12));
    let (u, v) = tup;
    let tagLocal = (y) -> (u, y);
    let (_, okFlag) = tagLocal(localId(true));
    let (u2, fifteen) = tagLocal(localId(15));
    let add20 = makeAdder(20);
    let mkPair = pairWith(u2 - 5);
    let extra = if (okFlag) applyTwice(localId, 0) else 999;
    let res = add(u, v) + sumTuple(mkPair(fifteen)) + add20(applyTwice(id, extra));
    putNumber(res)
}
"#;
    assert_eq!(compile_and_run_src(src), "62");
}

#[test]
fn e2e_pattern_matching_out_of_order_wildcards_nested_structs_and_guards() {
    let src = r#"
data Shape {
    Origin,
    Circle { radius: Float },
    Flags(Bool, Bool),
}

// Payload sizing test: Circle holds 8-byte Float (1 field), Flags holds 2-byte Bools (2 fields)
def areaApprox(s: Shape): Int = match s {
    // Out-of-order relative to declaration (`Circle` is tag 1, `Origin` is tag 0, `_` catches `Flags`)
    Circle { radius } if (radius > 2.0) -> 30,
    Circle { radius: _ } -> 10,
    _ -> 2,
}

def nestedSum(xs: List Int): Int = match xs {
    Cons(a, Cons(b, Nil)) -> a * 10 + b,
    Cons(a, _) if (a > 100) -> 999,
    _ -> 0,
}

def main() = {
    let s1 = areaApprox(Circle { radius: 3.5 });
    let s2 = areaApprox(Circle { radius: 1.0 });
    let s3 = areaApprox(Flags(true, false));
    let n1 = nestedSum([4, 2]);
    let n2 = nestedSum([200, 1, 2]);
    putNumber(s1 + s2 + s3 + n1 + n2)
}
"#;
    // 30 + 10 + 2 + 42 + 999 = 1083
    assert_eq!(compile_and_run_src(src), "1083");
}

#[test]
fn e2e_concepts_static_monomorphization_and_constrained_instances() {
    let src = r#"
data Maybe a {
    Nothing,
    Just(a),
}

concept Eq a {
    eq: (a, a) -> Bool
}

instance Eq Int {
    def eq(x, y) = x == y
}

instance Eq String {
    def eq(x, y) = x == y
}

instance (Eq a) => Eq (Maybe a) {
    def eq(x, y) = match (x, y) {
        (Nothing, Nothing) -> true,
        (Just(a), Just(b)) -> eq(a, b),
        _ -> false,
    }
}

instance (Eq a) => Eq (List a) {
    def eq(xs, ys) = match (xs, ys) {
        (Nil, Nil) -> true,
        (Cons(x, xt), Cons(y, yt)) -> eq(x, y) && eq(xt, yt),
        _ -> false,
    }
}

def lookup(key, pairs): forall k v. (Eq k) => (k, List (k, v)) -> Maybe v =
    match pairs {
        Nil -> Nothing,
        Cons((k, v), rest) ->
            if (eq(key, k)) Just(v)
            else lookup(key, rest),
    }

def main() = {
    let l1 = [Just(1), Nothing, Just(3)];
    let l2 = [Just(1), Nothing, Just(3)];
    let table = [("a", 10), ("b", 32)];
    let found = match lookup("b", table) {
        Just(v) -> v,
        Nothing -> 0,
    };
    if (eq(l1, l2)) putNumber(10 + found) else putNumber(0)
}
"#;
    assert_eq!(compile_and_run_src(src), "42");
}

#[test]
fn e2e_escape_analysis_and_gc_small_heap_stress() {
    let src = r#"
// Non-recursive nested ADTs escaping via return (must NOT produce dangling stack pointers)
data Inner { MkInner(Int, Int) }
data Outer { MkOuter(Inner) }

def makeEscaping(x: Int): Outer =
    let i = MkInner(x, x + 1) in
    let o = MkOuter(i) in
    o

// Mutually recursive ADTs (`Even` <-> `Odd`)
data Even { EZero, SuccE(Odd) }
data Odd { SuccO(Even) }

def evenToInt(e: Even): Int = match e {
    EZero -> 0,
    SuccE(o) -> 1 + oddToInt(o),
}

def oddToInt(o: Odd): Int = match o {
    SuccO(e) -> 1 + evenToInt(e),
}

// Top-level heap global root (`globalList` must survive repeated GC sweeps!)
def globalList = [10, 20, 12]

def sumList(xs: List Int): Int = match xs {
    Nil -> 0,
    Cons(h, t) -> h + sumList(t),
}

// Churn thousands of short-lived heap list nodes to force many GC collections
def churn(iter: Int, live: List Int): Int =
    if (iter == 0) sumList(live)
    else {
        let garbage = [iter, iter + 1, iter + 2, iter + 3];
        let _ = sumList(garbage);
        churn(iter - 1, live)
    }

def main() = {
    let localLive = [1, 2, 3, 4];
    let churnSum = churn(800, localLive);
    let escapedVal = match makeEscaping(20) {
        MkOuter(MkInner(a, b)) -> a + b
    };
    let mutRec = evenToInt(SuccE(SuccO(EZero)));
    putNumber(churnSum + sumList(globalList) + escapedVal + mutRec)
}
"#;
    // Force a tiny 64-word (512-byte) initial GC heap so `churn(800, ...)` triggers dozens of collections!
    let out = compile_and_run_src_with_env(src, &[("GEISHA_GC_HEAP_WORDS", "64")]);
    // churnSum (10) + globalList (42) + escapedVal (41) + mutRec (2) = 95
    assert_eq!(out, "95");
}

#[test]
fn e2e_inline_and_multifile_modules_with_visibility() {
    // 1. Single-file inline module
    let inline_src = r#"
mod Math {
    def square(x) = x * x
    pub def hypot2(a, b) = square(a) + square(b)
}

use Math.hypot2

def main() = putNumber(hypot2(3, 4))
"#;
    assert_eq!(compile_and_run_src(inline_src), "25");

    // Private access across inline module must fail
    let bad_inline = r#"
mod Math {
    def secret(x) = x
}
def main() = putNumber(Math.secret(1))
"#;
    assert!(compile_str("bad.gs", bad_inline).is_err());

    // 2. Multi-file `--src-root` project where submodule `service` imports sibling submodule `math`
    let dir = tempfile::tempdir().unwrap();
    let src_root = dir.path().join("src");
    fs::create_dir_all(src_root.join("main")).unwrap();

    fs::write(
        src_root.join("main/math.gs"),
        r#"
def internalScale() = 2
pub def add(x, y) = x + y
pub def scaled(x) = x * internalScale()
"#,
    )
    .unwrap();

    fs::write(
        src_root.join("main/service.gs"),
        r#"
use math.{add, scaled}
pub use math.add
pub def compute(a, b) = scaled(add(a, b))
"#,
    )
    .unwrap();

    let entry = src_root.join("main/mod.gs");
    fs::write(
        &entry,
        r#"
mod math
mod service
use service.{compute, add}

pub def main() = putNumber(add(compute(10, 5), 12))
"#,
    )
    .unwrap();

    let bin = dir.path().join("proj_bin");
    let status = Command::new(miko_bin())
        .arg("--src-root")
        .arg(&src_root)
        .arg("-o")
        .arg(&bin)
        .arg(&entry)
        .output()
        .unwrap();
    assert!(
        status.status.success(),
        "project build failed: {}",
        String::from_utf8_lossy(&status.stderr)
    );
    let run = Command::new(&bin).output().unwrap();
    assert_eq!(String::from_utf8_lossy(&run.stdout), "42");
}

#[test]
fn e2e_negative_compile_time_checks() {
    // Cyclic type alias
    let cyc = "type A = B\ntype B = A\ndef main() = ()";
    assert!(matches!(
        compile_str("cyc.gs", cyc),
        Err(miko::compile::CompileError::TypeError(
            TypeError::TypeAliasError(..),
            _
        ))
    ));

    // Unimplemented concept instance
    let no_inst = r#"
concept Show a { show: a -> String }
def main() = printLn(show(42))
"#;
    assert!(matches!(
        compile_str("no_inst.gs", no_inst),
        Err(miko::compile::CompileError::TypeError(
            TypeError::NoInstance { .. },
            _
        ))
    ));

    // Non-tail expression in `{ ... }` block returning non-`Void` MUST be rejected!
    let ignored_non_void = r#"
def step(x: Int): Int = x + 1
def main() = {
    step(10);
    putNumber(0)
}
"#;
    assert!(matches!(
        compile_str("ignored.gs", ignored_non_void),
        Err(miko::compile::CompileError::TypeError(
            TypeError::MisMatch(..),
            _
        ))
    ));

    // `while` body returning non-`Void` MUST be rejected!
    let bad_while = r#"
def main() = while (false) 42
"#;
    assert!(matches!(
        compile_str("bad_while.gs", bad_while),
        Err(miko::compile::CompileError::TypeError(
            TypeError::MisMatch(..),
            _
        ))
    ));
}

#[test]
fn e2e_while_cell_mutarray_sieve_and_gc_safety() {
    let src = r#"
data Boxed { MkBox(Int, Int) }

// Generic helper swapping two polymorphic `Cell t`s
def swapCells(a: Cell t, b: Cell t): Void = {
    let tmp = cellGet(a);
    cellSet(a, cellGet(b));
    cellSet(b, tmp)
}

// Helper storing a non-recursive ADT parameter into a Cell (must trigger interprocedural escape!)
def storeBox(dst: Cell Boxed, b: Boxed): Void = cellSet(dst, b)

def makeEscapingCell(n: Int): Cell Boxed = {
    let c = cellNew(MkBox(0, 0));
    let localBox = MkBox(n, n + 2);
    storeBox(c, localBox);
    c
}

// Sieve of Eratosthenes using `while`, `Cell`, and `MutArray`
def countPrimes(limit: Int): Int = {
    let isPrime = arrayNew(limit + 1, true);
    arraySet(isPrime, 0, false);
    arraySet(isPrime, 1, false);

    let p = cellNew(2);
    while (cellGet(p) * cellGet(p) <= limit) {
        let cur = cellGet(p);
        if (arrayGet(isPrime, cur)) {
            let mult = cellNew(cur * cur);
            while (cellGet(mult) <= limit) {
                arraySet(isPrime, cellGet(mult), false);
                cellSet(mult, cellGet(mult) + cur)
            }
        } else ();
        cellSet(p, cur + 1)
    };

    let count = cellNew(0);
    let idx = cellNew(2);
    while (cellGet(idx) <= limit) {
        if (arrayGet(isPrime, cellGet(idx))) {
            cellSet(count, cellGet(count) + 1)
        } else ();
        cellSet(idx, cellGet(idx) + 1)
    };
    cellGet(count)
}

def main() = {
    // 1. Closure sharing mutable `Cell` state cleanly via immutable value capture
    let counter = cellNew(0);
    let bump = (delta: Int) -> {
        cellSet(counter, cellGet(counter) + delta);
        cellGet(counter)
    };
    let _ = bump(4);
    let cval = bump(6); // 10

    // 2. Generic cell swap
    let c1 = cellNew(1);
    let c2 = cellNew(2);
    swapCells(c1, c2); // c1 = 2, c2 = 1

    // 3. `MutArray` holding heap-allocated ADTs under small-heap GC churn!
    let boxes = arrayNew(4, MkBox(0, 0));
    let k = cellNew(0);
    while (cellGet(k) < arrayLen(boxes)) {
        let idx = cellGet(k);
        arraySet(boxes, idx, MkBox(idx * 10, 1));
        cellSet(k, idx + 1)
    };
    // Trigger repeated GC collections while `boxes` and `escapedCell` hold heap ADTs
    let escapedCell = makeEscapingCell(5);
    let primes100 = countPrimes(100); // 25 primes <= 100
    let boxSum = match (arrayGet(boxes, 3), cellGet(escapedCell)) {
        (MkBox(a, b), MkBox(x, y)) -> a + b + x + y
    }; // (30 + 1) + (5 + 7) = 43

    // 10 + 2 + 25 + 43 = 80
    putNumber(cval + cellGet(c1) + primes100 + boxSum)
}
"#;
    // Run with tiny 64-word initial GC heap to stress GC scanning of `Cell` and `MutArray`!
    let out = compile_and_run_src_with_env(src, &[("GEISHA_GC_HEAP_WORDS", "64")]);
    assert_eq!(out, "80");

    // Verify `MutArray` bounds checking catches out-of-bounds index cleanly (aborts non-zero instead of silent memory corruption)
    let oob_src = r#"
def main() = {
    let arr = arrayNew(3, 10);
    putNumber(arrayGet(arr, 3))
}
"#;
    let dir = tempfile::tempdir().unwrap();
    let src_path = dir.path().join("oob.gs");
    let bin_path = dir.path().join("oob_bin");
    fs::write(&src_path, oob_src).unwrap();
    let comp = Command::new(miko_bin())
        .arg("-o")
        .arg(&bin_path)
        .arg(&src_path)
        .output()
        .unwrap();
    assert!(comp.status.success());
    let run = Command::new(&bin_path).output().unwrap();
    assert!(!run.status.success());
    assert!(String::from_utf8_lossy(&run.stderr).contains("out of bounds"));
}

#[test]
fn e2e_extern_c_ffi_stdlib_syslib_and_mini_compiler_pipeline() {
    let dir = tempfile::tempdir().unwrap();
    let input_gs = dir.path().join("sample_input.gs");
    let output_ir = dir.path().join("emitted_out.ll");
    let compiler_src = dir.path().join("mini_driver.gs");
    let compiler_bin = dir.path().join("mini_driver_bin");

    fs::write(&input_gs, "def answer = 42 + answer\n").unwrap();

    // Mini compiler driver exercising:
    // - Direct `extern` C FFI (`abs`) + first-class `extern` eta-wrapping
    // - `sys.env` (`args`, `getEnv`)
    // - `sys.fs` (`readFile`, `writeFile`, `fileExists`, `removeFile`)
    // - `sys.io` (`print`)
    // - `std.char` + `std.str` + `std.buf` + `std.vec` + `std.map.{Interner, StrMap, IntMap}` + `std.list`
    let driver_code = r#"
use std.option.{Option, Some, None, unwrapOr}
use std.result.{Result, Ok, Err}
use std.list.{listLen, listNth}
use std.char.{isIdentStartByte, isIdentContByte, isDigitByte, isSpaceByte}
use std.buf.{bufNew, bufPushStr, bufPushInt, bufPushLine, bufToString}
use std.str.{intToStr, strTrim, strSplitChar, strJoin}
use std.vec.{Vec, vecNew, vecLen, vecPush, vecGetOr}
use std.map.{
    Interner, internerNew, intern, internerResolve, internerCount,
    strMapNew, strMapInsert, strMapGet,
    intMapNew, intMapInsert, intMapGet
}
use sys.io.print
use sys.fs.{readFile, writeFile, fileExists}
use sys.env.{args, getEnv}

extern "abs" def c_abs(n: Int): Int

data Token {
    TokIdent(Int),
    TokInt(Int),
    TokSym(Int),
}

def applyIntFn(f: Int -> Int, x: Int): Int = f(x)

def tokenize(src: String, pool: std.map.Interner): Vec Token = {
    let toks = vecNew();
    let n = strLen(src);
    let i = cellNew(0);
    while (cellGet(i) < n) {
        let pos = cellGet(i);
        let b = strByteAt(src, pos);
        if (isSpaceByte(b)) {
            cellSet(i, pos + 1)
        } else if (isIdentStartByte(b)) {
            let end = cellNew(pos + 1);
            while (cellGet(end) < n && isIdentContByte(strByteAt(src, cellGet(end)))) {
                cellSet(end, cellGet(end) + 1)
            };
            let word = strSlice(src, pos, cellGet(end));
            let symId = intern(pool, word);
            vecPush(toks, TokIdent(symId));
            cellSet(i, cellGet(end))
        } else if (isDigitByte(b)) {
            let val = cellNew(0);
            let end = cellNew(pos);
            while (cellGet(end) < n && isDigitByte(strByteAt(src, cellGet(end)))) {
                let d = strByteAt(src, cellGet(end)) - 48;
                cellSet(val, cellGet(val) * 10 + d);
                cellSet(end, cellGet(end) + 1)
            };
            vecPush(toks, TokInt(cellGet(val)));
            cellSet(i, cellGet(end))
        } else {
            vecPush(toks, TokSym(b));
            cellSet(i, pos + 1)
        }
    };
    toks
}

def main() = {
    let cli = args();
    let inPath = unwrapOr(listNth(cli, 1), "");
    let outPath = unwrapOr(listNth(cli, 2), "");
    let tagEnv = unwrapOr(getEnv("GEISHA_TEST_TAG"), "missing");

    // Test both direct extern call and higher-order extern function value
    let absCheck = c_abs(-7) + applyIntFn(c_abs, -35); // 42

    // Stress StrMap rehashing (> 20 keys) and IntMap lookup
    let sm = strMapNew();
    let k = cellNew(0);
    while (cellGet(k) < 25) {
        let cur = cellGet(k);
        strMapInsert(sm, strConcat("key_", intToStr(cur)), cur * 2);
        cellSet(k, cur + 1)
    };
    let mapVal = unwrapOr(strMapGet(sm, "key_21"), 0); // 42

    let im = intMapNew();
    intMapInsert(im, 100, "ok");
    let imVal = unwrapOr(intMapGet(im, 100), "bad");

    let joined = strJoin(strSplitChar(strTrim("  alpha,beta,gamma  "), ','), "|");

    match readFile(inPath) {
        Err(msg) -> print(msg),
        Ok(src) -> {
            let pool = internerNew();
            let toks = tokenize(src, pool);
            // Expect tokens: `def`(id 0), `answer`(id 1), `=`, `42`, `+`, `answer`(reused id 1!)
            // Unique interned symbols == 2 (`def`, `answer`), total tokens == 6!
            let outBuf = bufNew();
            bufPushLine(outBuf, strConcat("; tag=", tagEnv));
            bufPushStr(outBuf, "tokens=");
            bufPushInt(outBuf, vecLen(toks));
            bufPushStr(outBuf, ",symbols=");
            bufPushInt(outBuf, internerCount(pool));
            bufPushStr(outBuf, ",sym1=");
            bufPushStr(outBuf, internerResolve(pool, 1));
            bufPushStr(outBuf, ",check=");
            bufPushInt(outBuf, absCheck + mapVal + listLen(cli));
            bufPushStr(outBuf, ",");
            bufPushStr(outBuf, imVal);
            bufPushStr(outBuf, ",");
            bufPushStr(outBuf, joined);

            match writeFile(outPath, bufToString(outBuf)) {
                Err(e) -> print(e),
                Ok(_) -> if (fileExists(outPath)) print("DONE") else print("MISSING"),
            }
        },
    }
}
"#;

    fs::write(&compiler_src, driver_code).unwrap();
    let comp = Command::new(miko_bin())
        .arg("-o")
        .arg(&compiler_bin)
        .arg(&compiler_src)
        .output()
        .unwrap();
    assert!(
        comp.status.success(),
        "failed compiling mini_driver.gs:\nstderr: {}",
        String::from_utf8_lossy(&comp.stderr)
    );

    let run = Command::new(&compiler_bin)
        .arg(&input_gs)
        .arg(&output_ir)
        .env("GEISHA_TEST_TAG", "stage1")
        .env("GEISHA_GC_HEAP_WORDS", "128")
        .output()
        .unwrap();
    assert!(
        run.status.success(),
        "mini_driver_bin crashed:\nstderr: {}",
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "DONE");

    let emitted = fs::read_to_string(&output_ir).unwrap();
    assert_eq!(
        emitted,
        "; tag=stage1\ntokens=6,symbols=2,sym1=answer,check=87,ok,alpha|beta|gamma"
    );
}

#[test]
fn e2e_concept_operator_overloading_and_scala_custom_operators() {
    let src = r#"
use std.list.listFold

data Vec2 {
    MkVec2(Int, Int),
}

// Layer 1: Overload built-in `+`, `-`, unary `-`, `==`/`!=`, and `<`/`<=`/`>`/`>=` via open Concepts!
instance Add Vec2 {
    def (+)(a, b) = match (a, b) {
        (MkVec2(x1, y1), MkVec2(x2, y2)) -> MkVec2(x1 + x2, y1 + y2),
    }
}

instance Sub Vec2 {
    def (-)(a, b) = match (a, b) {
        (MkVec2(x1, y1), MkVec2(x2, y2)) -> MkVec2(x1 - x2, y1 - y2),
    }
}

instance Neg Vec2 {
    def neg(v) = match v {
        MkVec2(x, y) -> MkVec2(-x, -y),
    }
}

instance Eq Vec2 {
    def (==)(a, b) = match (a, b) {
        (MkVec2(x1, y1), MkVec2(x2, y2)) -> x1 == x2 && y1 == y2,
    }
}

instance Ord Vec2 {
    def compare(a, b) = match (a, b) {
        (MkVec2(x1, y1), MkVec2(x2, y2)) -> (x1 * x1 + y1 * y1) - (x2 * x2 + y2 * y2),
    }
}

// Layer 2a: Custom symbolic operator in a Concept (`<+>`) with static monomorphization!
concept Semigroup a {
    (<+>): (a, a) -> a
}

instance Semigroup Int {
    def (<+>)(x, y) = x + y
}

instance Semigroup String {
    def (<+>)(x, y) = x + y
}

def combineThree(a, b, c): forall t. (Semigroup t) => (t, t, t) -> t =
    a <+> b <+> c

// Layer 2b: Custom top-level operators testing Scala precedence & `:`-suffix right-associativity!
// `|>` starts with `|` (lowest precedence, left-associative): `1 + 2 |> f |> g` == `g(f(1 + 2))`
def (|>)(x: a, f: a -> b): b = f(x)

// `::` ends with `:` -> RIGHT-ASSOCIATIVE: `1 :: 2 :: 3 :: Nil` == `Cons(1, Cons(2, Cons(3, Nil)))`
def (::)(head: a, tail: List a): List a = Cons(head, tail)

// `+:` starts with `+` and ends with `:` -> additive precedence AND right-associative!
def (+:)(head: Int, tail: List Int): List Int = Cons(head, tail)

def main() = {
    let v1 = MkVec2(3, 4);
    let v2 = MkVec2(1, 2);
    let v3 = v1 + v2 - (-MkVec2(6, 4)); // (4, 6) - (-6, -4) = (10, 10)

    // Operator section `(+)` passed as a first-class function to `listFold` on custom `Vec2` AND `Int`!
    let foldedVec = listFold([v1, v2], MkVec2(0, 0), (+)); // (4, 6)
    let nums = 10 :: 20 :: 12 :: Nil;
    let nums2 = 1 + 1 +: 3 +: Nil; // `(1 + 1) +: (3 +: Nil)` -> [2, 3]
    let sumNums = listFold(nums, 0, (+)) + listFold(nums2, 0, (<+>)); // 42 + 5 = 47

    // Pipeline `|>` has lower precedence than `+` and `==`!
    let piped = 10 + 5 |> (n) -> n * 2 |> (n) -> n - 10; // 20

    // Built-in `String + String` AND custom `Semigroup String` `<+>`
    let msg = combineThree("Gei", "sha", "!") + "";

    if (v3 == MkVec2(10, 10) && v1 != v2 && v2 < v1 && foldedVec == MkVec2(4, 6) && msg == "Geisha!") {
        putNumber(sumNums + piped) // 47 + 20 = 67
    } else {
        putNumber(0)
    }
}
"#;
    assert_eq!(compile_and_run_src(src), "67");
}

#[test]
fn e2e_concurrency_tasks_work_helping_wait_any_and_par_combinators() {
    let src = r#"
use std.list.{listLen, listFold}
use std.sync.{atomicNew, atomicAdd, atomicLoad, atomicCas}
use std.task.{spawn, await, par, parMap, parFor, parRange, waitAll, waitAny, sleepMs}

// Recursive nested fork-join: exercises inline work-helping on a 2-thread pool without deadlocking!
def fibPar(n: Int): Int =
    if (n <= 1) n
    else if (n <= 5) fibPar(n - 1) + fibPar(n - 2)
    else {
        let (a, b) = par(() -> fibPar(n - 1), () -> fibPar(n - 2));
        a + b
    }

def main() = {
    // 1. Nested recursive `par` under tight worker thread count (`GEISHA_THREADS=2`)
    let f11 = fibPar(11); // 89

    // 2. `parMap` & `parRange` preserving deterministic order over immutable values
    let mapped = parMap([1, 2, 3, 4, 5, 6], (x) -> x * x); // sum = 91
    let sumMapped = listFold(mapped, 0, (+));

    let ranged = parRange(1, 21, (i) -> i * 2); // 2 * (1..=20) = 420
    let sumRanged = listFold(ranged, 0, (+));

    // 3. `parFor` + `AtomicInt` (`atomicAdd` & `atomicCas`)
    let acc = atomicNew(0);
    parFor(0, 100, (i) -> {
        let _ = atomicAdd(acc, i);
        ()
    }); // 0..99 sum = 4950
    let casOk = atomicCas(acc, 4950, 5000) && !atomicCas(acc, 4950, 9999);

    // 4. `waitAny`: 2 fast tasks + 1 slow task (`sleepMs(50)`)
    let slow = spawn(() -> { sleepMs(50); 100 });
    let fast1 = spawn(() -> 10);
    let fast2 = spawn(() -> 20);
    let (ready, pending) = waitAny([slow, fast1, fast2], 2, -1);
    let readyCountOk = listLen(ready) >= 2 && (listLen(ready) + listLen(pending) == 3);
    let totalWaitAny = listFold(waitAll(ready), 0, (+)) + listFold(waitAll(pending), 0, (+)); // 130

    if (f11 == 89 && sumMapped == 91 && sumRanged == 420 && casOk && atomicLoad(acc) == 5000 && readyCountOk && totalWaitAny == 130) {
        printLn("CONC_TASK_OK")
    } else {
        printLn("CONC_TASK_FAIL")
    }
}
"#;
    let out = compile_and_run_src_with_env(src, &[("GEISHA_THREADS", "2")]);
    assert_eq!(out, "CONC_TASK_OK\n");
}

#[test]
fn e2e_concurrency_channels_mvars_actors_and_multicore_gc_stress() {
    let src = r#"
use std.option.{Option, Some, None, unwrapOr}
use std.list.{listFold}
use std.vec.{Vec, vecNew, vecPush, vecLen}
use std.map.{StrMap, strMapNew, strMapInsert, strMapGet}
use std.str.{intToStr}
use std.sync.{
    mvarEmpty, mvarPut, mvarTake, mvarRead, mvarTryTake,
    chanNew, chanSend, chanRecv, chanClose
}
use std.task.{spawn, await, waitAll}
use std.actor.{Actor, actorSpawn, actorCall, actorAsk, actorSend, actorStop}

data Packet {
    MkPacket(Int, String),
}

// Actor state combines `!Send` types (`Vec`, `StrMap`, `Cell`) safely on a pinned OS thread!
data StoreState {
    MkStore(Vec String, StrMap Int, Cell Int),
}

def drainSum(ch: Chan Packet, acc: Int): Int = match chanRecv(ch) {
    None -> acc,
    Some(MkPacket(v, label)) -> drainSum(ch, acc + v + strLen(label)),
}

def main() = {
    // 1. Bounded channel (`cap = 2`) with multiple concurrent producers & heap allocations under tiny GC heap
    let ch: Chan Packet = chanNew(2);
    let p1 = spawn(() -> {
        let i = cellNew(1);
        while (cellGet(i) <= 15) {
            let k = cellGet(i);
            let _ = chanSend(ch, MkPacket(k, "ab" + "cd")); // strLen = 4
            cellSet(i, k + 1)
        }
    });
    let p2 = spawn(() -> {
        let i = cellNew(16);
        while (cellGet(i) <= 30) {
            let k = cellGet(i);
            let _ = chanSend(ch, MkPacket(k, "xy" + "zw")); // strLen = 4
            cellSet(i, k + 1)
        }
    });
    let closer = spawn(() -> {
        await(p1);
        await(p2);
        chanClose(ch)
    });
    // Sum of 1..=30 is 465; plus 30 * 4 = 120 -> 585
    let chanTotal = drainSum(ch, 0);
    await(closer);

    // 2. `MVar` synchronization (empty -> put -> read -> take -> tryTake)
    let mv = mvarEmpty();
    let putter = spawn(() -> mvarPut(mv, ("token-" + intToStr(7), 77)));
    let peeked = mvarRead(mv);
    let taken = mvarTake(mv);
    let afterEmpty = mvarTryTake(mv);
    await(putter);
    let mvarOk = match (peeked, taken, afterEmpty) {
        ((s1, 77), (s2, 77), None) -> s1 == "token-7" && s2 == "token-7",
        _ -> false,
    };

    // 3. `Actor` encapsulating `!Send` `Vec`, `StrMap`, and `Cell`, and passing `Actor` handles across tasks!
    let store: Actor StoreState = actorSpawn(() -> MkStore(vecNew(), strMapNew(), cellNew(0)));

    let w1 = spawn(() -> {
        let i = cellNew(0);
        while (cellGet(i) < 10) {
            let idx = cellGet(i);
            let key = "k" + intToStr(idx);
            actorSend(store, (st) -> match st {
                MkStore(log, mp, cnt) -> {
                    vecPush(log, key);
                    strMapInsert(mp, key, idx * 10);
                    cellSet(cnt, cellGet(cnt) + 1)
                },
            });
            cellSet(i, idx + 1)
        }
    });
    let w2 = spawn(() -> {
        let i = cellNew(10);
        while (cellGet(i) < 20) {
            let idx = cellGet(i);
            let key = "k" + intToStr(idx);
            actorSend(store, (st) -> match st {
                MkStore(log, mp, cnt) -> {
                    vecPush(log, key);
                    strMapInsert(mp, key, idx * 10);
                    cellSet(cnt, cellGet(cnt) + 1)
                },
            });
            cellSet(i, idx + 1)
        }
    });
    await(w1);
    await(w2);

    // Non-blocking `actorCall` returns `Task r` directly composable with `waitAll`!
    let q1 = actorCall(store, (st) -> match st {
        MkStore(log, _, cnt) -> vecLen(log) + cellGet(cnt),
    });
    let q2 = actorCall(store, (st) -> match st {
        MkStore(_, mp, _) -> unwrapOr(strMapGet(mp, "k5"), 0) + unwrapOr(strMapGet(mp, "k15"), 0),
    });
    let querySum = listFold(waitAll([q1, q2]), 0, (+)); // (20 + 20) + (50 + 150) = 240
    let finalSnap = actorAsk(store, (st) -> match st {
        MkStore(_, mp, _) -> unwrapOr(strMapGet(mp, "k19"), 0),
    }); // 190
    actorStop(store);

    if (chanTotal == 585 && mvarOk && querySum == 240 && finalSnap == 190) {
        putNumber(chanTotal + querySum + finalSnap) // 585 + 240 + 190 = 1015
    } else {
        putNumber(0)
    }
}
"#;
    // Force frequent multicore stop-the-world GC cycles with a tiny 128-word initial heap!
    let out = compile_and_run_src_with_env(
        src,
        &[("GEISHA_GC_HEAP_WORDS", "128"), ("GEISHA_THREADS", "4")],
    );
    assert_eq!(out, "1015");
}

#[test]
fn e2e_concurrency_send_compile_time_safety_rejections() {
    let unsafe_programs = [
        // 1. Returning `!Send` `Cell` from `spawn`
        r#"
use std.task.spawn
def main() = {
    let _ = spawn(() -> cellNew(1));
    ()
}
"#,
        // 2. Sending `!Send` `MutArray` over a `Chan`
        r#"
use std.sync.{chanNew, chanSend}
def main() = {
    let ch = chanNew(1);
    let _ = chanSend(ch, arrayNew(4, 0));
    ()
}
"#,
        // 3. Storing `!Send` raw `Ptr` in an `MVar`
        r#"
use std.sync.mvarNew
def main() = {
    let _ = mvarNew(ptrNull());
    ()
}
"#,
        // 4. Sending ADT containing `!Send` container (`Vec Int`) across `spawn`
        r#"
use std.task.spawn
use std.vec.vecNew
def main() = {
    let _ = spawn(() -> vecNew());
    ()
}
"#,
        // 5. Capturing parent's `Cell` inside `spawn` closure (even though return type `Int` IS `Send`!)
        r#"
use std.task.spawn
def main() = {
    let c = cellNew(0);
    let _ = spawn(() -> { cellSet(c, 1); 42 });
    ()
}
"#,
        // 6. Capturing parent's `Cell` through higher-order `parMap`
        r#"
use std.task.parMap
def main() = {
    let c = cellNew(0);
    let _ = parMap([1, 2, 3], (x) -> { cellSet(c, x); x + 1 });
    ()
}
"#,
        // 7. Smuggling parent's `!Send` `Vec` into `actorSpawn` initializer closure
        r#"
use std.actor.actorSpawn
use std.vec.{vecNew, vecPush}
def main() = {
    let v = vecNew();
    vecPush(v, 1);
    let _ = actorSpawn(() -> v);
    ()
}
"#,
        // 8. Accessing a `!Send` top-level global `Cell` inside a `spawn`ed task
        r#"
use std.task.spawn
def globalCell = cellNew(0)
def main() = {
    let _ = spawn(() -> cellGet(globalCell));
    ()
}
"#,
    ];

    for (idx, src) in unsafe_programs.iter().enumerate() {
        let err = compile_str("unsafe_conc.gs", src)
            .expect_err(&format!("Case #{} should have failed Send check!", idx + 1));
        let msg = err.to_string();
        assert!(
            msg.contains("Send"),
            "Case #{} expected `Send` type error, got: {}",
            idx + 1,
            msg
        );
    }
}
