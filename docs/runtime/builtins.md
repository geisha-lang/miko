# Built-in Functions & Operators

## Standard I/O Functions (`src/lib/base.c`)

- **`printLn : String -> Void`** — Prints a UTF-8 string followed by a newline.
- **`putNumber : Int -> Void`** — Prints a signed 32-bit integer (`%d`).
- **`putFloat : Float -> Void`** — Prints a 64-bit floating-point number (`%g` / `%f`).
- **`putChar : Char -> Void`** — Prints a single character.

## Runtime String Helpers

- **`gs_str_eq(a, b)`** / **`gs_str_cmp(a, b)`** — Used by `==`, `!=`, `<`, `<=`, `>`, `>=` on `String` operands for content comparison.

## Mutable Container Primitives

- **`cellNew : forall a. a -> Cell a`** — Allocates a single-element mutable cell initialized with the given value.
- **`cellGet : forall a. Cell a -> a`** — Reads the current value of a `Cell a`.
- **`cellSet : forall a. (Cell a, a) -> Void`** — Overwrites the value stored in a `Cell a`.
- **`arrayNew : forall a. (Int, a) -> MutArray a`** — Allocates a fixed-length mutable array of `n >= 0` elements initialized to the given value.
- **`arrayLen : forall a. MutArray a -> Int`** — Returns the length of a `MutArray a`.
- **`arrayGet : forall a. (MutArray a, Int) -> a`** — Bounds-checked read at `0 <= idx < len`.
- **`arraySet : forall a. (MutArray a, Int, a) -> Void`** — Bounds-checked write at `0 <= idx < len`.

## Low-Level `PrimOp` Intrinsics (Used by `stdlib` & `syslib`)

- **Bitwise & Conversions**: `bitAnd`, `bitOr`, `bitXor`, `bitShl`, `bitShr` (`(Int, Int) -> Int`), `charToInt` (`Char -> Int`), `intToChar` (`Int -> Char`), `intToI64` (`Int -> Int64`), `i64ToInt` (`Int64 -> Int`).
- **Strings**: `strLen` (`String -> Int`), `strByteAt` (`(String, Int) -> Int`), `strSlice` (`(String, Int, Int) -> String`), `strConcat` (`(String, String) -> String`), `strFromBytes` (`(MutArray Int, Int) -> String`).
- **Opaque Pointers & CLI**: `Ptr`, `ptrNull` (`() -> Ptr`), `ptrIsNull` (`Ptr -> Bool`), `ptrAllocBytes` (`Int -> Ptr`), `ptrByteAt` (`(Ptr, Int) -> Int`), `ptrSetByte` (`(Ptr, Int, Int) -> Void`), `ptrToString` (`Ptr -> String`), `sysArgc` (`() -> Int`), `sysArgv` (`Int -> String`).

## Embedded Standard (`std.*`) & System (`sys.*`) Libraries

- **`std.option`** — `Option a { None, Some(a) }`, `isSome`, `isNone`, `unwrapOr`, `mapOption`, `andThenOption`
- **`std.result`** — `Result a e { Ok(a), Err(e) }`, `isOk`, `isErr`, `unwrapResultOr`, `mapResult`, `mapErr`, `andThenResult`
- **`std.list`** — `listLen`, `listIsEmpty`, `listRev`, `listAppend`, `listMap`, `listFilter`, `listFold`, `listAny`, `listAll`, `listNth`
- **`std.char`** — Lexer character/byte predicates (`isDigitByte`, `isAlphaByte`, `isIdentStartByte`, `isIdentContByte`, `isSpaceByte`, etc.)
- **`std.buf`** — Amortized $O(1)$ `StringBuf` (`bufNew`, `bufPushByte`, `bufPushChar`, `bufPushStr`, `bufPushLine`, `bufPushInt`, `bufToString`)
- **`std.str`** — `intToStr`, `boolToStr`, `strStartsWith`, `strEndsWith`, `strIndexOfByte`, `strTrim`, `strSplitChar`, `strJoin`, `strHash`
- **`std.vec`** — Dynamic growable `Vec a` (`vecNew`, `vecLen`, `vecPush`, `vecPop`, `vecGet`, `vecGetOr`, `vecSet`, `vecClear`, `vecToList`)
- **`std.map`** — Resizable `StrMap v`, `IntMap v`, and bidirectional compiler symbol `Interner` (`internerNew`, `intern`, `internerResolve`, `internerCount`)
- **`std.sync`** — Shared-memory synchronization (`AtomicInt`, `MVar a`, bounded/unbounded `Chan a`)
- **`std.task`** — Native multicore futures & data parallelism (`Task a`, `spawn`, `await`, `isReady`, `waitAll`, `waitAny`, `par`, `parMap`, `parFor`, `numCpu`, `sleepMs`)
- **`std.actor`** — Thread-pinned sequential actors encapsulating `!Send` state (`Actor s`, `actorSpawn`, `actorCall`, `actorAsk`, `actorSend`, `actorStop`)
- **`sys.io`** — Stream output (`print`, `eprint`, `eprintLn` via POSIX `write`)
- **`sys.fs`** — File I/O (`fileExists`, `readFile`, `writeFile`, `removeFile` via ANSI C / POSIX FFI)
- **`sys.env`** — CLI & environment (`argCount`, `argAt`, `args`, `getEnv`)
- **`sys.proc`** — Process control (`exit`, `shell`)

## Built-in Prelude Types & Operators

- Primitive types: `Int`, `Float`, `Char`, `String`, `Bool`, `Void` (`()`)
- Standard `List a { Nil, Cons(a, List a) }` (available by default for `[e1, e2, ...]` list literals unless overridden by a user `data List` declaration)
- Arithmetic: `+`, `-`, `*`, `/` (`Num`: `Int`, `Float`), `%` (`Int`), unary `-` (`Num`)
- Comparison: `==`, `!=` (`EqOp`: `Int`, `Float`, `Bool`, `Char`, `String`), `<`, `<=`, `>`, `>=` (`OrdOp`: `Int`, `Float`, `Char`, `String`)
- Logical: `!` (`Bool -> Bool`), short-circuit `&&` and `||` (`(Bool, Bool) -> Bool`)
