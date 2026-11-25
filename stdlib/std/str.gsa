// String manipulation, numeric parsing/formatting, and FNV-1a hashing (`std.str`)

use std.option.{Option, Some, None}
use std.result.{Result, Ok, Err}
use std.char.{isSpaceByte, isDigitByte}
use std.list.listRev
use std.buf.{bufNew, bufPushByte, bufPushStr, bufPushInt, bufToString}

extern "strtod" def c_strtod(s: String, endptr: Ptr): Float

pub def strIsEmpty(s: String): Bool = strLen(s) == 0

pub def intToStr(n: Int): String = {
    let b = bufNew();
    bufPushInt(b, n);
    bufToString(b)
}

pub def i64ToStr(n: Int64): String = {
    let zero = intToI64(0);
    let minI64 = intToI64(1) << 63;
    if (n == zero) "0"
    else if (n == minI64) "-9223372036854775808"
    else {
        let b = bufNew();
        let ten = intToI64(10);
        let v = cellNew(if (n < zero) { bufPushByte(b, 45); -n } else n);
        let digits = arrayNew(24, 0);
        let dlen = cellNew(0);
        while (cellGet(v) > zero) {
            let cur = cellGet(v);
            let idx = cellGet(dlen);
            arraySet(digits, idx, 48 + i64ToInt(cur % ten));
            cellSet(dlen, idx + 1);
            cellSet(v, cur / ten)
        };
        let k = cellNew(cellGet(dlen) - 1);
        while (cellGet(k) >= 0) {
            let idx = cellGet(k);
            bufPushByte(b, arrayGet(digits, idx));
            cellSet(k, idx - 1)
        };
        bufToString(b)
    }
}

def hexDigitByte(nib: Int): Int =
    if (nib < 10) 48 + nib else 55 + nib

/// Lossless 16-digit IEEE-754 / 64-bit hex representation (`0x0123456789ABCDEF`), ideal for LLVM IR `double` constants.
pub def i64ToHex16(bits: Int64): String = {
    let b = bufNew();
    bufPushStr(b, "0x");
    let mask = intToI64(15);
    let shift = cellNew(60);
    while (cellGet(shift) >= 0) {
        let s = cellGet(shift);
        let nib = i64ToInt((bits >>> s) & mask);
        bufPushByte(b, hexDigitByte(nib));
        cellSet(shift, s - 4)
    };
    bufToString(b)
}

pub def floatToHex16(f: Float): String = i64ToHex16(floatToBits64(f))

pub def floatToStr(f: Float): String = {
    let bits = floatToBits64(f);
    let expMask = (intToI64(2047)) << 52;
    let fracMask = (intToI64(1) << 52) - intToI64(1);
    if ((bits & expMask) == expMask) {
        if ((bits & fracMask) != intToI64(0)) "NaN"
        else if (bits < intToI64(0)) "-inf"
        else "inf"
    } else {
        let b = bufNew();
        let val = cellNew(f);
        if (floatToBits64(cellGet(val)) < intToI64(0)) {
            bufPushByte(b, 45);
            cellSet(val, -cellGet(val))
        } else ();
        let v = cellGet(val);
        let whole = floatToInt(v);
        bufPushInt(b, whole);
        bufPushByte(b, 46);
        let frac = cellNew(v - intToFloat(whole));
        let i = cellNew(0);
        let digits = arrayNew(6, 0);
        while (cellGet(i) < 6) {
            let scaled = cellGet(frac) * 10.0;
            let d = floatToInt(scaled);
            let clamped = if (d < 0) 0 else if (d > 9) 9 else d;
            arraySet(digits, cellGet(i), clamped);
            cellSet(frac, scaled - intToFloat(clamped));
            cellSet(i, cellGet(i) + 1)
        };
        let lastNonZero = cellNew(5);
        while (cellGet(lastNonZero) > 0 && arrayGet(digits, cellGet(lastNonZero)) == 0) {
            cellSet(lastNonZero, cellGet(lastNonZero) - 1)
        };
        let k = cellNew(0);
        while (cellGet(k) <= cellGet(lastNonZero)) {
            bufPushByte(b, 48 + arrayGet(digits, cellGet(k)));
            cellSet(k, cellGet(k) + 1)
        };
        bufToString(b)
    }
}

pub def boolToStr(v: Bool): String = if (v) "true" else "false"

pub def strStartsWith(s: String, prefix: String): Bool = {
    let sl = strLen(s);
    let pl = strLen(prefix);
    if (pl > sl) false
    else strSlice(s, 0, pl) == prefix
}

pub def strEndsWith(s: String, suffix: String): Bool = {
    let sl = strLen(s);
    let xl = strLen(suffix);
    if (xl > sl) false
    else strSlice(s, sl - xl, sl) == suffix
}

pub def strIndexOfByte(s: String, targetByte: Int, startIdx: Int): Option Int = {
    let len = strLen(s);
    let i = cellNew(if (startIdx < 0) 0 else startIdx);
    let found = cellNew(-1);
    while (cellGet(i) < len && cellGet(found) < 0) {
        let cur = cellGet(i);
        if (strByteAt(s, cur) == targetByte) {
            cellSet(found, cur)
        } else {
            cellSet(i, cur + 1)
        }
    };
    if (cellGet(found) >= 0) Some(cellGet(found)) else None
}

pub def strTrim(s: String): String = {
    let len = strLen(s);
    let lo = cellNew(0);
    while (cellGet(lo) < len && isSpaceByte(strByteAt(s, cellGet(lo)))) {
        cellSet(lo, cellGet(lo) + 1)
    };
    let hi = cellNew(len);
    while (cellGet(hi) > cellGet(lo) && isSpaceByte(strByteAt(s, cellGet(hi) - 1))) {
        cellSet(hi, cellGet(hi) - 1)
    };
    strSlice(s, cellGet(lo), cellGet(hi))
}

pub def parseInt64(raw: String): Result Int64 String = {
    let s = strTrim(raw);
    let len = strLen(s);
    if (len == 0) Err("empty integer string")
    else {
        let first = strByteAt(s, 0);
        let neg = first == 45;
        let start = if (first == 45 || first == 43) 1 else 0;
        if (start >= len) Err("invalid integer: " + raw)
        else {
            let acc = cellNew(intToI64(0));
            let i = cellNew(start);
            let ok = cellNew(true);
            let ten = intToI64(10);
            while (cellGet(i) < len && cellGet(ok)) {
                let b = strByteAt(s, cellGet(i));
                if (!isDigitByte(b)) {
                    cellSet(ok, false)
                } else {
                    let digit = intToI64(b - 48);
                    let next = if (neg) cellGet(acc) * ten - digit else cellGet(acc) * ten + digit;
                    cellSet(acc, next);
                    cellSet(i, cellGet(i) + 1)
                }
            };
            if (cellGet(ok)) Ok(cellGet(acc))
            else Err("invalid integer: " + raw)
        }
    }
}

pub def parseInt(raw: String): Result Int String = match parseInt64(raw) {
    Err(e) -> Err(e),
    Ok(v64) -> {
        let min32 = intToI64(-2147483647 - 1);
        let max32 = intToI64(2147483647);
        if (v64 < min32 || v64 > max32) Err("integer overflow: " + raw)
        else Ok(i64ToInt(v64))
    },
}

pub def parseFloat(raw: String): Result Float String = {
    let s = strTrim(raw);
    let len = strLen(s);
    if (len == 0) Err("empty float string")
    else {
        let i = cellNew(0);
        let b0 = strByteAt(s, 0);
        if (b0 == 45 || b0 == 43) cellSet(i, 1) else ();
        let intDigits = cellNew(0);
        while (cellGet(i) < len && isDigitByte(strByteAt(s, cellGet(i)))) {
            cellSet(intDigits, cellGet(intDigits) + 1);
            cellSet(i, cellGet(i) + 1)
        };
        let fracDigits = cellNew(0);
        let hasDot = if (cellGet(i) < len && strByteAt(s, cellGet(i)) == 46) {
            cellSet(i, cellGet(i) + 1);
            while (cellGet(i) < len && isDigitByte(strByteAt(s, cellGet(i)))) {
                cellSet(fracDigits, cellGet(fracDigits) + 1);
                cellSet(i, cellGet(i) + 1)
            };
            true
        } else false;
        let expOk = if (cellGet(i) < len && (strByteAt(s, cellGet(i)) == 101 || strByteAt(s, cellGet(i)) == 69)) {
            cellSet(i, cellGet(i) + 1);
            if (cellGet(i) < len && (strByteAt(s, cellGet(i)) == 45 || strByteAt(s, cellGet(i)) == 43)) {
                cellSet(i, cellGet(i) + 1)
            } else ();
            let ed = cellNew(0);
            while (cellGet(i) < len && isDigitByte(strByteAt(s, cellGet(i)))) {
                cellSet(ed, cellGet(ed) + 1);
                cellSet(i, cellGet(i) + 1)
            };
            cellGet(ed) > 0
        } else true;

        let valid =
            cellGet(i) == len
            && (cellGet(intDigits) > 0 || (hasDot && cellGet(fracDigits) > 0))
            && (!hasDot || cellGet(fracDigits) > 0 || cellGet(intDigits) > 0)
            && expOk;
        if (valid) Ok(c_strtod(s, ptrNull()))
        else Err("invalid float: " + raw)
    }
}

pub def strSplitChar(s: String, sep: Char): List String = {
    let sepByte = charToInt(sep);
    let len = strLen(s);
    let start = cellNew(0);
    let i = cellNew(0);
    let acc = cellNew(Nil);
    while (cellGet(i) <= len) {
        let cur = cellGet(i);
        let isEnd = if (cur == len) true else strByteAt(s, cur) == sepByte;
        if (isEnd) {
            let part = strSlice(s, cellGet(start), cur);
            cellSet(acc, Cons(part, cellGet(acc)));
            cellSet(start, cur + 1)
        } else ();
        cellSet(i, cur + 1)
    };
    listRev(cellGet(acc))
}

pub def strJoin(parts: List String, sep: String): String = {
    let b = bufNew();
    let cur = cellNew(parts);
    let first = cellNew(true);
    let keepGoing = cellNew(true);
    while (cellGet(keepGoing)) {
        match cellGet(cur) {
            Nil -> cellSet(keepGoing, false),
            Cons(h, t) -> {
                if (!cellGet(first)) bufPushStr(b, sep) else cellSet(first, false);
                bufPushStr(b, h);
                cellSet(cur, t)
            },
        }
    };
    bufToString(b)
}

// Non-negative 31-bit FNV-1a hash
pub def strHash(s: String): Int = {
    let len = strLen(s);
    let h = cellNew(5381);
    let i = cellNew(0);
    while (cellGet(i) < len) {
        let idx = cellGet(i);
        let mixed = (cellGet(h) * 33) ^ strByteAt(s, idx);
        cellSet(h, mixed & 2147483647);
        cellSet(i, idx + 1)
    };
    cellGet(h) & 2147483647
}
