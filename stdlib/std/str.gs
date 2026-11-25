// String manipulation, formatting, splitting, and FNV-1a hashing (`std.str`)

use std.option.{Option, Some, None}
use std.char.isSpaceByte
use std.list.listRev
use std.buf.{bufNew, bufPushStr, bufPushInt, bufToString}

pub def strIsEmpty(s: String): Bool = strLen(s) == 0

pub def intToStr(n: Int): String = {
    let b = bufNew();
    bufPushInt(b, n);
    bufToString(b)
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
        let mixed = bitXor(cellGet(h) * 33, strByteAt(s, idx));
        cellSet(h, bitAnd(mixed, 2147483647));
        cellSet(i, idx + 1)
    };
    bitAnd(cellGet(h), 2147483647)
}
