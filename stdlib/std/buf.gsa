// Amortized O(1) growable byte/string builder (`std.buf`)

pub data StringBuf {
    MkStringBuf(Cell (MutArray Int), Cell Int),
}

pub def bufWithCap(cap: Int): StringBuf = {
    let actual = if (cap < 16) 16 else cap;
    MkStringBuf(cellNew(arrayNew(actual, 0)), cellNew(0))
}

pub def bufNew(): StringBuf = bufWithCap(32)

pub def bufLen(b: StringBuf): Int = match b {
    MkStringBuf(_, lenCell) -> cellGet(lenCell),
}

pub def bufClear(b: StringBuf): Void = match b {
    MkStringBuf(_, lenCell) -> cellSet(lenCell, 0),
}

def ensureExtra(b: StringBuf, needed: Int): Void = match b {
    MkStringBuf(arrCell, lenCell) -> {
        let curArr = cellGet(arrCell);
        let curLen = cellGet(lenCell);
        let curCap = arrayLen(curArr);
        if (curLen + needed > curCap) {
            let newCap = cellNew(if (curCap < 16) 16 else curCap * 2);
            while (cellGet(newCap) < curLen + needed) {
                cellSet(newCap, cellGet(newCap) * 2)
            };
            let grown = arrayNew(cellGet(newCap), 0);
            let i = cellNew(0);
            while (cellGet(i) < curLen) {
                let idx = cellGet(i);
                arraySet(grown, idx, arrayGet(curArr, idx));
                cellSet(i, idx + 1)
            };
            cellSet(arrCell, grown)
        } else ()
    },
}

pub def bufPushByte(b: StringBuf, byte: Int): Void = {
    ensureExtra(b, 1);
    match b {
        MkStringBuf(arrCell, lenCell) -> {
            let idx = cellGet(lenCell);
            arraySet(cellGet(arrCell), idx, bitAnd(byte, 255));
            cellSet(lenCell, idx + 1)
        },
    }
}

pub def bufPushChar(b: StringBuf, ch: Char): Void =
    bufPushByte(b, charToInt(ch))

pub def bufPushStr(b: StringBuf, s: String): Void = {
    let n = strLen(s);
    ensureExtra(b, n);
    match b {
        MkStringBuf(arrCell, lenCell) -> {
            let dst = cellGet(arrCell);
            let base = cellGet(lenCell);
            let i = cellNew(0);
            while (cellGet(i) < n) {
                let off = cellGet(i);
                arraySet(dst, base + off, strByteAt(s, off));
                cellSet(i, off + 1)
            };
            cellSet(lenCell, base + n)
        },
    }
}

pub def bufPushLine(b: StringBuf, s: String): Void = {
    bufPushStr(b, s);
    bufPushByte(b, 10)
}

pub def bufPushBuf(dst: StringBuf, src: StringBuf): Void = match src {
    MkStringBuf(srcArrCell, srcLenCell) -> {
        let n = cellGet(srcLenCell);
        if (n > 0) {
            ensureExtra(dst, n);
            match dst {
                MkStringBuf(dstArrCell, dstLenCell) -> {
                    let sArr = cellGet(srcArrCell);
                    let dArr = cellGet(dstArrCell);
                    let base = cellGet(dstLenCell);
                    let i = cellNew(0);
                    while (cellGet(i) < n) {
                        let off = cellGet(i);
                        arraySet(dArr, base + off, arrayGet(sArr, off));
                        cellSet(i, off + 1)
                    };
                    cellSet(dstLenCell, base + n)
                },
            }
        } else ()
    },
}

pub def bufPushInt(b: StringBuf, n: Int): Void =
    if (n == 0) {
        bufPushByte(b, 48)
    } else if (n == -2147483647 - 1) {
        bufPushStr(b, "-2147483648")
    } else {
        let v = cellNew(if (n < 0) { bufPushByte(b, 45); -n } else n);
        let digits = arrayNew(12, 0);
        let dlen = cellNew(0);
        while (cellGet(v) > 0) {
            let cur = cellGet(v);
            let idx = cellGet(dlen);
            arraySet(digits, idx, 48 + (cur % 10));
            cellSet(dlen, idx + 1);
            cellSet(v, cur / 10)
        };
        let k = cellNew(cellGet(dlen) - 1);
        while (cellGet(k) >= 0) {
            let idx = cellGet(k);
            bufPushByte(b, arrayGet(digits, idx));
            cellSet(k, idx - 1)
        }
    }

pub def bufToString(b: StringBuf): String = match b {
    MkStringBuf(arrCell, lenCell) -> strFromBytes(cellGet(arrCell), cellGet(lenCell)),
}
