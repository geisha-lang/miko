// Bit manipulation helpers (`std.bits`) built on Geisha's native bitwise operators

pub def hasFlag(flags: Int, mask: Int): Bool = (flags & mask) == mask

pub def setFlag(flags: Int, mask: Int): Int = flags | mask

pub def clearFlag(flags: Int, mask: Int): Int = flags & ~mask

pub def toggleFlag(flags: Int, mask: Int): Int = flags ^ mask

pub def popCount(x: Int): Int = {
    let v = cellNew(x);
    let count = cellNew(0);
    while (cellGet(v) != 0) {
        let cur = cellGet(v);
        cellSet(v, cur & (cur - 1));
        cellSet(count, cellGet(count) + 1)
    };
    cellGet(count)
}

pub def popCount64(x: Int64): Int = {
    let zero64 = intToI64(0);
    let one64 = intToI64(1);
    let v = cellNew(x);
    let count = cellNew(0);
    while (cellGet(v) != zero64) {
        let cur = cellGet(v);
        cellSet(v, cur & (cur - one64));
        cellSet(count, cellGet(count) + 1)
    };
    cellGet(count)
}

pub def trailingZeros(x: Int): Int =
    if (x == 0) 32
    else {
        let v = cellNew(x);
        let n = cellNew(0);
        while ((cellGet(v) & 1) == 0) {
            cellSet(v, cellGet(v) >>> 1);
            cellSet(n, cellGet(n) + 1)
        };
        cellGet(n)
    }
