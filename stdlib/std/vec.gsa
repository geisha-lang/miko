// Dynamic growable array (`std.vec`), backed by `MutArray (Option a)` so `vecNew()` needs no dummy value!

use std.option.{Option, Some, None}

pub data Vec a {
    MkVec(Cell (MutArray (Option a)), Cell Int),
}

pub def vecWithCap(cap: Int): Vec a = {
    let actual = if (cap < 8) 8 else cap;
    MkVec(cellNew(arrayNew(actual, None)), cellNew(0))
}

pub def vecNew(): Vec a = vecWithCap(8)

pub def vecLen(v: Vec a): Int = match v {
    MkVec(_, lenCell) -> cellGet(lenCell),
}

pub def vecIsEmpty(v: Vec a): Bool = vecLen(v) == 0

pub def vecPush(v: Vec a, item: a): Void = match v {
    MkVec(arrCell, lenCell) -> {
        let curArr = cellGet(arrCell);
        let curLen = cellGet(lenCell);
        let curCap = arrayLen(curArr);
        if (curLen >= curCap) {
            let newCap = if (curCap < 8) 8 else curCap * 2;
            let grown = arrayNew(newCap, None);
            let i = cellNew(0);
            while (cellGet(i) < curLen) {
                let idx = cellGet(i);
                arraySet(grown, idx, arrayGet(curArr, idx));
                cellSet(i, idx + 1)
            };
            cellSet(arrCell, grown)
        } else ();
        arraySet(cellGet(arrCell), curLen, Some(item));
        cellSet(lenCell, curLen + 1)
    },
}

pub def vecPop(v: Vec a): Option a = match v {
    MkVec(arrCell, lenCell) -> {
        let curLen = cellGet(lenCell);
        if (curLen <= 0) None
        else {
            let lastIdx = curLen - 1;
            let arr = cellGet(arrCell);
            let out = arrayGet(arr, lastIdx);
            // Clear popped slot so dead heap references are not retained!
            arraySet(arr, lastIdx, None);
            cellSet(lenCell, lastIdx);
            out
        }
    },
}

pub def vecGet(v: Vec a, idx: Int): Option a = match v {
    MkVec(arrCell, lenCell) -> {
        let curLen = cellGet(lenCell);
        if (idx < 0 || idx >= curLen) None
        else arrayGet(cellGet(arrCell), idx)
    },
}

pub def vecGetOr(v: Vec a, idx: Int, fallback: a): a = match vecGet(v, idx) {
    Some(x) -> x,
    None -> fallback,
}

pub def vecSet(v: Vec a, idx: Int, item: a): Void = match v {
    MkVec(arrCell, lenCell) -> {
        let curLen = cellGet(lenCell);
        let arr = cellGet(arrCell);
        // Trigger standard bounds failure if out of logical range `0..curLen`
        if (idx < 0 || idx >= curLen) {
            let _ = arrayGet(arrayNew(curLen, 0), idx);
            ()
        } else {
            arraySet(arr, idx, Some(item))
        }
    },
}

pub def vecClear(v: Vec a): Void = match v {
    MkVec(arrCell, lenCell) -> {
        let arr = cellGet(arrCell);
        let n = cellGet(lenCell);
        let i = cellNew(0);
        while (cellGet(i) < n) {
            let idx = cellGet(i);
            arraySet(arr, idx, None);
            cellSet(i, idx + 1)
        };
        cellSet(lenCell, 0)
    },
}

pub def vecToList(v: Vec a): List a = match v {
    MkVec(arrCell, lenCell) -> {
        let arr = cellGet(arrCell);
        let acc = cellNew(Nil);
        let i = cellNew(cellGet(lenCell) - 1);
        while (cellGet(i) >= 0) {
            let idx = cellGet(i);
            match arrayGet(arr, idx) {
                Some(x) -> cellSet(acc, Cons(x, cellGet(acc))),
                None -> (),
            };
            cellSet(i, idx - 1)
        };
        cellGet(acc)
    },
}
