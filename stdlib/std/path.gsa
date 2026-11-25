// Portable POSIX path manipulation (`std.path`)

pub def isAbsPath(p: String): Bool =
    strLen(p) > 0 && strByteAt(p, 0) == 47

def trimTrailingSlash(p: String): String = {
    let hi = cellNew(strLen(p));
    while (cellGet(hi) > 1 && strByteAt(p, cellGet(hi) - 1) == 47) {
        cellSet(hi, cellGet(hi) - 1)
    };
    strSlice(p, 0, cellGet(hi))
}

pub def pathJoin(base: String, child: String): String =
    if (strLen(base) == 0 || base == ".") child
    else if (strLen(child) == 0) base
    else if (isAbsPath(child)) child
    else {
        let b = trimTrailingSlash(base);
        if (b == "/") "/" + child
        else b + "/" + child
    }

def lastSlashIndex(p: String): Int = {
    let i = cellNew(strLen(p) - 1);
    let found = cellNew(-1);
    while (cellGet(i) >= 0 && cellGet(found) < 0) {
        if (strByteAt(p, cellGet(i)) == 47) cellSet(found, cellGet(i))
        else cellSet(i, cellGet(i) - 1)
    };
    cellGet(found)
}

pub def pathDirname(raw: String): String = {
    let p = trimTrailingSlash(raw);
    let idx = lastSlashIndex(p);
    if (idx < 0) "."
    else if (idx == 0) "/"
    else strSlice(p, 0, idx)
}

pub def pathBasename(raw: String): String = {
    let p = trimTrailingSlash(raw);
    let idx = lastSlashIndex(p);
    if (idx < 0) p
    else strSlice(p, idx + 1, strLen(p))
}

pub def pathStem(raw: String): String = {
    let base = pathBasename(raw);
    let i = cellNew(strLen(base) - 1);
    let dotIdx = cellNew(-1);
    while (cellGet(i) > 0 && cellGet(dotIdx) < 0) {
        if (strByteAt(base, cellGet(i)) == 46) cellSet(dotIdx, cellGet(i))
        else cellSet(i, cellGet(i) - 1)
    };
    if (cellGet(dotIdx) <= 0) base
    else strSlice(base, 0, cellGet(dotIdx))
}

pub def pathWithExt(raw: String, ext: String): String = {
    let p = trimTrailingSlash(raw);
    let slashIdx = lastSlashIndex(p);
    let i = cellNew(strLen(p) - 1);
    let dotIdx = cellNew(-1);
    while (cellGet(i) > slashIdx + 1 && cellGet(dotIdx) < 0) {
        if (strByteAt(p, cellGet(i)) == 46) cellSet(dotIdx, cellGet(i))
        else cellSet(i, cellGet(i) - 1)
    };
    let stemPart = if (cellGet(dotIdx) < 0) p else strSlice(p, 0, cellGet(dotIdx));
    if (strLen(ext) == 0) stemPart
    else if (strByteAt(ext, 0) == 46) stemPart + ext
    else stemPart + "." + ext
}
