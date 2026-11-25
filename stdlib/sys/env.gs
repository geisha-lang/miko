// Command-line arguments & environment variables (`sys.env`)

use std.option.{Option, Some, None}

extern "getenv" def c_getenv(name: String): Ptr

pub def argCount(): Int = sysArgc()

pub def argAt(idx: Int): String = sysArgv(idx)

pub def args(): List String = {
    let acc = cellNew(Nil);
    let i = cellNew(sysArgc() - 1);
    while (cellGet(i) >= 0) {
        let idx = cellGet(i);
        cellSet(acc, Cons(sysArgv(idx), cellGet(acc)));
        cellSet(i, idx - 1)
    };
    cellGet(acc)
}

pub def getEnv(name: String): Option String = {
    let p = c_getenv(name);
    if (ptrIsNull(p)) None
    else Some(ptrToString(p))
}
