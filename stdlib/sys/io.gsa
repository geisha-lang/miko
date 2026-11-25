// Standard stream I/O via POSIX C ABI (`sys.io`)

extern "write" def c_write(fd: Int, buf: String, count: Int64): Int64

pub def print(s: String): Void = {
    let _ = c_write(1, s, intToI64(strLen(s)));
    ()
}

pub def eprint(s: String): Void = {
    let _ = c_write(2, s, intToI64(strLen(s)));
    ()
}

pub def eprintLn(s: String): Void = {
    eprint(s);
    eprint("\n")
}
