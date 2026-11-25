// File system operations via portable POSIX / ANSI C ABI (`sys.fs`)

use std.result.{Result, Ok, Err}
use std.buf.{bufNew, bufPushByte, bufToString}

extern "fopen" def c_fopen(path: String, mode: String): Ptr
extern "fclose" def c_fclose(fp: Ptr): Int
extern "fread" def c_fread(buf: Ptr, size: Int64, nitems: Int64, fp: Ptr): Int64
extern "fwrite" def c_fwrite(buf: String, size: Int64, nitems: Int64, fp: Ptr): Int64
extern "access" def c_access(path: String, mode: Int): Int
extern "remove" def c_remove(path: String): Int

pub def fileExists(path: String): Bool =
    c_access(path, 0) == 0

pub def readFile(path: String): Result String String = {
    let fp = c_fopen(path, "rb");
    if (ptrIsNull(fp)) {
        Err(strConcat("failed to open file for reading: ", path))
    } else {
        let chunkCap = 4096;
        let chunk = ptrAllocBytes(chunkCap);
        let out = bufNew();
        let reading = cellNew(true);
        while (cellGet(reading)) {
            let got = i64ToInt(c_fread(chunk, intToI64(1), intToI64(chunkCap), fp));
            if (got <= 0) {
                cellSet(reading, false)
            } else {
                let i = cellNew(0);
                while (cellGet(i) < got) {
                    let idx = cellGet(i);
                    bufPushByte(out, ptrByteAt(chunk, idx));
                    cellSet(i, idx + 1)
                }
            }
        };
        let _ = c_fclose(fp);
        Ok(bufToString(out))
    }
}

pub def writeFile(path: String, content: String): Result Void String = {
    let fp = c_fopen(path, "wb");
    if (ptrIsNull(fp)) {
        Err(strConcat("failed to open file for writing: ", path))
    } else {
        let total = strLen(content);
        let written = i64ToInt(c_fwrite(content, intToI64(1), intToI64(total), fp));
        let closeRc = c_fclose(fp);
        if (written == total && closeRc == 0) Ok(())
        else Err(strConcat("failed to write full contents to: ", path))
    }
}

pub def removeFile(path: String): Bool =
    c_remove(path) == 0
