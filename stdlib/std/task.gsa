// Multicore futures & data-parallel combinators (`std.task`)

use std.list.{listLen, listRev, listAppend}

pub def spawn(f: () -> a): Task a = __concTaskSpawn(f)

pub def await(t: Task a): a = __concTaskAwait(t)

pub def isReady(t: Task a): Bool = __concTaskIsReady(t)

pub def numCpu(): Int = __concNumCpu()

pub def sleepMs(ms: Int): Void = __concSleepMs(ms)

pub def par(f: () -> a, g: () -> b): (a, b) = {
    let rightTask = spawn(g);
    let leftVal = f();
    let rightVal = await(rightTask);
    (leftVal, rightVal)
}

def waitAllLoop(ts: List (Task a), acc: List a): List a = match ts {
    Nil -> listRev(acc),
    Cons(t, rest) -> waitAllLoop(rest, Cons(await(t), acc)),
}

pub def waitAll(tasks: List (Task a)): List a = waitAllLoop(tasks, Nil)

def spawnMapLoop(xs: List a, f: a -> b, acc: List (Task b)): List (Task b) = match xs {
    Nil -> listRev(acc),
    Cons(x, rest) -> spawnMapLoop(rest, f, Cons(spawn(() -> f(x)), acc)),
}

pub def parMap(xs: List a, f: a -> b): List b = {
    let tasks = spawnMapLoop(xs, f, Nil);
    waitAll(tasks)
}

def fillTaskArray(ts: List (Task a), arr: MutArray (Task a), idx: Int): Void = match ts {
    Nil -> (),
    Cons(t, rest) -> {
        arraySet(arr, idx, t);
        fillTaskArray(rest, arr, idx + 1)
    },
}

def partitionReady(
    ts: List (Task a),
    readyAcc: List (Task a),
    pendingAcc: List (Task a)
): (List (Task a), List (Task a)) = match ts {
    Nil -> (listRev(readyAcc), listRev(pendingAcc)),
    Cons(t, rest) ->
        if (isReady(t)) partitionReady(rest, Cons(t, readyAcc), pendingAcc)
        else partitionReady(rest, readyAcc, Cons(t, pendingAcc)),
}

pub def waitAny(
    tasks: List (Task a),
    count: Int,
    timeoutMs: Int
): (List (Task a), List (Task a)) = match tasks {
    Nil -> (Nil, Nil),
    Cons(first, _) -> {
        let n = listLen(tasks);
        let arr = arrayNew(n, first);
        fillTaskArray(tasks, arr, 0);
        __concTaskWaitAny(arr, count, timeoutMs);
        partitionReady(tasks, Nil, Nil)
    },
}

def runChunk(lo: Int, hi: Int, f: Int -> Void): Void = {
    let i = cellNew(lo);
    while (cellGet(i) < hi) {
        let cur = cellGet(i);
        f(cur);
        cellSet(i, cur + 1)
    }
}

def spawnChunks(
    start: Int,
    end: Int,
    step: Int,
    f: Int -> Void,
    acc: List (Task Void)
): List (Task Void) =
    if (start >= end) acc
    else {
        let next = if (start + step > end) end else start + step;
        let t = spawn(() -> runChunk(start, next, f));
        spawnChunks(next, end, step, f, Cons(t, acc))
    }

pub def parFor(start: Int, end: Int, f: Int -> Void): Void = {
    let total = end - start;
    if (total <= 0) ()
    else {
        let workers = numCpu();
        let step = if (total <= workers) 1 else (total + workers - 1) / workers;
        let tasks = spawnChunks(start, end, step, f, Nil);
        let _ = waitAll(tasks);
        ()
    }
}

def mapChunk(lo: Int, hi: Int, f: Int -> a): List a = {
    let acc = cellNew(Nil);
    let i = cellNew(hi - 1);
    while (cellGet(i) >= lo) {
        let cur = cellGet(i);
        cellSet(acc, Cons(f(cur), cellGet(acc)));
        cellSet(i, cur - 1)
    };
    cellGet(acc)
}

def spawnRangeChunks(
    start: Int,
    end: Int,
    step: Int,
    f: Int -> a,
    acc: List (Task (List a))
): List (Task (List a)) =
    if (start >= end) listRev(acc)
    else {
        let next = if (start + step > end) end else start + step;
        let t = spawn(() -> mapChunk(start, next, f));
        spawnRangeChunks(next, end, step, f, Cons(t, acc))
    }

def concatChunkLists(chunks: List (List a)): List a = match chunks {
    Nil -> Nil,
    Cons(c, rest) -> listAppend(c, concatChunkLists(rest)),
}

pub def parRange(start: Int, end: Int, f: Int -> a): List a = {
    let total = end - start;
    if (total <= 0) Nil
    else {
        let workers = numCpu();
        let step = if (total <= workers) 1 else (total + workers - 1) / workers;
        let tasks = spawnRangeChunks(start, end, step, f, Nil);
        concatChunkLists(waitAll(tasks))
    }
}
