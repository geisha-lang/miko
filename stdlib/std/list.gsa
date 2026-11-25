// Linked list utilities (`std.list`)

use std.option.{Option, Some, None}

def lenLoop(xs: List a, acc: Int): Int = match xs {
    Nil -> acc,
    Cons(_, tail) -> lenLoop(tail, acc + 1),
}

pub def listLen(xs: List a): Int = lenLoop(xs, 0)

pub def listIsEmpty(xs: List a): Bool = match xs {
    Nil -> true,
    Cons(_, _) -> false,
}

def revLoop(xs: List a, acc: List a): List a = match xs {
    Nil -> acc,
    Cons(h, t) -> revLoop(t, Cons(h, acc)),
}

pub def listRev(xs: List a): List a = revLoop(xs, Nil)

pub def listAppend(xs: List a, ys: List a): List a =
    revLoop(listRev(xs), ys)

def mapLoop(xs: List a, f: a -> b, acc: List b): List b = match xs {
    Nil -> listRev(acc),
    Cons(h, t) -> mapLoop(t, f, Cons(f(h), acc)),
}

pub def listMap(xs: List a, f: a -> b): List b = mapLoop(xs, f, Nil)

def filterLoop(xs: List a, pred: a -> Bool, acc: List a): List a = match xs {
    Nil -> listRev(acc),
    Cons(h, t) ->
        if (pred(h)) filterLoop(t, pred, Cons(h, acc))
        else filterLoop(t, pred, acc),
}

pub def listFilter(xs: List a, pred: a -> Bool): List a = filterLoop(xs, pred, Nil)

pub def listFold(xs: List a, init: b, f: (b, a) -> b): b = match xs {
    Nil -> init,
    Cons(h, t) -> listFold(t, f(init, h), f),
}

pub def listAny(xs: List a, pred: a -> Bool): Bool = match xs {
    Nil -> false,
    Cons(h, t) -> if (pred(h)) true else listAny(t, pred),
}

pub def listAll(xs: List a, pred: a -> Bool): Bool = match xs {
    Nil -> true,
    Cons(h, t) -> if (pred(h)) listAll(t, pred) else false,
}

pub def listNth(xs: List a, idx: Int): Option a =
    if (idx < 0) None
    else match xs {
        Nil -> None,
        Cons(h, t) -> if (idx == 0) Some(h) else listNth(t, idx - 1),
    }
