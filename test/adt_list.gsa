data List a {
    Nil,
    Cons(a, List a)
}

def length(lst) = match lst {
    Nil -> 0,
    Cons(h, t) -> 1 + length(t)
}

def main() = putNumber(length(Cons(1, Cons(2, Cons(3, Nil)))))
