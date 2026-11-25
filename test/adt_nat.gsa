data Nat {
    Zero,
    Succ(Nat)
}

def toInt(n) = match n {
    Zero -> 0,
    Succ(m) -> 1 + toInt(m)
}

def add(n, m) = match n {
    Zero -> m,
    Succ(p) -> Succ(add(p, m))
}

def main() = putNumber(toInt(add(Succ(Succ(Zero)), Succ(Succ(Succ(Zero))))))
