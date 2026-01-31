data Pair a b {
    MkPair(a, b)
}

def fst(p) = match p {
    MkPair(x, y) -> x
}

def snd(p) = match p {
    MkPair(x, y) -> y
}

def swap(p) = match p {
    MkPair(x, y) -> MkPair(y, x)
}

def main() = putNumber(fst(MkPair(10, 20)) + snd(swap(MkPair(30, 12))))
