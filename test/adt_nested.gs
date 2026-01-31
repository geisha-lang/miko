data Inner {
    InnerA(Int),
    InnerB(Int, Int)
}

data Outer {
    OuterX(Inner),
    OuterY(Inner, Inner)
}

def sumInner(i) = match i {
    InnerA(x) -> x,
    InnerB(x, y) -> x + y
}

def sumOuter(o) = match o {
    OuterX(i) -> sumInner(i),
    OuterY(i1, i2) -> sumInner(i1) + sumInner(i2)
}

def main() = putNumber(sumOuter(OuterY(InnerA(10), InnerB(15, 17))))
