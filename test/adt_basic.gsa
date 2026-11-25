data IntOption {
    IntNone,
    IntSome(Int)
}

def unwrapOr(opt, dflt) = match opt {
    IntNone -> dflt,
    IntSome(x) -> x
}

def main() = putNumber(unwrapOr(IntSome(42), 0))
