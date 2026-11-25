data Either a b {
    Left(a),
    Right(b)
}

def fromLeft(dflt, e) = match e {
    Left(x) -> x,
    Right(y) -> dflt
}

def fromRight(dflt, e) = match e {
    Left(x) -> dflt,
    Right(y) -> y
}

def main() = putNumber(fromLeft(0, Left(10)) + fromRight(0, Right(32)))
