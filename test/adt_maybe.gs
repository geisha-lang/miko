data Maybe a {
    Nothing,
    Just(a)
}

def fromMaybe(dflt, m) = match m {
    Nothing -> dflt,
    Just(x) -> x
}

def isJust(m) = match m {
    Nothing -> 0,
    Just(x) -> 1
}

def main() = putNumber(fromMaybe(99, Just(42)) + isJust(Nothing))
