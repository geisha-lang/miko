data Bool2 {
    False2,
    True2
}

def not2(b) = match b {
    False2 -> True2,
    True2 -> False2
}

def and2(a, b) = match a {
    False2 -> False2,
    True2 -> b
}

def or2(a, b) = match a {
    False2 -> b,
    True2 -> True2
}

def toInt2(b) = match b {
    False2 -> 0,
    True2 -> 1
}

def main() = putNumber(toInt2(and2(True2, not2(False2))) + toInt2(or2(False2, False2)))
