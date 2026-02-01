concept Eq a {
    eq: a * a -> Bool
}

instance Eq Int {
    def eq(x, y) = x == y
}

def main() = if (eq(1, 1)) putNumber(42) else putNumber(0)
