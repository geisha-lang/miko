def factorial(n) = if (n == 0) 1 else n * frac(n - 1)

def main() = putNumber(factorial(10))