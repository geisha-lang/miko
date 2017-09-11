def fibonacci(n) =
    if (n == 1 || n == 2)
        1
    else
        fibonacci(n - 1) + fibonacci(n - 2)

def main() =
    let fib = fibonacci(10) in
        putNumber(fib)
