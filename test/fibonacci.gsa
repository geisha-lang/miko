def fib(n) =
    let fib_tail = (n, p, c) ->
        if (n == 0)
            -1
        else
            if (n == 1)
                p
            else
                fib_tail(n - 1, c, p + c)
    in fib_tail(n, 0, 1)

def main() =
    let f = fib(10) in
        putNumber(f)
