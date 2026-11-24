// FizzBuzz from 1 to 15
def fizzbuzz(i: Int, limit: Int): Void =
    if (i > limit) ()
    else {
        if (i % 15 == 0) printLn("FizzBuzz")
        else if (i % 3 == 0) printLn("Fizz")
        else if (i % 5 == 0) printLn("Buzz")
        else { putNumber(i); printLn("") };
        fizzbuzz(i + 1, limit)
    }

def main() = fizzbuzz(1, 15)
