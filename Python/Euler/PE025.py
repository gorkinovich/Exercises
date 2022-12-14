"""
| **Problem 25:** 1000-digit Fibonacci number
| URL: https://projecteuler.net/problem=25
::

    The Fibonacci sequence is defined by the recurrence relation:

        F(n) = F(n-1) + F(n-2), where F(1) = 1 and F(2) = 1.

    Hence, the first 12 terms will be:

        F(1)  = 1
        F(2)  = 1
        F(3)  = 2
        F(4)  = 3
        F(5)  = 5
        F(6)  = 8
        F(7)  = 13
        F(8)  = 21
        F(9)  = 34
        F(10) = 55
        F(11) = 89
        F(12) = 144

    The 12th term, F(12), is the first term to contain three
    digits.

    What is the first term in the Fibonacci sequence to contain
    1000 digits?

:author: Gorka Suárez García
:copyright: (c) 2022, Gorka Suárez García
"""
from shared import fibonacci_generator

######################################################################
# Constants
######################################################################

SIZE = 1000


######################################################################
# Functions
######################################################################

def find_number(size):
    """
    Finds a fibonacci number greater or equal than a size in digits.
    :param size: The number of digits to surpass.
    :return: The first number to surpass the given size.
    """
    limit = 10 ** (size - 1)
    for number in fibonacci_generator():
        if number >= limit:
            return number


def main():
    """
    Main entry for the problem solver.
    """
    result = find_number(SIZE)

    # Show the final result of the problem:
    print(f"The first term in the Fibonacci sequence to contain {SIZE} digits {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
