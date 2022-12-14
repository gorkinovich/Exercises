"""
| **Problem 20:** Factorial digit sum
| URL: https://projecteuler.net/problem=20
::

    n! means n × (n - 1) × ... × 3 × 2 × 1

    For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
    and the sum of the digits in the number 10! is
    3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

    Find the sum of the digits in the number 100!

:author: Gorka Suárez García
:copyright: (c) 2022, Gorka Suárez García
"""
import math

######################################################################
# Constants
######################################################################

GOAL = 100


######################################################################
# Functions
######################################################################

def main():
    """
    Main entry for the problem solver.
    """
    result = sum(int(c) for c in str(math.factorial(GOAL)))

    # Show the final result of the problem:
    print(f"The sum of the digits in the number {GOAL}! is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
