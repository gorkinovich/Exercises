"""
| **Problem 16:** Power digit sum
| URL: https://projecteuler.net/problem=16
::

    2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

    What is the sum of the digits of the number 2^1000?

:author: Gorka Suárez García
:copyright: (c) 2022, Gorka Suárez García
"""

######################################################################
# Constants
######################################################################

GOAL = 1000


######################################################################
# Functions
######################################################################

def main():
    """
    Main entry for the problem solver.
    """
    result = sum(int(c) for c in str(2 ** GOAL))

    # Show the final result of the problem:
    print(f"The sum of the digits of the number 2^{GOAL} is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
