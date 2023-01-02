"""
| **Problem 1:** Multiples of 3 or 5
| URL: https://projecteuler.net/problem=1
::

    If we list all the natural numbers below 10 that are multiples of
    3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

    Find the sum of all the multiples of 3 or 5 below 1000.

:author: Gorka Suárez García
:copyright: (c) 2022, Gorka Suárez García
"""


######################################################################
# Functions
######################################################################

def is_multiple(left, right):
    """
    Checks if the LHS is a multiple of the RHS.
    :param left: The left-hand side.
    :param right: The right-hand side.
    :return: True when the LHS is a multiple of the RHS.
    """
    return left % right == 0


def is_multiple_35(victim):
    """
    Checks if a number is multiple of 3 or 5.
    :param victim: The number to check.
    :return: True when the number is multiple of 3 or 5.
    """
    return is_multiple(victim, 3) or is_multiple(victim, 5)


def main():
    """
    Main entry for the problem solver.
    """
    result = 0
    for victim in range(1, 1000):
        if is_multiple_35(victim):
            result += victim

    print(f"The sum of all the multiples of 3 or 5 below 1000 is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
