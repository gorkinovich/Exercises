"""
| **Problem 6:** Sum square difference
| URL: https://projecteuler.net/problem=6
::

    The sum of the squares of the first ten natural numbers is,
    1^2 + 2^2 + ... + 10^2 = 385

    The square of the sum of the first ten natural numbers is,
    (1 + 2 + ... + 10)^2 = 552 = 3025

    Hence, the difference between the sum of the squares of
    the first ten natural numbers and the square of the sum
    is 3025 - 385 = 2640.

    Find the difference between the sum of the squares of the
    first one hundred natural numbers and the square of the sum.

:author: Gorka Suárez García
:copyright: (c) 2022, Gorka Suárez García
"""

######################################################################
# Constants
######################################################################

FIRST = 1
LAST = 100


######################################################################
# Functions
######################################################################

def calc_number(first, last):
    """
    Calcs the difference between the sum of the squares
    and the square of the sum of a range of numbers.
    :param first: The first value of the range.
    :param last: The last value of the range.
    :return: The difference between the sums.
    """
    numbers = range(first, last + 1)
    squares = [number ** 2 for number in numbers]
    sum_square = sum(squares)
    square_sum = sum(numbers) ** 2
    return square_sum - sum_square


def main():
    """
    Main entry for the problem solver.
    """
    result = calc_number(FIRST, LAST)

    # Show the final result of the problem:
    message = "The difference between the sum of the squares "
    message += "and the square of the sum of the first "
    message += f"{LAST} numbers is {result}."
    print(message)


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
