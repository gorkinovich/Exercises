"""
| **Problem 38:** Pandigital multiples
| URL: https://projecteuler.net/problem=38
::

    Take the number 192 and multiply it by each of 1, 2, and 3:

                        192 × 1 = 192
                        192 × 2 = 384
                        192 × 3 = 576

    By concatenating each product we get the 1 to 9 pandigital,
    192384576. We will call 192384576 the concatenated product
    of 192 and (1,2,3)

    The same can be achieved by starting with 9 and multiplying
    by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which
    is the concatenated product of 9 and (1,2,3,4,5).

    What is the largest 1 to 9 pandigital 9-digit number that can
    be formed as the concatenated product of an integer with
    (1,2, ... , n) where n > 1?

:author: Gorka Suárez García
:copyright: (c) 2023, Gorka Suárez García
"""
import math

######################################################################
# Constants
######################################################################

DIGITS = {str(digit) for digit in range(1, 10)}
LEN_DIGITS = len(DIGITS)
LIMIT = 10 ** math.ceil(LEN_DIGITS / 2) - 1


######################################################################
# Functions
######################################################################

def check_condition(number):
    """
    Checks the pandigital problem condition.
    :param number: The number to check.
    :return: True if the number is pandigital.
    """
    digits = list(str(number))
    if len(digits) != len(set(digits)):
        return False
    index = 2
    while True:
        digits.extend(str(number * 2))
        if len(digits) == LEN_DIGITS:
            return DIGITS == set(digits)
        elif len(digits) > LEN_DIGITS:
            return False
        index += 1


def find_number(limit):
    """
    Finds the largest pandigital 9-digit number.
    :param limit: The limit number.
    :return: The largest pandigital 9-digit number.
    """
    for number in range(limit, 0, -1):
        if check_condition(number):
            return number
    return None


def main():
    """
    Main entry for the problem solver.
    """
    result = find_number(LIMIT)

    # Show the final result of the problem:
    print(f"The largest pandigital 9-digit number is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
