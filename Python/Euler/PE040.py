"""
| **Problem 40:** Champernowne's constant
| URL: https://projecteuler.net/problem=40
::

    An irrational decimal fraction is created by concatenating
    the positive integers:

    0.123456789101112131415161718192021...
                 *

    It can be seen that the 12th digit of the fractional part
    is 1.

    If d(n) represents the nth digit of the fractional part,
    find the value of the following expression.

    d(1) × d(10) × d(100) × d(1000) × d(10000)
         × d(100000) × d(1000000)

:author: Gorka Suárez García
:copyright: (c) 2023, Gorka Suárez García
"""
import math
from shared import check_argv

######################################################################
# Constants
######################################################################

LIMIT = 7
CANDIDATES = [10 ** number for number in range(0, LIMIT)]


######################################################################
# Classes
######################################################################

class Sequence:
    """
    This type represents the problems sequence of numbers.
    """

    def __init__(self):
        """
        Initializes the object instance.
        """
        self.__decimals = ""
        self.__number = 1

    def __str__(self):
        """
        Gets the string representation of the object.
        :return: A string with the representation.
        """
        return f"0.{self.__decimals}"

    def __getitem__(self, item):
        """
        Gets the position of a decimal in the sequence.
        :param item: The position to get.
        :return: A number with the digit obtained.
        """
        if item >= len(self.__decimals):
            self.expand(item + 1)
        return int(self.__decimals[item])

    def expand(self, size):
        """
        Expands the current sequence.
        :param size: The new minimum size.
        """
        while len(self.__decimals) < size:
            self.__decimals += str(self.__number)
            self.__number += 1


######################################################################
# Functions
######################################################################

def main():
    """
    Main entry for the problem solver.
    """
    decimals = Sequence()
    numbers = [decimals[item - 1] for item in CANDIDATES]
    result = math.prod(numbers)

    if check_argv("show"):
        print(f"{numbers = }")
        print(f"{decimals = }")

    # Show the final result of the problem:
    print(f"The product of the digits in the irrational decimal fraction is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
