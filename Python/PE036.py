"""
| **Problem 36:** Double-base palindromes
| URL: https://projecteuler.net/problem=36
::

    The decimal number, 585 = 1001001001b (binary), is
    palindromic in both bases.

    Find the sum of all numbers, less than one million, which
    are palindromic in base 10 and base 2.

    (Please note that the palindromic number, in either base,
    may not include leading zeros.)

:author: Gorka Suárez García
:copyright: (c) 2023, Gorka Suárez García
"""
from shared import check_argv

######################################################################
# Constants
######################################################################

LIMIT = 1_000_000


######################################################################
# Functions
######################################################################

def check_condition(number):
    """
    Checks the problem condition for a number.
    :param number: The number to check.
    :return: True if the condition applies.
    """
    bin_value = bin(number)[2:]
    str_value = str(number)
    bin_reversed = bin_value[-1::-1]
    str_reversed = str_value[-1::-1]
    return bin_value == bin_reversed and str_value == str_reversed


def main():
    """
    Main entry for the problem solver.
    """
    numbers = [number for number in range(1, LIMIT)
               if check_condition(number)]
    result = sum(numbers)

    if check_argv("show"):
        print(f"{numbers = }")

    # Show the final result of the problem:
    message = f"The sum of all numbers, less than {LIMIT}, which "
    message += f"are palindromic in base 10 and base 2 is {result}."
    print(message)


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
