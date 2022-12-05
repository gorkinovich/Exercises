"""
Problem 5: Smallest multiple
URL: https://projecteuler.net/problem=5

    2520 is the smallest number that can be divided by each of the
    numbers from 1 to 10 without any remainder.

    What is the smallest positive number that is evenly divisible
    (divisible with no remainder) by all of the numbers from 1 to 20?
"""
import math
import itertools


######################################################################
# Constants
######################################################################

FIRST = 1
LAST = 20


######################################################################
# Functions
######################################################################

def find_number(first, last):
    """
    Find the smallest natural divisible by a range of numbers.
    :param first: The first value of the divisors range.
    :param last: The last value of the divisors range.
    :return: The smallest natural number.
    """
    # First, get all the combinations of the divisors:
    divisors = range(first, last + 1)
    combinations = set()
    for size in range(1, len(divisors)):
        values = itertools.combinations(divisors, size)
        combinations |= set(math.prod(value) for value in values)
    # Then check each combination in ascending order, to
    # find the final result, and return it when found:
    for victim in sorted(combinations):
        success = True
        for divisor in divisors:
            if (victim % divisor) != 0:
                success = False
                break

        if success:
            return victim
    # Otherwise return None:
    return None


def main():
    """
    Main entry for the problem solver.
    """
    result = find_number(FIRST, LAST)

    # Show the final result of the problem:
    message = "The smallest positive number that is a multiple "
    message += f"of all numbers from 1 to 20 is {result}."
    print(message)


######################################################################
# Functions (Slow version)
######################################################################

def find_number_slow(first, last):
    """
    Find the smallest natural divisible by a range of numbers.
    :param first: The first value of the divisors range.
    :param last: The last value of the divisors range.
    :return: The smallest natural number.
    """
    result = 1
    while True:
        # Check if the current number is divisible:
        success = True
        for divisor in range(first, last + 1):
            if (result % divisor) != 0:
                success = False
                break
        # Return the result when success, otherwise
        # select the next candidate:
        if success:
            return result
        else:
            result += 1


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
