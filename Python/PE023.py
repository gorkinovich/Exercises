"""
Problem 23: Non-abundant sums
URL: https://projecteuler.net/problem=23

    A perfect number is a number for which the sum of its proper
    divisors is exactly equal to the number. For example, the sum
    of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28,
    which means that 28 is a perfect number.

    A number n is called deficient if the sum of its proper
    divisors is less than n, and it is called abundant if this
    sum exceeds n.

    As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16,
    the smallest number that can be written as the sum of two
    abundant numbers is 24. By mathematical analysis, it can be
    shown that all integers greater than 28123 can be written as
    the sum of two abundant numbers. However, this upper limit
    cannot be reduced any further by analysis even though it is
    known that the greatest number that cannot be expressed as
    the sum of two abundant numbers is less than this limit.

    Find the sum of all the positive integers which cannot be
    written as the sum of two abundant numbers.
"""
import enum
from shared import get_divisors

######################################################################
# Constants
######################################################################

LIMIT = 28_123 + 1


######################################################################
# Enumerations
######################################################################

class NumberType(enum.Enum):
    """
    The type of number of the problem.
    """
    PERFECT = enum.auto()
    DEFICIENT = enum.auto()
    ABUNDANT = enum.auto()

    @classmethod
    def check(cls, number):
        """
        Checks the type of number given.
        :param number: The number to check.
        :return: The type of the number.
        """
        divisors = get_divisors(number)
        value = sum(divisors[:-1])
        if value == number:
            return cls.PERFECT
        elif value < number:
            return cls.DEFICIENT
        else:
            return cls.ABUNDANT


######################################################################
# Functions
######################################################################

def get_numbers_by_type(limit, number_type):
    """
    Gets all the numbers of a given type.
    :param limit: The upper limit in the range of numbers.
    :param number_type: The type of number to get.
    :return: A sorted list with the numbers filtered.
    """
    result = set()
    for victim in range(1, limit):
        if NumberType.check(victim) == number_type:
            result.add(victim)
    return sorted(result)


def generate_abundant_sums(limit):
    """
    Gets all the sums of two abundant numbers.
    :param limit: The upper limit in the range of numbers.
    :return: A set with all the sums of two abundant numbers.
    """
    result = set()
    numbers = get_numbers_by_type(LIMIT, NumberType.ABUNDANT)
    for i in range(len(numbers)):
        left = numbers[i]
        for j in range(i, len(numbers)):
            right = numbers[j]
            if (left + right) < limit:
                result.add(left + right)
            else:
                break
    return result


def main():
    """
    Main entry for the problem solver.
    """
    forbidden = generate_abundant_sums(LIMIT)
    numbers = [number for number in range(1, LIMIT)
               if number not in forbidden]
    result = sum(numbers)

    # Show the final result of the problem:
    message = "The sum of all the naturals which cannot be written "
    message += f"as the sum of two abundant numbers is {result}."
    print(message)


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
