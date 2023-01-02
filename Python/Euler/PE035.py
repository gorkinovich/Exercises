"""
| **Problem 35:** Circular primes
| URL: https://projecteuler.net/problem=35
::

    The number, 197, is called a circular prime because all
    rotations of the digits: 197, 971, and 719, are themselves
    prime.

    There are thirteen such primes below 100: 2, 3, 5, 7, 11,
    13, 17, 31, 37, 71, 73, 79, and 97.

    How many circular primes are there below one million?

:author: Gorka Suárez García
:copyright: (c) 2022, Gorka Suárez García
"""
from shared import primes_generator

######################################################################
# Constants
######################################################################

LIMIT = 1_000_000


######################################################################
# Functions
######################################################################

def get_candidates(limit):
    """
    Gets all the primes bellow a limit number.
    :param limit: The limit number.
    :return: A list with the primes bellow the limit.
    """
    result = []
    for prime in primes_generator():
        if prime < limit:
            result.append(prime)
        else:
            break
    return result


def group_by_size(victims):
    """
    Groups the elements of a list using the length.
    :param victims: The list to check.
    :return: A dictionary with the groups.
    """
    result = {}
    for victim in victims:
        size = len(str(victim))
        if size not in result:
            result[size] = []
        result[size].append(victim)
    return result


def get_circular_numbers(number):
    """
    Gets all the variations of a circular number.
    :param number: The number to check.
    :return: A list with the variations.
    """
    result = [number]
    number = str(number)
    for _ in range(len(number) - 1):
        number = number[1:] + number[0]
        result.append(int(number))
    return result


def filter_circular_numbers(size, numbers):
    """
    Filters a group of numbers by checking the circular condition.
    :param size: The size of the numbers.
    :param numbers: The list with the numbers.
    :return: A list with the circular numbers.
    """
    if size == 1:
        return numbers
    else:
        result = []
        numbers = set(numbers)
        while numbers:
            victim = numbers.pop()
            candidates = get_circular_numbers(victim)
            if all([value in numbers for value in candidates[1:]]):
                result.append(tuple(candidates))
            else:
                numbers -= set(candidates)
        return result


def find_circular_primes(limit):
    """
    Finds all the circular primes below a limit number.
    :param limit: The limit number.
    :return: A list with the circular numbers.
    """
    result = []
    candidates = get_candidates(limit)
    groups = group_by_size(candidates)
    for size, primes in groups.items():
        result.extend(filter_circular_numbers(size, primes))
    return result


def main():
    """
    Main entry for the problem solver.
    """
    result = find_circular_primes(LIMIT)

    # Show the final result of the problem:
    print(f"The circular primes below {LIMIT} are: {result}")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
