"""
| **Problem 9:** Special Pythagorean triplet
| URL: https://projecteuler.net/problem=9
::

    A Pythagorean triplet is a set of three natural numbers,
    a < b < c, for which: a^2 + b^2 = c^2

    For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

    There exists exactly one Pythagorean triplet for which
    a + b + c = 1000.

    Find the product abc.

:author: Gorka Suárez García
:copyright: (c) 2022, Gorka Suárez García
"""
import math
import itertools

######################################################################
# Constants
######################################################################

CANDIDATE = 1000


######################################################################
# Functions
######################################################################

def is_pythagorean(a, b, c):
    """
    Checks if a triplet of numbers is a pythagorean triplet.
    :param a: The first number of the triplet.
    :param b: The second number of the triplet.
    :param c: The third number of the triplet.
    :return: True if the triplet is pythagorean.
    """
    return (0 < a < b < c) and (a ** 2 + b ** 2 == c ** 2)


def find_triplet(candidate):
    """
    Finds a pythagorean triplet for which sum is equal
    to a given candidate number.
    :param candidate: The candidate number.
    :return: A triplet with the numbers or None.
    """
    # To fulfill the (A < B < C) condition, the C variable goes
    # from 1/3 of the candidates to the top value:
    for c in range(math.trunc(candidate / 3), candidate):
        # Then, the B variable goes from 1 to (c - 1):
        for b in range(1, c):
            # Finally, the A variable gets the rest, and
            # we'll check if the condition applies:
            a = candidate - c - b
            if ((a + b + c) == candidate) \
                    and is_pythagorean(a, b, c):
                return a, b, c
    return None


def main():
    """
    Main entry for the problem solver.
    """
    result = find_triplet(CANDIDATE)

    # Show the final result of the problem:
    if result:
        a, b, c = result
        print(f"The Pythagorean triplet is ({a}, {b}, {c}).")
        print(f"{a} + {b} + {c} = {sum(result)}")
        print(f"{a} x {b} x {c} = {math.prod(result)}")
    else:
        print("No result was found...")


######################################################################
# Functions (Slow version)
######################################################################

def find_triplet_slow(candidate):
    """
    Finds a pythagorean triplet for which sum is equal
    to a given candidate number.
    :param candidate: The candidate number.
    :return: A triplet with the numbers or None.
    """
    numbers = [number for number in range(1, candidate + 1)]
    for triplet in itertools.combinations(numbers, 3):
        if (sum(triplet) == candidate) and is_pythagorean(*triplet):
            return triplet
    return None


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
