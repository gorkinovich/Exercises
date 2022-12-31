"""
| **Problem 33:** Digit cancelling fractions
| URL: https://projecteuler.net/problem=33
::

    The fraction 49/98 is a curious fraction, as an inexperienced
    mathematician in attempting to simplify it may incorrectly
    believe that 49/98 = 4/8, which is correct, is obtained by
    cancelling the 9s.

    We shall consider fractions like, 30/50 = 3/5, to be trivial
    examples.

    There are exactly four non-trivial examples of this type of
    fraction, less than one in value, and containing two digits
    in the numerator and denominator.

    If the product of these four fractions is given in its lowest
    common terms, find the value of the denominator.

:author: Gorka Suárez García
:copyright: (c) 2022, Gorka Suárez García
"""
import math
from shared import check_argv

######################################################################
# Constants
######################################################################

START = 10
LIMIT = 100


######################################################################
# Functions
######################################################################

def zero_ending(victim):
    """
    Check if the value ends in zero.
    :param victim: The value to check.
    :return: True if the value ends in zero.
    """
    value = str(victim)
    return value and (value[-1] == '0')


def get_digits(number):
    """
    Gets the digits of a number.
    :param number: The number to check.
    :return: A set with the digits.
    """
    return set(str(number))


def digits_intersection(n, d):
    """
    Gets the intersection of the digits between two numbers.
    :param n: The left number to check.
    :param d: The right number to check.
    :return: A set with the intersection.
    """
    return get_digits(n) & get_digits(d)


def get_candidates(start, limit, minor=True):
    """
    Gets the candidates of the problem.
    :param start: The start number to check.
    :param limit: The limit number to check.
    :param minor: The comparison flag, when True the numerator
    must be less than the denominator, otherwise the numerator
    must be distinct than the denominator.
    :return: A list with the candidate fractions.
    """
    return [(n, d) for n in range(start, limit)
            for d in range(start, limit)
            if (n < d if minor else n != d)
            and digits_intersection(n, d)
            and not(zero_ending(n) or zero_ending(d))]


def reduce_fraction(n, d):
    """
    Reduces a fraction removing one common digit.
    :param n: The numerator to reduce.
    :param d: The denominator to reduce.
    :return: A tuple with the reduced fraction.
    """
    n_digits = get_digits(n)
    d_digits = get_digits(d)
    digits = n_digits & d_digits
    n_digits -= digits
    d_digits -= digits
    if n_digits and d_digits:
        return int(n_digits.pop()), int(d_digits.pop())
    elif n_digits:
        return int(n_digits.pop()), int(digits.pop())
    elif d_digits:
        return int(digits.pop()), int(d_digits.pop())
    else:
        return n, d


def filter_fractions(candidates):
    """
    Filters the list of non-zero ending fractions to find
    those that can be reduced in a non-trivial way.
    :param candidates: The candidates to filter.
    :return: A list with the reduced fractions.
    """
    result = []
    for n, d in candidates:
        rn, rd = reduce_fraction(n, d)
        if n != rn and d != rd and (d * rn / n) == rd:
            result.append(((rn, rd), (n, d)))
    return result


def main():
    """
    Main entry for the problem solver.
    """
    candidates = get_candidates(START, LIMIT)
    numbers = filter_fractions(candidates)
    result = math.prod(d for (_, d), _ in numbers)

    if check_argv("show"):
        print(f"{candidates = }")
        print(f"{numbers = }")

    # Show the final result of the problem:
    print(f"The value of the denominator of the product is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()


######################################################################
# Test Functions
######################################################################

def test(minor=True):
    candidates = get_candidates(START, LIMIT, minor)
    reduced = [(v, reduce_fraction(*v)) for v in candidates]
    numbers = filter_fractions(candidates)

    print([(a, b) for a, b in reduced if a != b])
    print([(a, b) for a, b in reduced if a == b])
    print(f"{numbers = }")
