"""
Problem 27: Quadratic primes
URL: https://projecteuler.net/problem=27

    Euler published the remarkable quadratic formula: n^2 + n + 41

    It turns out that the formula will produce 40 primes for
    the consecutive values n = 0 to 39. However, when n = 40,
    40^2 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and
    certainly when n = 41, 41^2 + 41 + 41 is clearly divisible
    by 41.

    Using computers, the incredible formula  n^2 - 79n + 1601 was
    discovered, which produces 80 primes for the consecutive values
    n = 0 to 79. The product of the coefficients, -79 and 1601, is
    -126479.

    Considering quadratics of the form:

        n^2 + an + b, where |a| < 1000 and |b| <= 1000

        where |n| is the modulus/absolute value of n
        e.g. |11| = 11 and |-4| = 4

    Find the product of the coefficients, a and b, for the
    quadratic expression that produces the maximum number of
    primes for consecutive values of n, starting with n = 0.
"""
import math
from shared import PrimesGenerator, check_argv

######################################################################
# Constants
######################################################################

LIMIT = 1000


######################################################################
# Functions
######################################################################

def calc_formula(n, a, b):
    """
    Gets the value for the problem formula.
    :param n: The input value.
    :param a: The first coefficient.
    :param b: The second coefficient.
    :return: The final value.
    """
    return n ** 2 + a * n + b


def get_primes_chain(a, b):
    """
    Gets the chain of primes for some coefficients.
    :param a: The first coefficient.
    :param b: The second coefficient.
    :return: The list of primes obtained.
    """
    n = 0
    primes = []
    while PrimesGenerator.is_prime(number := calc_formula(n, a, b)):
        primes.append(number)
        n += 1
    return primes


def find_max_chain(first_limit, second_limit):
    """
    Finds the maximum chain of primes for some coefficients.
    The range of the limits are [0, first) and [0, second].
    :param first_limit: The first coefficient limit.
    :param second_limit: The second coefficient limit.
    :return: A tuple with the final coefficients.
    """
    max_chain = -1
    result = (-1, -1)
    coefficients = [(a, b) for a in range(first_limit)
                    for b in range(second_limit + 1)]
    for a, b in coefficients:
        primes = get_primes_chain(a, b)
        if check_argv("debug") and len(primes) > 0:
            print(f"{len(primes)} ({a}, {b}) -> {primes}")
        if max_chain < len(primes):
            max_chain = len(primes)
            result = (a, b)
    return result


def main():
    """
    Main entry for the problem solver.
    """
    a, b = find_max_chain(LIMIT, LIMIT)
    primes = get_primes_chain(a, b)

    # Show the final result of the problem:
    print(f"The product of the coefficients, {a} and {b}, is {a * b}.")
    print(f"Primes chain ({len(primes)}) = {primes}")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
