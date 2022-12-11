"""
Problem 10: Summation of primes
URL: https://projecteuler.net/problem=10

    The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

    Find the sum of all the primes below two million.
"""
from shared import primes_generator


######################################################################
# Constants
######################################################################

LIMIT = 2_000_000


######################################################################
# Functions
######################################################################

def sum_primes(limit):
    """
    Sums all the primes bellow a limit number.
    :param limit: The limit number.
    :return: The sum of all the primes.
    """
    result = 0
    primes = primes_generator()
    current = next(primes)
    while current < limit:
        result += current
        current = next(primes)
    return result


def main():
    """
    Main entry for the problem solver.
    """
    result = sum_primes(LIMIT)

    # Show the final result of the problem:
    print(f"The sum of all the primes below {LIMIT} is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
