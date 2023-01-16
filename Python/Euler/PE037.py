"""
| **Problem 37:**
| URL: https://projecteuler.net/problem=37
::

    The number 3797 has an interesting property. Being prime
    itself, it is possible to continuously remove digits from
    left to right, and remain prime at each stage: 3797, 797,
    97, and 7. Similarly we can work from right to left: 3797,
    379, 37, and 3.

    Find the sum of the only eleven primes that are both
    truncatable from left to right and right to left.

    NOTE: 2, 3, 5, and 7 are not considered to be truncatable
    primes.

:author: Gorka Suárez García
:copyright: (c) 2023, Gorka Suárez García
"""
from shared import PrimesGenerator, check_argv

######################################################################
# Constants
######################################################################

GOAL = 11


######################################################################
# Functions
######################################################################

def check_truncatable(victim):
    """
    Checks if a number is a truncatable prime number.
    :param victim: The number to check.
    :return: True if the number is truncatable.
    """
    def not_prime(victim):
        return not PrimesGenerator.is_prime(int(victim))
    # Get the string value of the number and check it has several digits:
    text = str(victim)
    size = len(text)
    if size <= 1:
        return False
    # Check all the truncated obtained numbers from the left and right:
    for index in range(1, size):
        if not_prime(text[index:size]) or not_prime(text[0:size - index]):
            return False
    # All the number checked were primes:
    return True


def find_primes(goal):
    """
    Finds some amount of truncatable prime numbers.
    :param goal: The amount to find.
    :return: A list with the numbers.
    """
    result = []
    while len(result) < goal:
        candidate = PrimesGenerator.next_prime()
        if check_truncatable(candidate):
            result.append(candidate)
    return result


def main():
    """
    Main entry for the problem solver.
    """
    primes = find_primes(GOAL)
    result = sum(primes)

    if check_argv("show"):
        print(f"{primes = }")

    # Show the final result of the problem:
    print(f"The sum of the primes that are both truncatable is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
