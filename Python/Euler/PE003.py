"""
| **Problem 3:** Largest prime factor
| URL: https://projecteuler.net/problem=3
::

    The prime factors of 13195 are 5, 7, 13 and 29.

    What is the largest prime factor of the number 600851475143?

:author: Gorka Suárez García
:copyright: (c) 2022, Gorka Suárez García
"""

######################################################################
# Constants
######################################################################

CANDIDATE = 600_851_475_143


######################################################################
# Functions
######################################################################

def reduce_division(victim, divisor):
    """
    Reduce a number with a division multiple times.
    :param victim: The number to reduce.
    :param divisor: The divisor number.
    :return: The reduced number.
    """
    while victim % divisor == 0:
        victim //= divisor
    return victim


def get_prime_factors(victim):
    """
    Gets the prime factors of a number.
    :param victim: The number to check.
    :return: A list with the factors.
    """
    divisor = 2
    factors = []
    # Checks if the numbers is negative or positive:
    if victim < 0:
        victim = abs(victim)
        factors.append(-1)
    else:
        factors.append(1)
    # Get all the factors of the number:
    while divisor <= victim:
        next_victim = reduce_division(victim, divisor)
        if next_victim < victim:
            victim = next_victim
            factors.append(divisor)
        divisor += 1
    # Exit with the list of factors:
    return factors


def main():
    """
    Main entry for the problem solver.
    """
    factors = get_prime_factors(CANDIDATE)
    result = factors[-1]

    # Show the final result of the problem:
    print(f"The largest prime factor of {CANDIDATE} is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
