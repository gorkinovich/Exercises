"""
Problem 3: Largest prime factor
URL: https://projecteuler.net/problem=3

    The prime factors of 13195 are 5, 7, 13 and 29.

    What is the largest prime factor of the number 600851475143?
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


def get_factors(victim):
    """
    Gets the factors of a number.
    :param victim: The number to check.
    :return: A list with the factors.
    """
    divisor = 2
    factors = []
    # Checks if the numbers is negative or positive.
    if victim < 0:
        victim = abs(victim)
        factors.append(-1)
    else:
        factors.append(1)
    # Get all the factors of the number.
    while divisor <= victim:
        next = reduce_division(victim, divisor)
        if next < victim:
            victim = next
            factors.append(divisor)
        divisor += 1
    # Exit with the list of factors.
    return factors


def main():
    """
    Main entry for the problem solver.
    """
    factors = get_factors(CANDIDATE)
    result = factors[-1]

    # Show the final result of the problem:
    print(f"The largest prime factor of {CANDIDATE} is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
