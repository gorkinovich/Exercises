"""
Problem 7: 10001st prime
URL: https://projecteuler.net/problem=7

    By listing the first six prime numbers: 2, 3, 5, 7, 11, and
    13, we can see that the 6th prime is 13.

    What is the 10,001st prime number?
"""
import math
from shared import is_multiple


######################################################################
# Constants
######################################################################

CANDIDATE = 10_001


######################################################################
# Functions
######################################################################

def primes_generator():
    """
    This generator returns a sequence of prime numbers.
    :return: The prime numbers generator.
    """
    # Send the first prime and set the state of the loop:
    yield 2
    primes = [2]
    victim = 3
    while True:
        # Check if the current candidate is a prime number:
        is_prime = True
        limit = math.trunc(math.sqrt(victim)) + 1
        for prime in primes:
            if prime > limit:
                break
            if is_multiple(victim, prime):
                is_prime = False
                break
        # If the candidate is a prime number send it back:
        if is_prime:
            primes.append(victim)
            yield victim
        # Select the next candidate to check:
        victim += 1


def find_prime(index):
    """
    Finds a prime number at a given index.
    :param index: The index position of the prime number.
    :return: The prime number at the given index.
    """
    result = 0
    primes = primes_generator()
    for _ in range(index):
        result = next(primes)
    return result


def main():
    """
    Main entry for the problem solver.
    """
    result = find_prime(CANDIDATE)

    # Show the final result of the problem:
    print(f"The 10,001st prime number is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
