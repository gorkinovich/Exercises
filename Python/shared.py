"""
Generic shared functions.
"""
import sys
import math
import bisect

from PE001 import is_multiple
from PE002 import fibonacci_generator
from PE007 import primes_generator
from PE021 import get_divisors

######################################################################
# Aliases
######################################################################

is_multiple = is_multiple
fibonacci_generator = fibonacci_generator
primes_generator = primes_generator
get_divisors = get_divisors


######################################################################
# Functions
######################################################################

def check_argv(option):
    """
    Checks if an option exists in the command's arguments.
    :param option: The option to check.
    :return: True if the option exists, otherwise False.
    """
    return (len(sys.argv) > 1) and (option in sys.argv)


def binary_search(container, victim, on_key=None):
    """
    Search a value inside a container.
    :param container: The container to check.
    :param victim: The value to search.
    :param on_key: The key selector for the value.
    :return: The index of the value if found, otherwise None.
    """
    index = bisect.bisect_left(container, victim, key=on_key)
    if index < len(container) and container[index] == victim:
        return index
    else:
        return None


def factorial(number):
    """
    Gets the factorial for a number.
    :param number: The number to check.
    :return: The factorial of the number.
    """
    if number <= 1:
        return 1
    else:
        return math.prod(range(2, number + 1))


######################################################################
# Classes
######################################################################

class PrimesGenerator:
    """
    This type represents a generator of prime numbers with memory.
    """

    __primes = [1]

    @classmethod
    def next_prime(cls):
        """
        Gets the next prime to calculate.
        :return: The next prime number.
        """
        # Sets the initial value to check:
        victim = cls.__primes[-1] + 1
        while True:
            # Check if the current candidate is a prime number:
            is_prime = True
            limit = math.trunc(math.sqrt(victim)) + 1
            for prime in cls.__primes[1:]:
                if prime > limit:
                    break
                if is_multiple(victim, prime):
                    is_prime = False
                    break
            # If the candidate is a prime number send it back:
            if is_prime:
                cls.__primes.append(victim)
                return victim
            # Select the next candidate to check:
            victim += 1

    @classmethod
    def is_prime(cls, candidate):
        """
        Checks if a number is a prime number or not.
        :param candidate: The number to check.
        :return: True if the number is prime.
        """
        while cls.__primes[-1] < candidate:
            cls.next_prime()
        return binary_search(cls.__primes, candidate) is not None

    @classmethod
    def to_string(cls):
        """
        Converts the content of the class into a string.
        :return: A string with the content.
        """
        return str(cls.__primes)

    @classmethod
    def reset(cls):
        """
        Resets the current static data of the generator.
        """
        cls.__primes = [1]

    def __init__(self):
        """
        Initializes the object instance.
        """
        self.index = 0

    def __iter__(self):
        """
        Gets an iterator over a sequence of prime numbers.
        """
        self.index = 0
        return self

    def __next__(self):
        """
        Gets the next prime number in the sequence.
        """
        while self.index >= len(PrimesGenerator.__primes):
            PrimesGenerator.next_prime()
        number = PrimesGenerator.__primes[self.index]
        self.index += 1
        return number
