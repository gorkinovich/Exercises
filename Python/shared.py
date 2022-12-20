"""
Generic shared functions.
"""
import sys

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


######################################################################
# Classes
######################################################################

class PrimesGenerator:
    """
    This type represents a generator of prime numbers with memory.
    """

    __primes = [1]
    __generator = primes_generator()

    @staticmethod
    def reset():
        """
        Resets the current static data of the generator.
        """
        PrimesGenerator.__primes = [1]
        PrimesGenerator.__generator = primes_generator()

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
            PrimesGenerator.__primes.append(next(PrimesGenerator.__generator))
        number = PrimesGenerator.__primes[self.index]
        self.index += 1
        return number
