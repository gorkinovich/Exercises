"""
Problem 14: Longest Collatz sequence
URL: https://projecteuler.net/problem=14

    The following iterative sequence is defined for the set of
    positive integers:

      n -> n/2 (n is even)
      n -> 3n + 1 (n is odd)

    Using the rule above and starting with 13, we generate the
    following sequence:

      13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

    It can be seen that this sequence (starting at 13 and
    finishing at 1) contains 10 terms. Although it has not
    been proved yet (Collatz Problem), it is thought that
    all starting numbers finish at 1.

    Which starting number, under one million, produces the
    longest chain?

    NOTE: Once the chain starts the terms are allowed to go
    above one million.
"""

######################################################################
# Constants
######################################################################

LIMIT = 1_000_000


######################################################################
# Classes
######################################################################

class Sequence:
    """
    This type represents the problems sequence of numbers.
    """

    def __init__(self):
        """
        Initializes the object instance.
        """
        self.__cache = {1: self.node(1)}

    def length(self, number):
        """
        Gets the length of the chain for a number inside the sequence.
        :param number: The number to check.
        :return: The length of the chain for the number.
        """
        self.check(number)
        return self.__cache[number]['length']

    def check(self, number):
        """
        Checks the cache of the sequence to create the missing nodes.
        :param number: The start number to check.
        """
        if number > 1:
            # Seek for the first number that exists in the cache:
            stack = []
            while not self.__cache.get(number, None):
                next_number = self.formula(number)
                stack.append(self.node(number, next_number))
                number = next_number
            # Add to the cache all the nodes in the stack:
            previous = self.__cache[number]
            while stack:
                current = stack.pop()
                current['length'] += previous['length']
                self.__cache[current['number']] = current
                previous = current

    @staticmethod
    def node(number, after=None, length=1):
        """
        Makes a new node inside the cache of the sequence.
        :param number: The current number.
        :param after: The next number.
        :param length: The length of the chain.
        :return: The new node created.
        """
        return {'number': number,
                'next': after,
                'length': length}

    @staticmethod
    def formula(number):
        """
        Gets the next number in the sequence.
        :param number: The current number.
        :return: The next number.
        """
        if number <= 1:
            return 1
        elif number % 2 == 0:
            return number // 2
        else:
            return 3 * number + 1


######################################################################
# Functions
######################################################################

def longest_number(limit):
    """
    Calcs the number in the sequence with the longest chain.
    :param limit: The upper limit in the range.
    :return: The number with the longest chain.
    """
    length = 1
    candidate = 1
    sequence = Sequence()
    for number in range(1, limit):
        current_length = sequence.length(number)
        if length < current_length:
            length = current_length
            candidate = number
    return candidate


def main():
    """
    Main entry for the problem solver.
    """
    result = longest_number(LIMIT)

    # Show the final result of the problem:
    print(f"The starting number, under {LIMIT}, with the longest chain is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
