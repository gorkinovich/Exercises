"""
Problem 24: Lexicographic permutations
URL: https://projecteuler.net/problem=24

    A permutation is an ordered arrangement of objects. For
    example, 3124 is one possible permutation of the digits
    1, 2, 3 and 4. If all of the permutations are listed
    numerically or alphabetically, we call it lexicographic
    order. The lexicographic permutations of 0, 1 and 2 are:

        012   021   102   120   201   210

    What is the millionth lexicographic permutation of the
    digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
"""
import itertools

######################################################################
# Constants
######################################################################

GOAL = 1_000_000
ITEMS = "0123456789"


######################################################################
# Functions
######################################################################

def main():
    """
    Main entry for the problem solver.
    """
    values = [''.join(item) for item in itertools.permutations(ITEMS)]
    result = values[GOAL - 1]

    # Show the final result of the problem:
    print(f"The millionth lexicographic permutation is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
