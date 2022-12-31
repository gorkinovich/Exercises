"""
| **Problem 28:** Number spiral diagonals
| URL: https://projecteuler.net/problem=28
::

    Starting with the number 1 and moving to the right in a
    clockwise direction a 5 by 5 spiral is formed as follows:

        21 22 23 24 25    21  .  .  . 25
        20  7  8  9 10     .  7  .  9  .
        19  6  1  2 11     .  .  1  .  .
        18  5  4  3 12     .  5  .  3  .
        17 16 15 14 13    17  .  .  . 13

    It can be verified that the sum of the numbers on the
    diagonals is 101.

    What is the sum of the numbers on the diagonals in a
    1001 by 1001 spiral formed in the same way?

:author: Gorka Suárez García
:copyright: (c) 2022, Gorka Suárez García
"""

######################################################################
# Constants
######################################################################

SIZE = 1001
OFFSET_INITIAL = 2
OFFSET_STEP = 2
STEPS_PER_CYCLE = 4


######################################################################
# Functions
######################################################################

def sum_diagonal_numbers(size):
    """
    Sums the diagonal numbers in a spiral inside a square.
    :param size: The side size of the square.
    :return: The final sum of the diagonal numbers.
    """
    if ((size - 1) % 2) != 0:
        raise ValueError
    # Set the initial data of the algorithm:
    result = 1
    current = 1
    offset = OFFSET_INITIAL
    # Visit all the diagonal positions from the center to the
    # outside of the square, calculating the numbers of each
    # corner in each cycle, and add them to the result:
    for cycle in range((size - 1) // 2):
        for step in range(STEPS_PER_CYCLE):
            current += offset
            result += current
        offset += OFFSET_STEP
    # Return the final obtained result:
    return result


def main():
    """
    Main entry for the problem solver.
    """
    result = sum_diagonal_numbers(SIZE)

    # Show the final result of the problem:
    print(f"The sum of the numbers on the diagonals in a {SIZE}^2 spiral is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
