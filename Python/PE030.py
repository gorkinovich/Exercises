"""
Problem 30: Digit fifth powers
URL: https://projecteuler.net/problem=30

    Surprisingly there are only three numbers that can be written
    as the sum of fourth powers of their digits:

        1634 = 1^4 + 6^4 + 3^4 + 4^4
        8208 = 8^4 + 2^4 + 0^4 + 8^4
        9474 = 9^4 + 4^4 + 7^4 + 4^4

    As 1 = 1^4 is not a sum it is not included.

    The sum of these numbers is 1634 + 8208 + 9474 = 19316.

    Find the sum of all the numbers that can be written as the
    sum of fifth powers of their digits.
"""

######################################################################
# Constants
######################################################################

GOAL = 5


######################################################################
# Functions
######################################################################

def find_numbers(goal):
    """
    Finds the numbers required for the problem.
    :param goal: The size in digits of the numbers to check.
    :return: A set with the numbers that can be written as the
    sum of the N-th powers of their N-digits.
    """
    result = set()
    start = 10 ** (goal - 1)
    limit = start * 10
    for victim in range(start, limit):
        number = sum(int(c) ** goal for c in str(victim))
        if number == victim:
            result.add(victim)
    return result


def main():
    """
    Main entry for the problem solver.
    """
    result = find_numbers(GOAL)

    # Show the final result of the problem:
    message = "The sum of all the numbers that can be written as the "
    message += f"sum of fifth powers of their digits are {result}."
    print(message)


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
