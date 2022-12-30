"""
Problem 34: Digit factorials
URL: https://projecteuler.net/problem=34

    145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

    Find the sum of all numbers which are equal to the sum of the
    factorial of their digits.

    Note: as 1! = 1 and 2! = 2 are not sums they are not included.
"""
from shared import factorial, check_argv

######################################################################
# Constants
######################################################################

START = 3
NUMBERS = [factorial(i) for i in range(10)]
TABLE = {str(i): NUMBERS[i] for i in range(10)}


######################################################################
# Functions
######################################################################

def get_limit_number():
    """
    Gets the limit number for the problem.
    :return: The limit number for the problem.
    """
    size = 1
    digit = 9
    result = digit
    while (NUMBERS[digit] * size) >= result:
        result = result * 10 + digit
        size += 1
    return result


def check_condition(number):
    """
    Checks if a number is equal to the sum of the factorial
    of their digits.
    :param number: The number to check.
    :return: True if the condition applies.
    """
    fact_sum = 0
    for digit in str(number):
        fact_sum += TABLE[digit]
    return number == fact_sum


def find_numbers(start, limit):
    """
    Finds all the numbers that check the problem condition.
    :param start: The start number of the range.
    :param limit: The limit number of the range.
    :return: A list with the numbers that check the condition.
    """
    return [number for number in range(start, limit)
            if check_condition(number)]


def main():
    """
    Main entry for the problem solver.
    """
    limit = get_limit_number()
    numbers = find_numbers(START, limit)
    result = sum(numbers)

    if check_argv("show"):
        print(f"{limit = }")
        print(f"{numbers = }")

    # Show the final result of the problem:
    message = "The sum of all numbers which are equal to the sum"
    message += f" of the factorial of their digits is {result}."
    print(message)


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
