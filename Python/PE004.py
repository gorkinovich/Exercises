"""
Problem 4: Largest palindrome product
URL: https://projecteuler.net/problem=4

    A palindromic number reads the same both ways. The largest
    palindrome made from the product of two 2-digit numbers is
    9009 = 91 × 99.

    Find the largest palindrome made from the product of two
    3-digit numbers.
"""
import time

######################################################################
# Constants
######################################################################

START = 100
LIMIT = 1000


######################################################################
# Functions
######################################################################

def is_palindrome(victim):
    """
    Checks if a value is a palindrome string.
    :param victim: The value to check.
    :return: True if the value is a palindrome.
    """
    if type(victim) != type(str):
        victim = str(victim)

    return victim == victim[::-1]


def find_palindrome_last(start, end):
    """
    Finds the largest palindrome number from the product of
    two numbers inside a range of numbers.
    :param start: The start number of the range.
    :param end: The limit number of the range.
    :return: A triplet with the palindrome and the two numbers of
    the multiplication if there is a palindrome; otherwise None.
    """
    candidates = range(start, end)
    numbers = [(n * m, n, m)
               for n in candidates
               for m in candidates
               if is_palindrome(n * m)]
    if numbers:
        return sorted(numbers)[-2]
    else:
        return None


def find_palindrome(start, end):
    """
    Finds the largest palindrome number from the product of
    two numbers inside a range of numbers.
    :param start: The start number of the range.
    :param end: The limit number of the range.
    :return: A triplet with the palindrome and the two numbers of
    the multiplication if there is a palindrome; otherwise None.
    """
    result = 0
    candidates = range(start, end)
    for n in candidates:
        for m in candidates:
            number = n * m
            if is_palindrome(number) and \
                    (not result or result[0] < number):
                result = (number, n, m)
    return result


def main(on_execute):
    """
    Main entry for the problem solver.
    """
    result, n, m = on_execute(START, LIMIT)

    # Show the final result of the problem:
    message = "The largest palindrome made from the "
    message += "product of two 3-digit numbers is "
    if result:
        message += f"{n} * {m} = {result}."
    else:
        message += f"none."

    print(message)


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main(find_palindrome)


######################################################################
# Test Functions
######################################################################

def test():
    S1 = time.time()
    V1 = find_palindrome(START, LIMIT)
    S2 = time.time()
    V2 = find_palindrome_last(START, LIMIT)
    S3 = time.time()
    print(f"Loop: {V1} [{S2 - S1}]")
    print(f"Sort: {V2} [{S3 - S2}]")
