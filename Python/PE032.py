"""
Problem 32: Pandigital products
URL: https://projecteuler.net/problem=32

    We shall say that an n-digit number is pandigital if it makes
    use of all the digits 1 to n exactly once; for example, the
    5-digit number, 15234, is 1 through 5 pandigital.

    The product 7254 is unusual, as the identity, 39 × 186 = 7254,
    containing multiplicand, multiplier, and product is 1 through
    9 pandigital.

    Find the sum of all products whose multiplicand / multiplier
    / product identity can be written as a 1 through 9 pandigital.

    HINT: Some products can be obtained in more than one way so
    be sure to only include it once in your sum.
"""
import itertools
from shared import check_argv

######################################################################
# Constants
######################################################################

DIGITS = [str(digit) for digit in range(1, 10)]


######################################################################
# Functions
######################################################################

def get_candidates(digits):
    """
    Gets all the candidate numbers of the problem.
    :param digits: The digits to work with.
    :return: An ordered list with tuples, where the first
    component is the number, and the second is a set of digits.
    """
    result = [(int(item), {item}) for item in digits]
    for size in range(2, (len(digits) // 2) + 1):
        numbers = [''.join(item) for item
                   in itertools.permutations(digits, size)
                   if len(item) == len(set(item))]
        result.extend((int(item), set(item)) for item in numbers)
    return result


def find_triplets(digits, candidates):
    result = []
    initial_digits = set(digits)
    # Check all the candidate numbers, getting the number, the set
    # of digits, and updating the set with the remaining digits:
    for i in range(len(candidates) - 1):
        left_number, left_digits = candidates[i]
        current_digits = initial_digits - left_digits
        # Check all the following candidate numbers, getting the
        # number, the set of digits, and also updating the set
        # with the remaining digits to check, when the right
        # candidate exists in the set of digits:
        for j in range(i + 1, len(candidates)):
            right_number, right_digits = candidates[j]
            if right_digits <= current_digits:
                final_digits = current_digits - right_digits
                # Get the product and the digits to check if
                # the number is equal to the remaining digits:
                product = left_number * right_number
                product_digits = set(digit for digit in str(product))
                if len(str(product)) == len(product_digits) \
                        and final_digits == product_digits:
                    result.append((left_number, right_number, product))
    return result


def main():
    """
    Main entry for the problem solver.
    """
    candidates = get_candidates(DIGITS)
    triplets = find_triplets(DIGITS, candidates)
    result = sum(number for _, _, number in triplets)

    if check_argv("show"):
        print(f"{candidates = }")
        print(f"{triplets = }")

    # Show the final result of the problem:
    print(f"The sum of all pandigital products is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
