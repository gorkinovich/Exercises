"""
Problem 21: Amicable numbers
URL: https://projecteuler.net/problem=21

    Let d(n) be defined as the sum of proper divisors of n
    (numbers less than n which divide evenly into n).

    If d(a) = b and d(b) = a, where a <> b, then a and b are an
    amicable pair and each of a and b are called amicable numbers.

    For example, the proper divisors of 220 are 1, 2, 4, 5, 10,
    11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper
    divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

    Evaluate the sum of all the amicable numbers under 10000.
"""
from shared import is_multiple

######################################################################
# Constants
######################################################################

LIMIT = 10_000


######################################################################
# Functions
######################################################################

def get_divisors(number):
    """
    Gets the divisors of a number.
    :param number: The number to check.
    :return: A list with the divisors.
    """
    result = [1]
    if number > 1:
        tail = [number]
        # Check each number up to the victim to obtain a divisor
        # and the mirror divisor obtained with the division:
        for divisor in range(2, number):
            if divisor >= tail[-1]:
                break
            if is_multiple(number, divisor):
                result.append(divisor)
                tail.append(number // divisor)
        # Add the mirrored divisors to the result:
        tail.reverse()
        result += tail
    return result


def sum_proper_divisors(number):
    """
    Sums all the proper divisors of a number.
    :param number: The number to check.
    :return: The sum of the proper divisors.
    """
    if number > 1:
        divisors = get_divisors(number)
        return sum(divisors[:-1])
    else:
        return 0


def check_amicable_condition(number):
    """
    Checks if a number has an amicable number.
    :param number: The number to check.
    :return: A tuple with the amicable numbers or False.
    """
    mirror = sum_proper_divisors(number)
    if (mirror != number) and (sum_proper_divisors(mirror) == number):
        if number <= mirror:
            return number, mirror
        else:
            return mirror, number
    else:
        return False


def get_amicable_numbers(limit):
    """
    Gets all the amicable numbers under a limit number.
    :param limit: The limit number.
    :return: A set with all the amicable numbers.
    """
    result = set()
    numbers = set(range(1, limit))
    while numbers:
        victim = numbers.pop()
        match check_amicable_condition(victim):
            case (a, b):
                numbers -= {a, b}
                result |= {a, b}
    return result


def main():
    """
    Main entry for the problem solver.
    """
    numbers = get_amicable_numbers(LIMIT)
    result = sum(numbers)

    # Show the final result of the problem:
    print(f"The sum of all the amicable numbers under {LIMIT} is {result}.")
    print(f"Amicable numbers: {sorted(numbers)}")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
