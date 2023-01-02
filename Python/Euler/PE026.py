"""
| **Problem 26:** Reciprocal cycles
| URL: https://projecteuler.net/problem=26
::

    A unit fraction contains 1 in the numerator. The decimal
    representation of the unit fractions with denominators
    2 to 10 are given:

        1/2  = 0.5
        1/3  = 0.(3)
        1/4  = 0.25
        1/5  = 0.2
        1/6  = 0.1(6)
        1/7  = 0.(142857)
        1/8  = 0.125
        1/9  = 0.(1)
        1/10 = 0.1

    Where 0.1(6) means 0.166666..., and has a 1-digit recurring
    cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

    Find the value of d < 1000 for which 1/d contains the
    longest recurring cycle in its decimal fraction part.

:author: Gorka Suárez García
:copyright: (c) 2022, Gorka Suárez García
"""

######################################################################
# Constants
######################################################################

LIMIT = 1000


######################################################################
# Functions
######################################################################

def division_generator(left, right):
    """
    Gets the values of a given division, step by step.
    :param left: The left value.
    :param right: The right value.
    :return: A tuple with the integer division result, the remainder,
    the current left operand and the current right operand.
    """
    while True:
        if left < right:
            division, remainder = 0, left
        else:
            division, remainder = divmod(left, right)
        yield division, remainder, left, right
        if remainder == 0:
            return
        left = remainder * 10


def extract_decimal_parts(table, initial_key, cycle_key):
    """
    Extracts the decimal ending cycle part from the initial part.
    :param table: The table with the decimal digits.
    :param initial_key: The initial remainder key.
    :param cycle_key: The initial remainder cycle key.
    :return: A tuple with two string, where the first component is
    the initial decimal part, and the second component is the cycle
    ending part of the decimals.
    """
    def append(x, v): x.append(ord('0') + v)
    decimal_part = bytearray()
    ending_part = bytearray()
    key = initial_key
    # Here we'll extract the initial decimals outside the cycle:
    while (key != 0) and (key != cycle_key):
        digit, key = table[key]
        append(decimal_part, digit)
    # If the decimals don't stop, we'll extract the cycle:
    if key != 0:
        loop = True
        while loop:
            digit, key = table[key]
            append(ending_part, digit)
            if key == cycle_key:
                loop = False
    # Return the result as a tuple of strings:
    decimal_part = str(decimal_part, "utf-8")
    ending_part = str(ending_part, "utf-8")
    return decimal_part, ending_part


def get_division_parts(left, right):
    table = {}
    generator = division_generator(left, right)
    integer_part, remainder, _, _ = next(generator)
    initial_key = remainder
    cycle_key = None
    # Calculate the decimal digits of the division:
    while remainder:
        # We'll check if the current remainder exists as a key in the table,
        # if so we'll set the cycle key and exit the current loop:
        if remainder in table:
            cycle_key = remainder
            break
        else:
            # We need to save the previous remainder as the current key, and
            # then calculate the current digit and remainder of the division,
            # to set in the table the obtained values:
            key = remainder
            digit, remainder, _, _ = next(generator)
            table[key] = (digit, remainder)
    # Return the result as a tuple of strings:
    decimal_part, ending_part = extract_decimal_parts(table, initial_key, cycle_key)
    return str(integer_part), decimal_part, ending_part


def find_number(limit):
    """
    Finds the number with the longest recurring cycle in its decimal
    fraction part in the value (1 / number).
    :param limit: The limit number in the range to check.
    :return: The number with the longest recurring cycle.
    """
    result = (0, 0)
    for number in range(2, limit):
        parts = get_division_parts(1, number)
        if result[1] < len(parts[2]):
            result = (number, len(parts[2]))
    return result[0]


def main():
    """
    Main entry for the problem solver.
    """
    result = find_number(LIMIT)

    # Show the final result of the problem:
    print(f"The value 1/{result} contains the longest recurring cycle in its decimal fraction part.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()


######################################################################
# Test Functions
######################################################################

def test():
    test_division(11)
    print()
    test_generator(2041, 20)


def test_division(limit):
    for i in range(2, limit):
        a, b, c = get_division_parts(1, i)
        if c:
            print(f"{i} = {a}.{b}({c})")
        else:
            print(f"{i} = {a}.{b}")


def test_generator(left, right):
    parts = get_division_parts(left, right)
    print(parts)
    if parts[2] == '':
        for value in division_generator(left, right):
            print(value)
