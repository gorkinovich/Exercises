"""
AER 100: Kaprekar constant
URL: https://www.aceptaelreto.com/problem/statement.php?id=100

Example commands.:
python AER100.py -d 3524 1111 1121 6174 1893
python AER100.py -f data/kaprekar.txt
"""
import loader


######################################################################
# Constants
######################################################################

INVALID_NUMBER_COUNT = 8
MAX_NUMBER_LENGTH = 4
KAPREKAR_CONSTANT = "6174"


######################################################################
# Functions
######################################################################

def invalid_number(number):
    """
    Checks if the current number is an invalid one, like 1111, 2222, etc.
    :param number: The number to check for the Kaprekar algorithm.
    :return: True if the number is invalid; otherwise False.
    """
    return len(number) > MAX_NUMBER_LENGTH or \
           len(set(number)) == 1


def kaprekar(number, count):
    """
    Calculates the number of steps to reach the Kaprekar constant.
    :param number: An string with the number to check.
    :param count: The current number of steps.
    :return: The number of steps to reach the Kaprekar constant.
    """
    if len(number) < MAX_NUMBER_LENGTH:
        number = number.rjust(MAX_NUMBER_LENGTH, "0")

    if invalid_number(number):
        return INVALID_NUMBER_COUNT
    elif number == KAPREKAR_CONSTANT:
        return count
    else:
        minor = int("".join(sorted(number)))
        mayor = int("".join(sorted(number, reverse=True)))
        return kaprekar(str(mayor - minor), count + 1)


def exec_numbers(numbers):
    """
    Executes the Kaprekar constant algorithm for a list of numbers.
    :param numbers: A list of strings with the numbers to check.
    """
    for number in numbers:
        try:
            print(kaprekar(number, 0))
        except Exception as error:
            print(f"ERROR: Invalid input {number}")
            print(f"({type(error).__name__}) {error}")


def on_lines(lines):
    """
    The on lines loaded event for the problem.
    :param lines: The loaded lines.
    :return: The final lines for the problem.
    """
    if len(lines) > 1:
        size = int(lines[0])
        return lines[1:size + 1]
    else:
        return lines


def main():
    """
    Main entry for the Kaprekar constant problem.
    """
    loader.main(exec_numbers, on_lines,
                loader.InputMode.SIZE_FIRST)


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
