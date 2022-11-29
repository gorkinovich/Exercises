######################################################################
# AER 100: Kaprekar constant
# URL: https://www.aceptaelreto.com/problem/statement.php?id=100
######################################################################
import sys

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


def load_numbers(path):
    """
    Loads a list of numbers from a file.
    :param path: The path of the file with the numbers.
    :return: A list of strings with the numbers; otherwise an empty list.
    """

    # Initialize a list of numbers:
    numbers = []

    # Obtain the list of numbers:
    with open(path, "r") as file:
        lines = [line.strip() for line in file.readlines()]
        if len(lines) > 1:
            size = int(lines[0])
            numbers = lines[1:size + 1]

    # Exit with the list of numbers:
    return numbers


def exec_numbers(numbers):
    """
    Executes the Kaprekar constant algorithm for a list of numbers.
    :param numbers: A list of strings with the numbers to check.
    """
    for number in numbers:
        try:
            print(kaprekar(number, 0))
        except Exception as error:
            print(f"ERROR: Invalid input {number} ({error.__class__.__name__})")
            print(error)


def main():
    """
    Main entry for the Kaprekar constant problem.
    """
    if len(sys.argv) > 1:
        if sys.argv[1] == "-n":
            exec_numbers(sys.argv[2:])
        else:
            for path in sys.argv[1:]:
                if numbers := load_numbers(path):
                    exec_numbers(numbers)
                else:
                    print(f"ERROR: Invalid file {path}")

    else:
        print("ERROR: Input files needed")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
