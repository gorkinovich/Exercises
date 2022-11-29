"""
AER 100: Kaprekar constant
URL: https://www.aceptaelreto.com/problem/statement.php?id=100
"""
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
            print(f"({error.__class__.__name__}) {error}")


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


def input_numbers():
    """
    Loads a list of numbers from the console input.
    :return: A list of strings with the numbers; otherwise an empty list.
    """

    # Initialize a list of numbers:
    numbers = []

    # Obtain the list of numbers:
    try:
        size = int(input())
        numbers = [input() for _ in range(size)]
    except Exception as error:
        print(f"ERROR: Invalid console input")
        print(f"({error.__class__.__name__}) {error}")

    # Exit with the list of numbers:
    return numbers


def exec_files(paths):
    """
    Executes the Kaprekar constant algorithm for a list of files.
    :param paths: A list of strings with the files to check.
    """
    for path in paths:
        if numbers := load_numbers(path):
            exec_numbers(numbers)
        else:
            print(f"ERROR: Invalid file {path}")


def exec_input():
    """
    Executes the Kaprekar constant algorithm for a list of inputs.
    """
    if numbers := input_numbers():
        exec_numbers(numbers)


def main():
    """
    Main entry for the Kaprekar constant problem.
    """
    argc = len(sys.argv)
    if argc < 2:
        exec_input()
    elif sys.argv[1] == "-i":
        exec_input()
    elif sys.argv[1] == "-n":
        exec_numbers(sys.argv[2:])
    elif sys.argv[1] == "-f":
        exec_files(sys.argv[2:])
    else:
        exec_files(sys.argv[1:])


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
