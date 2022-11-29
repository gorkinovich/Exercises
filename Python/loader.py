import sys


######################################################################
# Functions
######################################################################


def get_input():
    """
    Gets a list of lines from the console input.
    :return: A list of strings; otherwise an empty list.
    """

    # Initialize a list of lines:
    lines = []

    # Obtain the list of lines:
    try:
        size = int(input())
        lines = [input() for _ in range(size)]
    except Exception as error:
        print(f"ERROR: Invalid console input")
        print(f"({type(error).__name__}) {error}")

    # Exit with the list of lines:
    return lines


def exec_input(problem):
    """
    Executes a problem for a list of inputs.
    :param problem: The problem to execute.
    """
    if content := get_input():
        problem(content)


def load_input(path):
    """
    Loads a list of strings from a file.
    :param path: The path of the file with the data.
    :return: A list of strings with the data; otherwise an empty list.
    """

    # Initialize a list of strings:
    numbers = []

    # Obtain the list of strings:
    try:
        with open(path, "r") as file:
            lines = [line.strip() for line in file.readlines()]
            if len(lines) > 1:
                size = int(lines[0])
                numbers = lines[1:size + 1]
    except Exception as error:
        print(f"ERROR: Invalid input file {path}")
        print(f"({type(error).__name__}) {error}")

    # Exit with the list of strings:
    return numbers


def exec_files(problem, paths):
    """
    Executes a problem for a list of files.
    :param problem: The problem to execute.
    :param paths: A list of file paths to check.
    """
    for path in paths:
        if content := load_input(path):
            problem(content)
        else:
            print(f"ERROR: Invalid file {path}")


def exec_help():
    """
    Shows the command help.
    """
    print(f"{sys.argv[0]} [options]")
    print("-d values = Input loaded from args")
    print("-f paths  = Input loaded from files")
    print("-i        = Input loaded from terminal")
    print("-h        = Shows this help")


def main(problem):
    """
    Main entry for the data loader for problems.
    :param problem: The problem to execute.
    """
    argc = len(sys.argv)
    if argc < 2:
        exec_input(problem)
    elif sys.argv[1] == "-h":
        exec_help()
    elif sys.argv[1] == "-i":
        exec_input(problem)
    elif sys.argv[1] == "-f":
        exec_files(problem, sys.argv[2:])
    elif sys.argv[1] == "-d":
        problem(sys.argv[2:])
    else:
        problem(sys.argv[1:])
