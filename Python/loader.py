import sys
import enum


######################################################################
# Enumerations
######################################################################

class InputMode(enum.Enum):
    SIZE_FIRST = enum.auto()
    ZERO_ENDING = enum.auto()


######################################################################
# Functions
######################################################################

def get_input(input_mode=InputMode.SIZE_FIRST):
    """
    Gets a list of lines from the console input.
    :param input_mode: The input mode to load the data.
    :return: A list of strings; otherwise an empty list.
    """
    # Initialize a list of lines:
    lines = []
    # Obtain the list of lines:
    try:
        match input_mode:
            # This mode gets first the size of the data first:
            case InputMode.SIZE_FIRST:
                lines.append(input().strip())
                size = int(lines[0])
                for _ in range(size):
                    lines.append(input().strip())
            # This mode gets data until a zero is received:
            case InputMode.ZERO_ENDING:
                loop = True
                while loop:
                    lines.append(input().strip())
                    if lines[-1] == "0":
                        loop = False
    except Exception as error:
        print(f"ERROR: Invalid console input")
        print(f"({type(error).__name__}) {error}")
    # Exit with the list of lines:
    return lines


def exec_input(problem, on_lines, input_mode):
    """
    Executes a problem for a list of inputs.
    :param problem: The problem to execute.
    :param on_lines: The on lines loaded event.
    :param input_mode: The input mode to load the data.
    """
    if content := on_lines(get_input(input_mode)):
        problem(content)


def load_input(path):
    """
    Loads a list of strings from a file.
    :param path: The path of the file with the data.
    :return: A list of strings with the data; otherwise an empty list.
    """
    # Initialize a list of strings:
    lines = []
    # Obtain the list of strings:
    try:
        with open(path, "r") as file:
            lines = [line.strip() for line in file.readlines()]
    except Exception as error:
        print(f"ERROR: Invalid input file {path}")
        print(f"({type(error).__name__}) {error}")
    # Exit with the list of strings:
    return lines


def exec_files(problem, on_lines, paths):
    """
    Executes a problem for a list of files.
    :param problem: The problem to execute.
    :param on_lines: The on lines loaded event.
    :param paths: A list of file paths to check.
    """
    for path in paths:
        if content := on_lines(load_input(path)):
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


def main(problem, on_lines, input_mode=InputMode.SIZE_FIRST):
    """
    Main entry for the data loader for problems.
    :param problem: The problem to execute.
    :param on_lines: The on lines loaded event.
    :param input_mode: The input mode to load the data.
    """
    argc = len(sys.argv)
    if argc < 2:
        exec_input(problem, on_lines, input_mode)
    elif sys.argv[1] == "-h":
        exec_help()
    elif sys.argv[1] == "-i":
        exec_input(problem, on_lines, input_mode)
    elif sys.argv[1] == "-f":
        exec_files(problem, on_lines, sys.argv[2:])
    elif sys.argv[1] == "-d":
        problem(sys.argv[2:])
    else:
        problem(sys.argv[1:])
