"""
| **Problem 39:** Integer right triangles
| URL: https://projecteuler.net/problem=39
::

    If p is the perimeter of a right angle triangle with integral
    length sides, {a,b,c}, there are exactly three solutions for
    p = 120.

    {20,48,52}, {24,45,51}, {30,40,50}

    For which value of p <= 1000, is the number of solutions
    maximised?

:author: Gorka Suárez García
:copyright: (c) 2023, Gorka Suárez García
"""
import itertools
from shared import check_argv

######################################################################
# Constants
######################################################################

LIMIT = 1000


######################################################################
# Functions
######################################################################

def check_pythagoras(a, b, c):
    """
    Checks if a triplet of sides is a Pythagoras' triangle.
    :param a: The minimum side.
    :param b: The medium side.
    :param c: The maximum side.
    :return: True if the triplet is a triangle.
    """
    return ((a * a + b * b) - (c * c)) == 0


def find_solution(limit):
    """
    Finds the solution of the problem.
    :param limit: The limit value of the range.
    :return: A tuple with the perimeter and the triangles.
    """
    table = {}
    for sides in itertools.combinations(range(1, limit), 3):
        perimeter = sum(sides)
        if perimeter <= limit and check_pythagoras(*sides):
            triangles = table.get(perimeter, [])
            triangles.append(sides)
            table[perimeter] = triangles
    return max(table.items(), key=lambda x: len(x[1]))


def main():
    """
    Main entry for the problem solver.
    """
    result, triangles = find_solution(LIMIT)

    if check_argv("show"):
        print(f"{triangles = }")

    # Show the final result of the problem:
    print(f"The perimeter with the maximum number of solutions is {result}.")


######################################################################
# Functions (Slow version)
######################################################################

def get_triangles(perimeter):
    """
    Gets the Pythagoras' triangles for a perimeter.
    :param perimeter: The perimeter to check.
    :return: A list with the triangles.
    """
    result = []
    for sides in itertools.combinations(range(1, perimeter + 1), 3):
        if sum(sides) == perimeter and check_pythagoras(*sides):
            result.append(sides)
    return result


def find_solution_slow(start, limit):
    """
    Finds the solution of the problem.
    :param start: The start value of the range.
    :param limit: The limit value of the range.
    :return: A tuple with the perimeter and the triangles.
    """
    result = (0, [])
    for perimeter in range(start, limit):
        triangles = get_triangles(perimeter)
        if len(result[1]) < len(triangles):
            result = (perimeter, triangles)
    return result


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
