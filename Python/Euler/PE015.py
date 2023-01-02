"""
| **Problem 15:** Lattice paths
| URL: https://projecteuler.net/problem=15
::

    Starting in the top left corner of a 2×2 grid, there are 6
    routes (without backtracking) to the bottom right corner.

        1 2 3    1 2 .    1 2 .
        . . 4    . 3 4    . 3 .
        . . 5    . . 5    . 4 5

        1 . .    1 . .    1 . .
        2 3 4    2 3 .    2 . .
        . . 5    . 4 5    3 4 5

    How many routes are there through a 20×20 grid?

:author: Gorka Suárez García
:copyright: (c) 2022, Gorka Suárez García
"""
import enum

######################################################################
# Constants
######################################################################

SIZE = 20


######################################################################
# Classes
######################################################################

class PascalTriangle():
    """
    This type represents the `pascal triangle
    <https://en.wikipedia.org/wiki/Pascal's_triangle>`_.
    """

    def __init__(self):
        """
        Initializes the object instance.
        """
        self.numbers = {}

    def calc(self, n, k):
        """
        Gets the (n, k) number inside the pascal triangle.
        :param n: The row in the triangle.
        :param k: The column in the triangle.
        :return: The natural number for (n, k).
        """
        if n < 0 or k < 0:
            raise ValueError
        elif n == 0:
            return 1
        elif 0 < k < n:
            if (n,k) not in self.numbers:
                left = self.calc(n - 1, k - 1)
                right = self.calc(n - 1, k)
                self.numbers[(n,k)] = left + right
            return self.numbers[(n,k)]
        else:
            return 1

    @staticmethod
    def calc_slow(n, k):
        """
        Gets the (n, k) number inside the pascal triangle.
        :param n: The row in the triangle.
        :param k: The column in the triangle.
        :return: The natural number for (n, k).
        """
        if n < 0 or k < 0:
            raise ValueError
        elif n == 0:
            return 1
        elif 0 < k < n:
            left = PascalTriangle.calc_slow(n - 1, k - 1)
            right = PascalTriangle.calc_slow(n - 1, k)
            return left + right
        else:
            return 1


######################################################################
# Functions
######################################################################

def main():
    """
    Main entry for the problem solver.
    """
    pascal = PascalTriangle()
    result = pascal.calc(SIZE * 2, SIZE)

    # Show the final result of the problem:
    print(f"The number of routes in a {SIZE}x{SIZE} grid are {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()


######################################################################
# Test Functions
######################################################################

class TestFlag(enum.Flag):
    LENGTHS = enum.auto()
    SHOW_ROUTES = enum.auto()
    SHOW_PASCAL = enum.auto()


def check_routes(step, limit, moves, accumulated, on_route):
    if step < limit:
        for move in moves:
            if move['count'] < move['limit']:
                move['count'] += 1
                next_accumulated = accumulated + [move['name']]
                check_routes(step + 1, limit, moves, next_accumulated, on_route)
                move['count'] -= 1
    elif accumulated:
        on_route(accumulated)


def test_routes(size):
    def on_route(accumulated):
        print(accumulated)

    moves = [{'name': 'L', 'count': 0, 'limit': size},
             {'name': 'R', 'count': 0, 'limit': size}]
    check_routes(0, size * 2, moves, [], on_route)


def test_pascal(size):
    pascal = PascalTriangle()
    for n in range(size * 2 + 1):
        for k in range(n + 1):
            print(pascal.calc(n, k), end=" ")
        print()


def test_lengths(size):
    def on_route(accumulated):
        nonlocal count
        count += 1

    pascal = PascalTriangle()
    for value in range(1, size + 1):
        count = 0
        moves = [{'name': 'L', 'count': 0, 'limit': value},
                 {'name': 'R', 'count': 0, 'limit': value}]
        check_routes(0, value * 2, moves, [], on_route)
        count2 = pascal.calc(value * 2, value)
        print(f"Case: {value} -> check: {count}, pascal: {count2}.")
        if count != count2:
            raise Exception(f"test_lengths failed!", f"size = {value}",
                            f"check = {count}", f"pascal = {count2}")
    print("Ok")


def test(size, flag=TestFlag.LENGTHS):
    if flag & TestFlag.SHOW_ROUTES:
        test_routes(size)

    if flag & TestFlag.SHOW_PASCAL:
        test_pascal(size)

    if flag & TestFlag.LENGTHS:
        test_lengths(size)
