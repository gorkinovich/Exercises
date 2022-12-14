"""
Problem 15: Lattice paths
URL: https://projecteuler.net/problem=15

    Starting in the top left corner of a 2×2 grid, there are 6
    routes (without backtracking) to the bottom right corner.

        1 2 3    1 2 .    1 2 .
        . . 4    . 3 4    . 3 .
        . . 5    . . 5    . 4 5

        1 . .    1 . .    1 . .
        2 3 4    2 3 .    2 . .
        . . 5    . 4 5    3 4 5

    How many routes are there through a 20×20 grid?
"""


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

def print_routes(step, limit, moves, accumulated):
    if step < limit:
        for move in moves:
            if move['count'] < move['limit']:
                move['count'] += 1
                next_accumulated = accumulated + [move['name']]
                print_routes(step + 1, limit, moves, next_accumulated)
                move['count'] -= 1
    elif accumulated:
        print(accumulated)


def test(size, show_routes=False, show_pascal=True):
    if show_routes:
        moves = [{'name': 'L', 'count': 0, 'limit': size},
                 {'name': 'R', 'count': 0, 'limit': size}]
        print_routes(0, size * 2, moves, [])

    if show_pascal:
        pascal = PascalTriangle()
        for n in range(size + 1):
            for k in range(n + 1):
                print(pascal.calc(n, k), end=" ")
            print()
