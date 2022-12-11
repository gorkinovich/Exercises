"""
AER 101: Diabolical and esoteric squares
URL: https://www.aceptaelreto.com/problem/statement.php?id=101
"""
import loader
import locale
import enum


######################################################################
# Enumerations
######################################################################

class SquareType(enum.Enum):
    """
    The types of squares in the problem.
    """

    NORMAL = enum.auto()
    DIABOLICAL = enum.auto()
    ESOTERIC = enum.auto()

    def __str__(self):
        """
        Gets the string representation of the object.
        :return: A string with the representation.
        """
        lang, _ = locale.getlocale()
        if lang.startswith('es_'):
            match self:
                case SquareType.NORMAL:
                    return "NO"
                case SquareType.DIABOLICAL:
                    return "DIABOLICO"
                case SquareType.ESOTERIC:
                    return "ESOTERICO"

        return self.name


######################################################################
# Classes
######################################################################

class Square:
    """
    This type represents a square matrix of numbers.
    """

    def __init__(self, size, values):
        """
        Initializes the object instance.
        :param size: The size of the side.
        :param values: The values of the square.
        """
        self.size = size
        self.values = values

    def __str__(self):
        """
        Gets the string representation of the object.
        :return: A string with the representation.
        """
        return f"Square(size={self.size}, values={self.values})"

    @property
    def half_size(self):
        """
        Gets the half size of the side.
        """
        return self.size // 2

    def get_value(self, x, y):
        """
        Gets a value inside the square.
        :param x: The column of the square.
        :param y: The row of the square.
        :return: The number inside the cell.
        """
        offset = x + self.size * y
        if offset < len(self.values):
            return self.values[offset]
        else:
            raise Exception("Square.get_value",
                            f"Invalid coordinates ({x}, {y})")

    def get_center(self):
        """
        Gets the center value inside the square.
        :return: The number inside the region.
        """
        if is_even(self.size):
            offset = self.half_size
            cells = [self.get_value(offset,   offset),
                     self.get_value(offset-1, offset),
                     self.get_value(offset,   offset-1),
                     self.get_value(offset-1, offset-1)]
            return sum(cells)
        else:
            offset = len(self.values) // 2
            return self.values[offset]

    def get_north(self):
        """
        Gets the north value inside the square.
        :return: The number inside the region.
        """
        offset = self.half_size
        if is_even(self.size):
            return self.values[offset] \
                   + self.values[offset-1]
        else:
            return self.values[offset]

    def get_south(self):
        """
        Gets the south value inside the square.
        :return: The number inside the region.
        """
        offset = len(self.values) - (self.half_size + 1)
        if is_even(self.size):
            return self.values[offset] \
                   + self.values[offset+1]
        else:
            return self.values[offset]

    def get_west(self):
        """
        Gets the west value inside the square.
        :return: The number inside the region.
        """
        if is_even(self.size):
            base_x = 0
            base_y = self.half_size
            return self.get_value(base_x, base_y) \
                   + self.get_value(base_x, base_y-1)
        else:
            offset = (len(self.values) // 2) - self.half_size
            return self.values[offset]

    def get_east(self):
        """
        Gets the east value inside the square.
        :return: The number inside the region.
        """
        if is_even(self.size):
            base_x = self.size - 1
            base_y = self.half_size
            return self.get_value(base_x, base_y) \
                   + self.get_value(base_x, base_y-1)
        else:
            offset = (len(self.values) // 2) + self.half_size
            return self.values[offset]

    def get_north_west(self):
        """
        Gets the north-west value inside the square.
        :return: The number inside the region.
        """
        return self.values[0]

    def get_north_east(self):
        """
        Gets the north-east value inside the square.
        :return: The number inside the region.
        """
        return self.values[self.size-1]

    def get_south_west(self):
        """
        Gets the south-west value inside the square.
        :return: The number inside the region.
        """
        return self.values[len(self.values)-self.size]

    def get_south_east(self):
        """
        Gets the south-east value inside the square.
        :return: The number inside the region.
        """
        return self.values[len(self.values)-1]

    def sum_column(self, x):
        """
        Gets the sum of a column inside the square.
        :param x: The column of the square.
        :return: The final sum number.
        """
        return sum([self.get_value(x, y)
                    for y in range(self.size)])

    def sum_row(self, y):
        """
        Gets the sum of a row inside the square.
        :param y: The row of the square.
        :return: The final sum number.
        """
        return sum([self.get_value(x, y)
                    for x in range(self.size)])

    def sum_diagonal_north_south(self):
        """
        Gets the sum of the north-to-south diagonal inside the square.
        :return: The final sum number.
        """
        return sum([self.get_value(i, i)
                    for i in range(self.size)])

    def sum_diagonal_south_north(self):
        """
        Gets the sum of the south-to-north diagonal inside the square.
        :return: The final sum number.
        """
        return sum([self.get_value(i, self.size-(i+1))
                    for i in range(self.size)])


######################################################################
# Functions
######################################################################

def is_even(number):
    """
    Checks if a number is even.
    :param number: The number to check.
    :return: True if the number is even, otherwise False.
    """
    return number % 2 == 0


def check_diabolical(square):
    """
    Check if the square is diabolical.
    :param square: The square to check.
    :return: True if the square is diabolical.
    """
    magic_number = square.sum_diagonal_north_south()
    sums = [magic_number, square.sum_diagonal_south_north()]
    for i in range(square.size):
        sums.append(square.sum_column(i))
        sums.append(square.sum_row(i))
    return all(value == magic_number for value in sums)


def check_first_rule(square):
    """
    Check the first rule of the esoteric squares.
    :param square: The square to check.
    :return: True if the rule applies.
    """
    left = sorted(square.values)
    right = range(1, square.size ** 2 + 1)
    return all(n == m for (n, m) in zip(left, right))


def check_second_rule(square, magic_number):
    """
    Check the second rule of the esoteric squares.
    :param square: The square to check.
    :param magic_number: The magic number to check.
    :return: True if the rule applies.
    """
    corners = square.get_north_west() \
              + square.get_north_east() \
              + square.get_south_west() \
              + square.get_south_east()
    return corners == magic_number


def check_third_rule(square, magic_number):
    """
    Check the third rule of the esoteric squares.
    :param square: The square to check.
    :param magic_number: The magic number to check.
    :return: True if the rule applies.
    """
    sides = square.get_north() \
            + square.get_south() \
            + square.get_west() \
            + square.get_east()
    if is_even(square.size):
        magic_number *= 2
    return sides == magic_number


def check_fourth_rule(square, magic_number):
    """
    Check the fourth rule of the esoteric squares.
    :param square: The square to check.
    :param magic_number: The magic number to check.
    :return: True if the rule applies.
    """
    center = square.get_center()
    if not is_even(square.size):
        center *= 4
    return center == magic_number


def get_esoteric_number(square):
    """
    Gets the magic number for esoteric squares.
    :param square: The square to check.
    :return: The magic number for the square.
    """
    magic_number = square.sum_diagonal_north_south()
    return 4 * magic_number // square.size


def check_square(square):
    """
    Checks the type of the content inside the square.
    :param square: The square to check.
    :return: The type of square: normal, diabolical or esoteric.
    """
    if check_diabolical(square):
        if check_first_rule(square):
            magic_number = get_esoteric_number(square)
            if check_second_rule(square, magic_number) \
                    and check_third_rule(square, magic_number) \
                    and check_fourth_rule(square, magic_number):
                return SquareType.ESOTERIC
        return SquareType.DIABOLICAL
    else:
        return SquareType.NORMAL


def execute(squares):
    """
    Executes the algorithm for the problem.
    :param squares: The input data.
    """
    for square in squares:
        try:
            print(check_square(square))
        except Exception as error:
            print(f"ERROR: Invalid input {square}")
            print(f"({type(error).__name__}) {error}")


def on_lines(lines):
    """
    The on lines loaded event for the problem.
    :param lines: The loaded lines.
    :return: The final lines for the problem.
    """
    data = []

    # Get the content inside the lines:
    try:
        for i in range(0, len(lines), 2):
            if size := int(lines[i]):
                values = [int(value) for value
                          in lines[i + 1].split()]
                data.append(Square(size, values))
    except Exception as error:
        print(f"ERROR: Invalid input {lines}")
        print(f"({type(error).__name__}) {error}")

    # Exit with the final input data:
    return data


def main():
    """
    Main entry for the diabolical squares' problem.
    """
    loader.main(execute, on_lines, loader.InputMode.ZERO_ENDING)


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
