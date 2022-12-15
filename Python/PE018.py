"""
Problem 18: Maximum path sum I
URL: https://projecteuler.net/problem=18

    By starting at the top of the triangle below and moving to
    adjacent numbers on the row below, the maximum total from top
    to bottom is 23.

           3              3
          7 4            7 .
         2 4 6          . 4 .
        8 5 9 3        . . 9 .

    That is, 3 + 7 + 4 + 9 = 23.

    Find the maximum total from top to bottom of the triangle
    below:

                      75
                     95 64
                    17 47 82
                   18 35 87 10
                  20 04 82 47 65
                 19 01 23 75 03 34
                88 02 77 73 07 63 67
               99 65 04 28 06 16 70 92
              41 41 26 56 83 40 80 70 33
             41 48 72 33 47 32 37 16 94 29
            53 71 44 65 25 43 91 52 97 51 14
           70 11 33 28 77 73 17 78 39 68 17 57
          91 71 52 38 17 14 91 43 58 50 27 29 48
         63 66 04 68 89 53 67 30 73 16 69 87 40 31
        04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

    NOTE: As there are only 16384 routes, it is possible to solve
    this problem by trying every route. However, Problem 67, is
    the same challenge with a triangle containing one-hundred
    rows; it cannot be solved by brute force, and requires a
    clever method! ;o)
"""


######################################################################
# Constants
######################################################################

WORLD = [[75],
         [95, 64],
         [17, 47, 82],
         [18, 35, 87, 10],
         [20,  4, 82, 47, 65],
         [19,  1, 23, 75,  3, 34],
         [88,  2, 77, 73,  7, 63, 67],
         [99, 65,  4, 28,  6, 16, 70, 92],
         [41, 41, 26, 56, 83, 40, 80, 70, 33],
         [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
         [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
         [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
         [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
         [63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
         [ 4, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23]]


######################################################################
# Functions
######################################################################

def find_path(world):
    """
    Finds the maximum valued path inside a triangular world.
    :param world: The data that represents the world.
    :return: A dictionary object where 'sum' is the value of the
    path, and 'path' is the current list of elements of that path.
    """
    wildcard = {'sum': -1}
    previous = []
    # Check all the rows inside the given world:
    for row in world:
        current = []
        # Check each column of the row:
        for index in range(len(row)):
            if previous:
                # When the row isn't the first one, we'll select the
                # left and right path objects from the previous results:
                if index - 1 >= 0:
                    left = previous[index - 1]
                else:
                    left = wildcard
                if index < len(previous):
                    right = previous[index]
                else:
                    right = wildcard
                # Then, we'll select the maximum value between the left
                # and the right, to create a new path object result and
                # add it to the current results:
                selected = left if left['sum'] >= right['sum'] else right
                cell = {'sum': row[index], 'path': [row[index]]}
                cell['path'] = selected['path'] + cell['path']
                cell['sum'] += selected['sum']
                current.append(cell)
            else:
                # On the first row of the world, just add to the
                # current objects the initial path object with the
                # root of the world:
                current.append({'sum': row[index], 'path': [row[index]]})
        # Update the previous results with the current ones:
        previous = current
    # Select the maximum valued path from the previous results:
    return max(previous, key=lambda x: x['sum'])


def main():
    """
    Main entry for the problem solver.
    """
    result = find_path(WORLD)

    # Show the final result of the problem:
    print(f"The maximum total from top to bottom of the triangle is {result['sum']}.")
    print(f"Final path: {result['path']}")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
