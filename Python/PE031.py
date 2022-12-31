"""
| **Problem 31:** Coin sums
| URL: https://projecteuler.net/problem=31
::

    In England the currency is made up of pound, £, and pence, p,
    and there are eight coins in general circulation:

        1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

    It is possible to make £2 in the following way:

        1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

    How many different ways can £2 be made using any number of
    coins?

:author: Gorka Suárez García
:copyright: (c) 2022, Gorka Suárez García
"""
import enum
from shared import check_argv


######################################################################
# Enumerations
######################################################################

class Coin(enum.IntEnum):
    """
    The type of coins in the problem.
    """
    PENCE_ONE = 1
    PENCE_TWO = 2
    PENCE_FIVE = 5
    PENCE_TEN = 10
    PENCE_TWENTY = 20
    PENCE_FIFTY = 50
    POUND_ONE = 100
    POUND_TWO = 200

    def __str__(self):
        """
        Gets the string representation of the object.
        :return: A string with the representation.
        """
        match self:
            case Coin.PENCE_ONE:
                return "1p"
            case Coin.PENCE_TWO:
                return "2p"
            case Coin.PENCE_FIVE:
                return "5p"
            case Coin.PENCE_TEN:
                return "10p"
            case Coin.PENCE_TWENTY:
                return "20p"
            case Coin.PENCE_FIFTY:
                return "50p"
            case Coin.POUND_ONE:
                return "£1"
            case Coin.POUND_TWO:
                return "£2"
        return self.name


######################################################################
# Constants
######################################################################

GOAL = Coin.POUND_TWO

COIN_VALUES = [Coin.POUND_ONE, Coin.PENCE_FIFTY, Coin.PENCE_TWENTY,
               Coin.PENCE_TEN, Coin.PENCE_FIVE, Coin.PENCE_TWO,
               Coin.PENCE_ONE]

COINS_SIZE = len(COIN_VALUES)


######################################################################
# Classes
######################################################################

class Coins:
    """
    This type represents a vector of coins.
    """

    def __init__(self, limit):
        """
        Initializes the object instance.
        :param limit: The limit value to achieve.
        """
        self.data = [0 for _ in range(COINS_SIZE)]
        self.limit = limit

    def __str__(self):
        """
        Gets the string representation of the object.
        :return: A string with the representation.
        """
        def to_string(string, coins, values, index):
            separator = ", " if string else ""
            return f"{separator}{coins[index]}x{values[index]}"
        result = ""
        for coin in range(0, COINS_SIZE):
            if self.data[coin] > 0:
                result += to_string(result, self.data, COIN_VALUES, coin)
        return f"({result})" if result else "0"

    def __repr__(self):
        """
        Gets the string representation of the object.
        :return: A string with the representation.
        """
        return str(self)

    def __eq__(self, other):
        """
        Checks if two objects are equal.
        :param other: The other object.
        :return: True if the content is equal.
        """
        return isinstance(other, Coins) \
            and self.limit == other.limit \
            and str(self.data) == str(other.data)

    def __hash__(self):
        """
        Gets the hash code for the object.
        :return: The hash code for the object.
        """
        return hash(str(self.limit) + str(self.data))

    def clone(self):
        """
        Clones the current object instance.
        :return: The cloned object.
        """
        victim = Coins(self.limit)
        victim.data = self.data[:]
        return victim

    def clear(self, start=0):
        """
        Clears the vector of coins.
        :param start: The start position to modify.
        """
        for index in range(start, COINS_SIZE):
            self.data[index] = 0

    def fill(self, start=0):
        """
        Fills the vector of coins with a given limit value.
        :param start: The start position to modify.
        """
        goal = self.limit - self.get_value()
        if goal > 0:
            for index in range(start, COINS_SIZE):
                amount = goal // COIN_VALUES[index]
                if amount > 0:
                    self.data[index] += amount
                    goal -= amount * COIN_VALUES[index]
                    if goal == 0:
                        break
                    elif goal < 0:
                        raise ValueError
        elif goal < 0:
            raise ValueError

    def get_value(self, start=0):
        """
        Gets the final value of the vector of coins.
        :param start: The start position to count.
        :return: The sum of all the coins.
        """
        result = 0
        for index in range(start, COINS_SIZE):
            result += COIN_VALUES[index] * self.data[index]
        return result


######################################################################
# Functions
######################################################################

def get_combinations(victim, coins, combinations):
    """
    Gets all the combinations of coins for a limit value.
    :param victim: The current coin to check.
    :param coins: The vector of coins.
    :param combinations: The set of combinations to return.
    """
    if victim < COINS_SIZE:
        coins.clear(victim)
        coins.fill(victim)
        combinations.add(coins.clone())
        if victim + 1 < COINS_SIZE:
            if coins.data[victim] == 0:
                get_combinations(victim + 1, coins, combinations)
            else:
                while coins.data[victim] > 0:
                    coins.data[victim] -= 1
                    get_combinations(victim + 1, coins, combinations)


def main():
    """
    Main entry for the problem solver.
    """
    combinations = set()
    coins = Coins(GOAL)
    get_combinations(0, coins, combinations)
    result = len(combinations)

    if check_argv("show"):
        for item in combinations:
            print(f"{item}")
            if item.get_value() != GOAL:
                raise ValueError

    # Show the final result of the problem:
    print(f"There are {result} different ways £2 can be made using any number of coins.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()


######################################################################
# Test Functions
######################################################################

def test():
    combinations = set()
    coins = Coins(Coin.PENCE_TEN)
    get_combinations(0, coins, combinations)
    for item in combinations:
        print(f"{item}")
