"""
Problem 17: Number letter counts
URL: https://projecteuler.net/problem=17

    If the numbers 1 to 5 are written out in words: one, two,
    three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19
    letters used in total.

    If all the numbers from 1 to 1000 (one thousand) inclusive
    were written out in words, how many letters would be used?

    NOTE: Do not count spaces or hyphens. For example, 342 (three
    hundred and forty-two) contains 23 letters and 115 (one
    hundred and fifteen) contains 20 letters. The use of "and"
    when writing out numbers is in compliance with British usage.
"""
import enum

######################################################################
# Constants
######################################################################

BEGIN = 1
FINAL = 1000

SPACE = ' '

NUMBER_TEN = 10
NUMBER_TWENTY = 20
NUMBER_HUNDRED = 100
NUMBER_THOUSAND = 1000
NUMBER_MILLION = NUMBER_THOUSAND ** 2
NUMBER_BILLION = NUMBER_THOUSAND ** 3
NUMBER_LIMIT = NUMBER_THOUSAND ** 4


######################################################################
# Classes
######################################################################

class Transform:
    """
    This type represents a converter of values.
    """
    class Word(enum.StrEnum):
        AND = "and"
        HUNDRED = "hundred"
        THOUSAND = "thousand"
        MILLION = "million"
        BILLION = "billion"

    words_units = ["zero", "one", "two", "three", "four",
                   "five", "six", "seven", "eight", "nine",
                   "ten", "eleven", "twelve", "thirteen",
                   "fourteen", "fifteen", "sixteen",
                   "seventeen", "eighteen", "nineteen"]

    words_tens = ["", "", "twenty", "thirty", "forty", "fifty",
                  "sixty", "seventy", "eighty", "ninety"]

    @classmethod
    def __num_to_word(cls, number, divisor, left_to_word, middle_words=()):
        """
        Converts a number from digits to words.
        :param number: The number to convert.
        :param divisor: The divisor limit.
        :param left_to_word: The left side converter.
        :return: The number in words.
        """
        left, right = divmod(number, divisor)
        left_word = left_to_word(left)
        right_word = cls.num_to_word(right) if right else ""
        if right_word:
            return SPACE.join([left_word, *middle_words, right_word])
        elif middle_words:
            return SPACE.join([left_word, middle_words[0]])
        else:
            return left_word

    @classmethod
    def num_to_word(cls, number):
        """
        Converts a number from digits to words.
        :param number: The number to convert.
        :return: The number in words.
        """
        if number >= NUMBER_LIMIT or number < 0:
            raise ValueError
        elif number >= NUMBER_BILLION:
            return cls.__num_to_word(number, NUMBER_BILLION,
                                     lambda left: cls.num_to_word(left),
                                     [cls.Word.BILLION])
        elif number >= NUMBER_MILLION:
            return cls.__num_to_word(number, NUMBER_MILLION,
                                     lambda left: cls.num_to_word(left),
                                     [cls.Word.MILLION])
        elif number >= NUMBER_THOUSAND:
            return cls.__num_to_word(number, NUMBER_THOUSAND,
                                     lambda left: cls.num_to_word(left),
                                     [cls.Word.THOUSAND])
        elif number >= NUMBER_HUNDRED:
            return cls.__num_to_word(number, NUMBER_HUNDRED,
                                     lambda left: cls.words_units[left],
                                     [cls.Word.HUNDRED, cls.Word.AND])
        elif number >= NUMBER_TWENTY:
            return cls.__num_to_word(number, NUMBER_TEN,
                                     lambda left: cls.words_tens[left])
        else:
            return cls.words_units[number]


######################################################################
# Functions
######################################################################

def main():
    """
    Main entry for the problem solver.
    """
    numbers = [Transform.num_to_word(number)
               for number in range(BEGIN, FINAL + 1)]
    result = sum(1 for c in ''.join(numbers) if c.isalpha())

    # Show the final result of the problem:
    print(f"Number of letters of the written numbers from {BEGIN} to {FINAL} is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
