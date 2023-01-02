"""
| **Problem 22:** Names scores
| URL: https://projecteuler.net/problem=22
::

    Using names.txt (right click and 'Save Link/Target As...'),
    a 46K text file containing over five-thousand first names,
    begin by sorting it into alphabetical order. Then working out
    the alphabetical value for each name, multiply this value by
    its alphabetical position in the list to obtain a name score.

    For example, when the list is sorted into alphabetical order,
    COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th
    name in the list. So, COLIN would obtain a score of
    938 × 53 = 49714.

    What is the total of all the name scores in the file?

:author: Gorka Suárez García
:copyright: (c) 2022, Gorka Suárez García
"""
import urllib.request

######################################################################
# Constants
######################################################################

URL = "https://projecteuler.net/project/resources/p022_names.txt"


######################################################################
# Functions
######################################################################

def load_remote_text(url):
    """
    Load a remote text in the web.
    :param url: The URL of the text.
    :return: The string with the text.
    """
    request = urllib.request.urlopen(url)
    text = request.read()
    return str(text, "utf-8")


def get_score(position, name):
    """
    Gets the score for a name in a position.
    :param position: The position of the name.
    :param name: The name to check.
    :return: A tuple with the name and the score.
    """
    score = sum(1 + ord(c) - ord('A') for c in name)
    return name, position * score


def get_scores(text):
    """
    Gets the scores for a list of names.
    :param text: The text with the list of names.
    :return: A list with tuples with the name and the score.
    """
    names = [item.strip('"') for item in text.split(",")]
    names.sort()
    return [get_score(position, name)
            for position, name
            in zip(range(1, len(names) + 1), names)]


def main():
    """
    Main entry for the problem solver.
    """
    text = load_remote_text(URL)
    scores = get_scores(text)
    result = sum(score for _, score in scores)

    # Show the final result of the problem:
    print(f"The total of all the name scores in the file is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
