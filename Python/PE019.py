"""
Problem 19: Counting Sundays
URL: https://projecteuler.net/problem=19

    You are given the following information, but you may prefer
    to do some research for yourself.

        + 1 Jan 1900 was a Monday.
        + Thirty days has September,
          April, June and November.
          All the rest have thirty-one,
          Saving February alone,
          Which has twenty-eight, rain or shine.
          And on leap years, twenty-nine.
        + A leap year occurs on any year evenly divisible by 4,
          but not on a century unless it is divisible by 400.

    How many Sundays fell on the first of the month during the
    twentieth century (1 Jan 1901 to 31 Dec 2000)?
"""
import datetime
from shared import check_argv

######################################################################
# Constants
######################################################################

SUNDAY = 6
DATE_BEGIN = "1901-01-01"
DATE_END = "2000-12-31"


######################################################################
# Functions
######################################################################

def add_month(value):
    """
    Adds one month to a date.
    :param value: The date to change.
    :return: The new date.
    """
    if value.month < 12:
        year = value.year
        month = value.month + 1
        day = value.day
    else:
        year = value.year + 1
        month = 1
        day = value.day
    return datetime.date(year, month, day)


def get_weekdays(begin, end, weekday):
    """
    Gets a list of dates of those 1st of the months that
    fell on a given weekday inside a range of dates.
    :param begin: The begin date.
    :param end: The end date.
    :param weekday: The week day to select.
    :return: The list of days that match with the weekday.
    """
    dates = []
    current = datetime.date.fromisoformat(begin)
    limit = datetime.date.fromisoformat(end)
    while current <= limit:
        if current.weekday() == weekday:
            dates.append(current.strftime("%A, %d-%B-%Y"))
        current = add_month(current)
    return dates


def main():
    """
    Main entry for the problem solver.
    """
    dates = get_weekdays(DATE_BEGIN, DATE_END, SUNDAY)
    result = len(dates)

    # Show the final dates of the problem:
    if check_argv("show_dates"):
        print("List of obtained dates:")
        for date in dates:
            print(date)
        print()

    # Show the final result of the problem:
    print(f"The number of Sundays that fell on the month's 1st during the 20th century is {result}.")


######################################################################
# Module execution
######################################################################

if __name__ == "__main__":
    main()
