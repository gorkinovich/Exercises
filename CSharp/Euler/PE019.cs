//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * You are given the following information, but you may prefer
 * to do some research for yourself.
 * 
 *     + 1 Jan 1900 was a Monday.
 *     + Thirty days has September,
 *       April, June and November.
 *       All the rest have thirty-one,
 *       Saving February alone,
 *       Which has twenty-eight, rain or shine.
 *       And on leap years, twenty-nine.
 *     + A leap year occurs on any year evenly divisible by 4,
 *       but not on a century unless it is divisible by 400.
 * 
 * How many Sundays fell on the first of the month during the
 * twentieth century (1 Jan 1901 to 31 Dec 2000)?
 */

using System;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 19.
    /// </summary>
    public class PE019 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run() {
            var MONTHS = 12;
            var START = new DateTime(1901, 1, 1);
            var LIMIT = new DateTime(2001, 1, 1);

            var result = Enumerable.Range(0, (LIMIT.Year - START.Year) * MONTHS)
                                   .Select(x => START.AddMonths(x).DayOfWeek == DayOfWeek.Sunday ? 1 : 0)
                                   .Sum();

            Console.WriteLine($"The number of Sundays that fell on the month's 1st during the 20th century is {result}.");
        }
    }
}
