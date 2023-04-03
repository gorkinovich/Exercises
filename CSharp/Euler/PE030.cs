//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * Surprisingly there are only three numbers that can be written
 * as the sum of fourth powers of their digits:
 * 
 *     1634 = 1^4 + 6^4 + 3^4 + 4^4
 *     8208 = 8^4 + 2^4 + 0^4 + 8^4
 *     9474 = 9^4 + 4^4 + 7^4 + 4^4
 * 
 * As 1 = 1^4 is not a sum it is not included.
 * 
 * The sum of these numbers is 1634 + 8208 + 9474 = 19316.
 * 
 * Find the sum of all the numbers that can be written as the
 * sum of fifth powers of their digits.
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 30.
    /// </summary>
    public class PE030 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int GOAL = 5;

            var result = FindNumbers(GOAL).Sum();

            Console.Write($"The sum of all the numbers that can be written as the ");
            Console.WriteLine($"sum of fifth powers of their digits is {result}.");
        }

        /// <summary>
        /// Finds the numbers required for the problem.
        /// </summary>
        /// <param name="goal">The size in digits of the numbers to check.</param>
        /// <returns>A set with the numbers that can be written as the sum of the
        /// N-th powers of their N-digits.</returns>
        IEnumerable<int> FindNumbers (int goal) {
            var start = Tools.IntPow(10, goal - 1);
            var limit = start * 10;
            return Tools.Sequence(start, limit)
                        .Where(victim => {
                            var number = victim.GetDigits()
                                               .Select(x => Tools.IntPow(x, goal))
                                               .Sum();
                            return number == victim;
                        })
                        .Distinct();
        }
    }
}
