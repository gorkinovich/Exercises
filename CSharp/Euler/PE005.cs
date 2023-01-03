//======================================================================
// Copyright (C) 2023, Gorka Suárez García
//======================================================================

/*
 * 2520 is the smallest number that can be divided by each of the
 * numbers from 1 to 10 without any remainder.
 * 
 * What is the smallest positive number that is evenly divisible
 * (divisible with no remainder) by all of the numbers from 1 to 20?
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 5.
    /// </summary>
    public class PE005 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int FIRST = 1, LAST = 20;

            var result = FindNumber(FIRST, LAST);

            Console.Write("The smallest positive number that is a multiple ");
            Console.WriteLine($"of all numbers from 1 to 20 is {result}.");
        }

        /// <summary>
        /// Find the smallest natural divisible by a range of numbers.
        /// </summary>
        /// <param name="first">The first value of the divisors range.</param>
        /// <param name="last">The last value of the divisors range.</param>
        /// <returns>The smallest natural number.</returns>
        public static ulong FindNumber (int first, int last) {
            var divisors = Enumerable.Range(first, last)
                                     .Select(x => (ulong) x)
                                     .ToArray();
            foreach (var size in Enumerable.Range(1, divisors.Length)) {
                foreach (var combination in Tools.Combinations(divisors, size)) {
                    var number = combination.Product();
                    if (divisors.All(divisor => number % divisor == 0)) {
                        return number;
                    }
                }
            }
            return 0;
        }
    }
}
