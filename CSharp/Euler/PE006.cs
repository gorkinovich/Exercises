//======================================================================
// Copyright (C) 2023, Gorka Suárez García
//======================================================================

/*
 * The sum of the squares of the first ten natural numbers is,
 * 1^2 + 2^2 + ... + 10^2 = 385
 * 
 * The square of the sum of the first ten natural numbers is,
 * (1 + 2 + ... + 10)^2 = 552 = 3025
 * 
 * Hence, the difference between the sum of the squares of
 * the first ten natural numbers and the square of the sum
 * is 3025 - 385 = 2640.
 * 
 * Find the difference between the sum of the squares of the
 * first one hundred natural numbers and the square of the sum.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Euler {
    /// <summary>
    /// This class represents the problem 6.
    /// </summary>
    public class PE006 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int FIRST = 1, LAST = 100;

            var result = CalcNumber(FIRST, LAST);

            Console.Write("The difference between the sum of the squares and the square ");
            Console.WriteLine($"of the sum of the first {LAST} numbers is {result}.");
        }

        /// <summary>
        /// Calcs the difference between the sum of the squares
        /// and the square of the sum of a range of numbers.
        /// </summary>
        /// <param name="first">The first value of the range.</param>
        /// <param name="last">The last value of the range.</param>
        /// <returns>The difference between the sums.</returns>
        public long CalcNumber (int first, int last) {
            Func<long, long> square = x => x * x;
            var numbers = Tools.Range<long>(first, last);
            var sum_square = numbers.Select(square).Sum();
            var square_sum = square(numbers.Sum());
            return square_sum - sum_square;
        }
    }
}
