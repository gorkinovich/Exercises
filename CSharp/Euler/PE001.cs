/* Problem 1: Multiples of 3 or 5
 * URL: https://projecteuler.net/problem=1
 * 
 * If we list all the natural numbers below 10 that are multiples of
 * 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
 * 
 * Find the sum of all the multiples of 3 or 5 below 1000.
 */
using System;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 1.
    /// </summary>
    public class PE001 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int LIMIT = 1000;
            var result = Enumerable.Range(1, LIMIT - 1)
                                   .Where(x => (x % 3) == 0 || (x % 5) == 0)
                                   .Sum();
            Console.WriteLine($"The sum of all the multiples of 3 or 5 below 1000 is {result}.");
        }
    }
}
