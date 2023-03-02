//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * Let d(n) be defined as the sum of proper divisors of n
 * (numbers less than n which divide evenly into n).
 * 
 * If d(a) = b and d(b) = a, where a <> b, then a and b are an
 * amicable pair and each of a and b are called amicable numbers.
 * 
 * For example, the proper divisors of 220 are 1, 2, 4, 5, 10,
 * 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper
 * divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
 * 
 * Evaluate the sum of all the amicable numbers under 10000.
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 20.
    /// </summary>
    public class PE021 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int LIMIT = 10_000;

            var numbers = GetAmicableNumbers(LIMIT).Order();
            var result = numbers.Sum();

            Console.WriteLine($"The sum of all the amicable numbers under {LIMIT} is {result}.");
            Console.WriteLine($"Amicable numbers: {Tools.ToString(numbers, ", ", "[", "]")}");
        }

        /// <summary>
        /// Gets all the amicable numbers under a limit number.
        /// </summary>
        /// <param name="limit">The limit number.</param>
        /// <returns>A set with all the amicable numbers.</returns>
        IEnumerable<ulong> GetAmicableNumbers (int limit) {
            var numbers = new SortedSet<ulong>(Tools.Sequence<ulong>(1, limit));
            while (numbers.Count > 0) {
                var victim = numbers.First();
                var (a, b) = CheckAmicableCondition(victim);
                if (a == victim || b == victim) {
                    numbers.Remove(a);
                    numbers.Remove(b);
                    yield return a;
                    yield return b;
                } else {
                    numbers.Remove(victim);
                }
            }
        }

        /// <summary>
        /// Checks if a number has an amicable number.
        /// </summary>
        /// <param name="number">The number to check.</param>
        /// <returns>A tuple with the amicable numbers or (0, 0).</returns>
        (ulong, ulong) CheckAmicableCondition (ulong number) {
            var mirror = SumProperDivisors(number);
            if ((mirror != number) && (SumProperDivisors(mirror) == number)) {
                if (number < mirror) {
                    return (number, mirror);
                } else {
                    return (mirror, number);
                }
            } else {
                return (0, 0);
            }
        }

        /// <summary>
        /// Sums all the proper divisors of a number.
        /// </summary>
        /// <param name="number">The number to check.</param>
        /// <returns>The sum of the proper divisors.</returns>
        ulong SumProperDivisors (ulong number) {
            if (number > 1) {
                return Sequences.Divisors(number).Sum() - number;
            } else {
                return 0;
            }
        }
    }
}
