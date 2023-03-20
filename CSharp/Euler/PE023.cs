//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * A perfect number is a number for which the sum of its proper
 * divisors is exactly equal to the number. For example, the sum
 * of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28,
 * which means that 28 is a perfect number.
 * 
 * A number n is called deficient if the sum of its proper
 * divisors is less than n, and it is called abundant if this
 * sum exceeds n.
 * 
 * As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16,
 * the smallest number that can be written as the sum of two
 * abundant numbers is 24. By mathematical analysis, it can be
 * shown that all integers greater than 28123 can be written as
 * the sum of two abundant numbers. However, this upper limit
 * cannot be reduced any further by analysis even though it is
 * known that the greatest number that cannot be expressed as
 * the sum of two abundant numbers is less than this limit.
 * 
 * Find the sum of all the positive integers which cannot be
 * written as the sum of two abundant numbers.
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 23.
    /// </summary>
    public class PE023 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int LIMIT = 28123 + 1;

            var result = Tools.Sequence(1, LIMIT)
                              .Except(GenerateAbundantSums(LIMIT))
                              .Sum();

            Console.Write("The sum of all the naturals which cannot be written ");
            Console.WriteLine($"as the sum of two abundant numbers is {result}.");
        }

        /// <summary>
        /// Gets all the sums of two abundant numbers.
        /// </summary>
        /// <param name="limit">The upper limit in the range of numbers.</param>
        /// <returns>A set with all the sums of two abundant numbers.</returns>
        IEnumerable<int> GenerateAbundantSums (int limit) {
            var numbers = GetNumbersByType(limit, NumberType.Abundant).ToArray();
            for (int i = 0; i < numbers.Length; i++) {
                for (int j = 0; j < numbers.Length; j++) {
                    var value = numbers[i] + numbers[j];
                    if (value < limit) {
                        yield return value;
                    } else {
                        break;
                    }
                }
            }
        }

        /// <summary>
        /// Gets all the numbers of a given type.
        /// </summary>
        /// <param name="limit">The upper limit in the range of numbers.</param>
        /// <param name="type">The type of number to get.</param>
        /// <returns>A sorted list with the numbers filtered.</returns>
        IEnumerable<int> GetNumbersByType (int limit, NumberType type) {
            return Tools.Sequence(1, limit)
                        .Where(x => NumberTypeCheck((ulong)x) == type);
        }

        /// <summary>
        /// Checks the type of number given.
        /// </summary>
        /// <param name="number">The number to check.</param>
        /// <returns>The type of the number.</returns>
        NumberType NumberTypeCheck (ulong number) {
            var divisors = Sequences.Divisors(number);
            var value = divisors.Sum() - number;
            if (value == number) {
                return NumberType.Perfect;
            } else if (value < number) {
                return NumberType.Deficient;
            } else {
                return NumberType.Abundant;
            }
        }

        /// <summary>
        /// The type of number of the problem.
        /// </summary>
        enum NumberType {
            Perfect,
            Deficient,
            Abundant
        }
    }
}
