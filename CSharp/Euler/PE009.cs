//======================================================================
// Copyright (C) 2023, Gorka Suárez García
//======================================================================

/*
 * A Pythagorean triplet is a set of three natural numbers,
 * a < b < c, for which: a^2 + b^2 = c^2
 * 
 * For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
 * 
 * There exists exactly one Pythagorean triplet for which
 * a + b + c = 1000.
 * 
 * Find the product abc.
 */

using System;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 6.
    /// </summary>
    public class PE009 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int CANDIDATE = 1000;

            var (a, b, c) = FindTriplet(CANDIDATE);

            if (a == default || b == default || c == default) {
                Console.WriteLine("No result was found...");
            } else {
                Console.WriteLine($"The Pythagorean triplet is ({a}, {b}, {c}).");
                Console.WriteLine($"{a} + {b} + {c} = {a + b + c}");
                Console.WriteLine($"{a} x {b} x {c} = {a * b * c}");
            }
        }

        /// <summary>
        /// Finds a pythagorean triplet for which sum is equal to a
        /// given candidate number.
        /// </summary>
        /// <param name="candidate">The candidate number.</param>
        /// <returns>A triplet with the numbers or the default.</returns>
        (int, int, int) FindTriplet (int candidate) {
            var limit = (int) Math.Truncate(candidate / 3.0);
            var query = from c in Tools.Sequence(limit, candidate)
                        from b in Tools.Sequence(1, c)
                        let a = candidate - c - b
                        where (a + b + c) == candidate && IsPythagorean(a, b, c)
                        select (a, b, c);
            return query.FirstOrDefault();
        }

        /// <summary>
        /// Checks if a triplet of numbers is a pythagorean triplet.
        /// </summary>
        /// <param name="a">The first number of the triplet.</param>
        /// <param name="b">The second number of the triplet.</param>
        /// <param name="c">The third number of the triplet.</param>
        /// <returns>True if the triplet is pythagorean.</returns>
        bool IsPythagorean (int a, int b, int c) {
            return (0 < a) && (a < b) && (b < c) && (a * a + b * b == c * c);
        }
    }
}
