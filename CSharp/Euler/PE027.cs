//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * Euler published the remarkable quadratic formula:
 * 
 *                     n^2 + n + 41
 * 
 * It turns out that the formula will produce 40 primes for
 * the consecutive values n = 0 to 39. However, when n = 40,
 * 40^2 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and
 * certainly when n = 41, 41^2 + 41 + 41 is clearly divisible
 * by 41.
 * 
 * Using computers, the incredible formula  n^2 - 79n + 1601
 * was discovered, which produces 80 primes for the consecutive
 * values n = 0 to 79. The product of the coefficients, -79 and
 * 1601, is -126479.
 * 
 * Considering quadratics of the form:
 * 
 *     n^2 + an + b, where |a| < 1000 and |b| < 1000
 * 
 *     where |n| is the modulus/absolute value of n
 *     e.g. |11| = 11 and |-4| = 4
 * 
 * Find the product of the coefficients, a and b, for the
 * quadratic expression that produces the maximum number
 * of primes for consecutive values of n, starting with
 * n = 0.
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 27.
    /// </summary>
    public class PE027 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int LIMIT = 1000;

            (int a, int b) = FindMaxChain(LIMIT, LIMIT);
            var primes = GetPrimesChain(a, b);

            Console.WriteLine($"The product of the coefficients, {a} and {b}, is {a * b}.");
            Console.WriteLine($"Primes chain ({primes.Count()}) = {Tools.ToStringList(primes)}");
        }

        /// <summary>
        /// Finds the maximum chain of primes for some coefficients.
        /// The range of the limits are[0, first) and[0, second].
        /// </summary>
        /// <param name="firstLimit">The first coefficient limit.</param>
        /// <param name="secondLimit">The second coefficient limit.</param>
        /// <returns>A tuple with the final coefficients.</returns>
        (int, int) FindMaxChain (int firstLimit, int secondLimit) {
            var coefficients = from a in Tools.Sequence(firstLimit)
                               from b in Tools.Sequence(secondLimit + 1)
                               select (a, b);
            return coefficients.Select(v => new { Value = v, Size = GetPrimesChain(v.a, v.b).Count() })
                               .MaxBy(x => x.Size)
                               .Value;
        }

        /// <summary>
        /// Gets the chain of primes for some coefficients.
        /// </summary>
        /// <param name="a">The first coefficient.</param>
        /// <param name="b">The second coefficient.</param>
        /// <returns>The list of primes obtained.</returns>
        IEnumerable<int> GetPrimesChain (int a, int b) {
            int n = 0, number = CalcFormula(n, a, b);
            while (Tools.IsPrime((ulong) number)) {
                yield return number;
                n++;
                number = CalcFormula(n, a, b);
            }
        }

        /// <summary>
        /// Gets the value for the problem formula.
        /// </summary>
        /// <param name="n">The input value.</param>
        /// <param name="a">The first coefficient.</param>
        /// <param name="b">The second coefficient.</param>
        /// <returns>The final value.</returns>
        int CalcFormula (int n, int a, int b) {
            return n * n + a * n + b;
        }
    }
}
