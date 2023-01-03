//======================================================================
// Copyright (C) 2023, Gorka Suárez García
//======================================================================

/*
 * The prime factors of 13195 are 5, 7, 13 and 29.
 * 
 * What is the largest prime factor of the number 600851475143?
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 3.
    /// </summary>
    public class PE003 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const ulong CANDIDATE = 600_851_475_143;
            var result = GetFactors(CANDIDATE).Distinct().LastOrDefault();
            Console.WriteLine($"The largest prime factor of {CANDIDATE} is {result}.");
        }

        /// <summary>
        /// Makes a enumerable that returns the factors of a number.
        /// </summary>
        /// <param name="number">A enumerable to obtain the factors.</param>
        /// <returns></returns>
        public static IEnumerable<ulong> GetFactors(ulong number) {
            ulong divisor = 2;
            while (number >= divisor) {
                if (number % divisor == 0) {
                    number /= divisor;
                    yield return divisor;
                } else {
                    divisor++;
                }
            }
        }
    }
}
