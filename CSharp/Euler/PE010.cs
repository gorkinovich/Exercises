//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 * 
 * Find the sum of all the primes below two million.
 */

using System;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 6.
    /// </summary>
    public class PE010 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const ulong LIMIT = 2_000_000UL;

            var result = Sequences.Primes()
                                  .TakeWhile(x => x < LIMIT)
                                  .Aggregate((accum, next) => accum + next);

            Console.WriteLine($"The sum of all the primes below {LIMIT} is {result}.");
        }
    }
}
