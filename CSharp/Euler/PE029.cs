//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * A permutation is an ordered arrangement of objects. For
 * example, 3124 is one possible permutation of the digits
 * 1, 2, 3 and 4. If all of the permutations are listed
 * numerically or alphabetically, we call it lexicographic
 * order. The lexicographic permutations of 0, 1 and 2 are:
 * 
 *     012   021   102   120   201   210
 * 
 * What is the millionth lexicographic permutation of the
 * digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
 */

using System;
using System.Linq;
using System.Numerics;

namespace Euler {
    /// <summary>
    /// This class represents the problem 29.
    /// </summary>
    public class PE029 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int LIMIT = 100;

            var numbers = from a in Tools.Sequence(2, LIMIT + 1)
                          from b in Tools.Sequence(2, LIMIT + 1)
                          select BigInteger.Pow(a, b);
            var result = numbers.Distinct().Count();

            Console.WriteLine($"The distinct terms in the sequence generated are {result}.");
        }
    }
}
