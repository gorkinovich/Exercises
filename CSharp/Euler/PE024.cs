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

namespace Euler {
    /// <summary>
    /// This class represents the problem 24.
    /// </summary>
    public class PE024 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int GOAL = 1_000_000;
            const string ITEMS = "0123456789";

            var result = string.Concat(Tools.Permutations(ITEMS.ToArray()).Take(GOAL).Last());

            Console.WriteLine($"The millionth lexicographic permutation is {result}.");
        }
    }
}
