//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * The following iterative sequence is defined for the set of
 * positive integers:
 *
 *   n -> n/2 (n is even)
 *   n -> 3n + 1 (n is odd)
 *
 * Using the rule above and starting with 13, we generate the
 * following sequence:
 *
 *   13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
 *
 * It can be seen that this sequence (starting at 13 and
 * finishing at 1) contains 10 terms. Although it has not
 * been proved yet (Collatz Problem), it is thought that
 * all starting numbers finish at 1.
 *
 * Which starting number, under one million, produces the
 * longest chain?
 *
 * NOTE: Once the chain starts the terms are allowed to go
 * above one million.
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 14.
    /// </summary>
    public class PE014 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int LIMIT = 1_000_000;

            var result = Tools.Sequence(1, LIMIT)
                              .Select(x => new { Number = x, Length = GetLength(x) })
                              .MaxBy(x => x.Length)
                              .Number;

            Console.WriteLine($"The starting number, under {LIMIT}, with the longest chain is {result}.");
        }

        /// <summary>
        /// The cache of results of previous sequences.
        /// </summary>
        Dictionary<long, int> cache = new Dictionary<long, int>();

        /// <summary>
        /// Checks a number to obtain the length of its sequence.
        /// </summary>
        /// <param name="number">The number to check.</param>
        /// <returns>The length of the sequence.</returns>
        int GetLength (long number) {
            if (!cache.ContainsKey(number)) {
                int length = 0;
                // Get all the numbers not included in the cache:
                var victims = new Stack<long>();
                foreach (var victim in Sequence(number)) {
                    if (cache.ContainsKey(victim)) {
                        length = cache[victim];
                        break;
                    } else {
                        victims.Push(victim);
                    }
                }
                // Add all the obtained numbers in the cache:
                while (victims.Count > 0) {
                    length++;
                    cache[victims.Pop()] = length;
                }
            }
            return cache[number];
        }

        /// <summary>
        /// Makes a enumerable that returns the problem sequence.
        /// </summary>
        /// <param name="number">The initial number of the sequence.</param>
        /// <returns>A enumerable to obtain the numbers of the sequence.</returns>
        IEnumerable<long> Sequence (long number) {
            while (number > 0) {
                yield return number;
                if (number <= 1) {
                    yield break;
                } else if ((number % 2) == 0) {
                    number /= 2;
                } else {
                    number = 3 * number + 1;
                }
            }
        }
    }
}
