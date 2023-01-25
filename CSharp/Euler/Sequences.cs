//======================================================================
// Copyright (C) 2023, Gorka Suárez García
//======================================================================

using System;
using System.Collections.Generic;

namespace Euler {
    /// <summary>
    /// This class represents a collection of number sequences.
    /// </summary>
    public static class Sequences {
        /// <summary>
        /// Makes a enumerable that returns Fibonacci's numbers.
        /// </summary>
        /// <returns>A enumerable to obtain the sequence's numbers.</returns>
        public static IEnumerable<ulong> Fibonacci () {
            ulong previous = 0, current = 1, next;
            while (true) {
                yield return current;
                next = previous + current;
                previous = current;
                current = next;
            }
        }

        /// <summary>
        /// Makes a enumerable that returns prime numbers.
        /// </summary>
        /// <returns>A enumerable to obtain the prime numbers.</returns>
        public static IEnumerable<ulong> Primes () {
            // Send the first primes and set the state of the loop:
            yield return 2;
            var primes = new List<ulong>() { 2 };
            ulong victim = 3;
            while (true) {
                // Check if the current candidate is a prime number:
                bool isPrime = true;
                ulong limit = 1 + (ulong) Math.Truncate(Math.Sqrt(victim));
                foreach (var prime in primes) {
                    if (prime > limit) {
                        break;
                    } else if ((victim % prime) == 0) {
                        isPrime = false;
                        break;
                    }
                }
                // If the candidate is a prime number send it back:
                if (isPrime) {
                    primes.Add(victim);
                    yield return victim;
                }
                // Select the next candidate to check:
                victim += 2;
            }
        }
    }
}
