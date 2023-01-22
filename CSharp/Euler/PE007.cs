//======================================================================
// Copyright (C) 2023, Gorka Suárez García
//======================================================================

/*
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and
 * 13, we can see that the 6th prime is 13.
 * 
 * What is the 10,001st prime number?
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 6.
    /// </summary>
    public class PE007 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int CANDIDATE = 10_001;

            var result = Primes().Skip(CANDIDATE - 1).First();

            Console.WriteLine($"The 10,001st prime number is {result}.");
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
