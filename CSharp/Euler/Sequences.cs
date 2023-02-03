//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

using System;
using System.Collections.Generic;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents a collection of number sequences.
    /// </summary>
    public static class Sequences {
        //----------------------------------------------------------------------
        // IEnumerable
        //----------------------------------------------------------------------

        /// <summary>
        /// Makes a enumerable that returns the factors of a number.
        /// </summary>
        /// <param name="victim">The number to check.</param>
        /// <returns>A enumerable to obtain the sequence's numbers.</returns>
        public static IEnumerable<ulong> Factors(ulong victim, IEnumerable<ulong> primes = null) {
            if (primes == null) {
                primes = Primes(true);
            }
            foreach (var prime in primes) {
                if (victim < prime) {
                    yield break;
                } else if (prime == 1) {
                    yield return 1;
                } else {
                    while ((victim % prime) == 0) {
                        yield return prime;
                        victim /= prime;
                    }
                }
            }
        }

        /// <summary>
        /// Makes a enumerable that returns Fibonacci's numbers.
        /// </summary>
        /// <param name="includeZero">The include number zero flag.</param>
        /// <returns>A enumerable to obtain the sequence's numbers.</returns>
        /// <remarks>This is an infinite loop sequence.</remarks>
        public static IEnumerable<ulong> Fibonacci(bool includeZero = false) {
            if (includeZero) {
                yield return 0;
            }
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
        /// <param name="includeZero">The include number one flag.</param>
        /// <returns>A enumerable to obtain the prime numbers.</returns>
        /// <remarks>This is an infinite loop sequence.</remarks>
        public static IEnumerable<ulong> Primes(bool includeOne = false) {
            // Send the first primes and set the state of the loop:
            if (includeOne) {
                yield return 1;
            }
            yield return 2;
            yield return 3;
            var primes = new List<ulong>() { 2, 3 };
            while (true) {
                var victim = NextPrime(primes);
                primes.Add(victim);
                yield return victim;
            }
        }

        /// <summary>
        /// Makes a enumerable that returns triangular numbers.
        /// </summary>
        /// <param name="includeZero">The include number zero flag.</param>
        /// <returns>A enumerable to obtain the triangular numbers.</returns>
        /// <remarks>This is an infinite loop sequence.</remarks>
        public static IEnumerable<ulong> Triangular(bool includeZero = false) {
            if (includeZero) {
                yield return 0;
            }
            ulong result = 0, number = 1;
            while (true) {
                result += number;
                yield return result;
                number++;
            }
        }

        //----------------------------------------------------------------------
        // LazySequence
        //----------------------------------------------------------------------

        /// <summary>
        /// Makes a lazy sequence that returns Fibonacci's numbers.
        /// </summary>
        /// <param name="includeZero">The include number zero flag.</param>
        /// <returns>A lazy sequence to obtain the sequence's numbers.</returns>
        public static LazySequence<ulong> LazyFibonacci(bool includeZero = false) {
            return new LazySequence<ulong>(
                (memory) => memory[^1] + memory[^2],
                includeZero ? () => new List<ulong>() { 0, 1 } :
                              () => new List<ulong>() { 1, 1 }
            );
        }


        /// <summary>
        /// Makes a lazy sequence that returns prime numbers.
        /// </summary>
        /// <param name="includeZero">The include number one flag.</param>
        /// <returns>A lazy sequence to obtain the prime numbers.</returns>
        public static LazySequence<ulong> LazyPrimes(bool includeOne = false) {
            return new LazySequence<ulong>(
                NextPrime,
                includeOne ? () => new List<ulong>() { 1, 2, 3 } :
                             () => new List<ulong>() { 2, 3 }
            );
        }

        /// <summary>
        /// Makes a lazy sequence that returns triangular numbers.
        /// </summary>
        /// <param name="includeZero">The include number zero flag.</param>
        /// <returns>A lazy sequence to obtain the triangular numbers.</returns>
        public static LazySequence<ulong> LazyTriangular(bool includeZero = false) {
            return new LazySequence<ulong>(
                includeZero ? (memory) => Tools.Triangular((ulong)memory.Count) :
                              (memory) => Tools.Triangular((ulong)memory.Count + 1),
                includeZero ? () => new List<ulong>() { 0 } :
                              () => new List<ulong>() { 1 }
            );
        }

        //----------------------------------------------------------------------
        // Shared Members
        //----------------------------------------------------------------------

        /// <summary>
        /// Gets the next prime number.
        /// </summary>
        /// <param name="primes">The list of prime numbers.</param>
        /// <returns>The next prime number.</returns>
        private static ulong NextPrime(IList<ulong> primes) {
            const ulong offset = 2;
            var victim = primes[^1] + offset;
            while (true) {
                // Check if the current candidate is a prime number:
                bool isPrime = true;
                ulong limit = 1 + (ulong)Math.Truncate(Math.Sqrt(victim));
                foreach (var prime in primes.SkipWhile(x => x < 2)) {
                    if (prime > limit) {
                        break;
                    } else if ((victim % prime) == 0) {
                        isPrime = false;
                        break;
                    }
                }
                // If the candidate is a prime number send it back:
                if (isPrime) {
                    return victim;
                }
                // Select the next candidate to check:
                victim += offset;
            }
        }
    }
}
