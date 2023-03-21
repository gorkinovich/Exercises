//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Euler {
    /// <summary>
    /// This class represents a collection of big number sequences.
    /// </summary>
    public static class BigSequences {
        //----------------------------------------------------------------------
        // IEnumerable
        //----------------------------------------------------------------------

        /// <summary>
        /// Makes a enumerable that returns the divisors of a number.
        /// </summary>
        /// <param name="victim">The number to check.</param>
        /// <returns>A enumerable to obtain the sequence's numbers.</returns>
        public static IEnumerable<BigInteger> Divisors (BigInteger victim) {
            yield return 1;
            if (victim > 1) {
                for (BigInteger divisor = 2; divisor < victim; divisor++) {
                    if (victim % divisor == 0) {
                        yield return divisor;
                    }
                }
                yield return victim;
            }
        }

        /// <summary>
        /// Makes a enumerable that returns the factors of a number.
        /// </summary>
        /// <param name="victim">The number to check.</param>
        /// <returns>A enumerable to obtain the sequence's numbers.</returns>
        public static IEnumerable<BigInteger> Factors (BigInteger victim, IEnumerable<BigInteger> primes = null) {
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
        public static IEnumerable<BigInteger> Fibonacci (bool includeZero = false) {
            if (includeZero) {
                yield return 0;
            }
            BigInteger previous = 0, current = 1, next;
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
        public static IEnumerable<BigInteger> Primes (bool includeOne = false) {
            // Send the first primes and set the state of the loop:
            if (includeOne) {
                yield return 1;
            }
            yield return 2;
            yield return 3;
            var primes = new List<BigInteger>() { 2, 3 };
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
        public static IEnumerable<BigInteger> Triangular (bool includeZero = false) {
            if (includeZero) {
                yield return 0;
            }
            BigInteger result = 0, number = 1;
            while (true) {
                result += number;
                yield return result;
                number++;
            }
        }

        //----------------------------------------------------------------------
        // Shared Members
        //----------------------------------------------------------------------

        /// <summary>
        /// Gets the next prime number.
        /// </summary>
        /// <param name="primes">The list of prime numbers.</param>
        /// <returns>The next prime number.</returns>
        private static BigInteger NextPrime (IList<BigInteger> primes) {
            BigInteger offset = 2;
            var victim = primes[^1] + offset;
            while (true) {
                // Check if the current candidate is a prime number:
                bool isPrime = true;
                BigInteger limit = 1 + (BigInteger) Math.Truncate(Math.Sqrt((double) victim));
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
