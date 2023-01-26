//======================================================================
// Copyright (C) 2023, Gorka Suárez García
//======================================================================

/*
 * The prime factors of 13195 are 5, 7, 13 and 29.
 * 
 * What is the largest prime factor of the number 600851475143?
 */

using System;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 4.
    /// </summary>
    public class PE004 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int START = 100, LIMIT = 1000;

            var (result, left, right) = FindPalindrome(START, LIMIT);

            Console.Write("The largest palindrome made from the product of two ");
            Console.WriteLine($"3-digit numbers is {left} * {right} = {result}.");
        }

        /// <summary>
        /// Finds the largest palindrome number from the product of two numbers
        /// inside a range of numbers.
        /// </summary>
        /// <param name="start">The start number of the range.</param>
        /// <param name="limit">The limit number of the range.</param>
        /// <returns>A triplet with the palindrome and the two numbers of the
        /// multiplication if there is a palindrome; otherwise a triplet with
        /// zeroes.</returns>
        (int, int, int) FindPalindrome (int start, int limit) {
            (int number, int, int) result = (0, 0, 0);
            var candidates = Tools.Sequence(start, limit);
            foreach (var left in candidates) {
                foreach (var right in candidates) {
                    var number = left * right;
                    if (IsPalindrome(number) && number > result.number) {
                        result = (number, left, right);
                    }
                }
            }
            return result;
        }

        /// <summary>
        /// Checks if a value is a palindrome string.
        /// </summary>
        /// <typeparam name="T">The type of the input value.</typeparam>
        /// <param name="victim">The value to check.</param>
        /// <returns>True if the value is a palindrome.</returns>
        bool IsPalindrome<T> (T victim) {
            var normal = victim.ToString();
            var reversed = new string(normal.Reverse().ToArray());
            return normal == reversed;
        }
    }
}
