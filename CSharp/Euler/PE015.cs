//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * Starting in the top left corner of a 2×2 grid, there are 6
 * routes (without backtracking) to the bottom right corner.
 *
 *     1 2 3    1 2 .    1 2 .
 *     . . 4    . 3 4    . 3 .
 *     . . 5    . . 5    . 4 5
 *
 *     1 . .    1 . .    1 . .
 *     2 3 4    2 3 .    2 . .
 *     . . 5    . 4 5    3 4 5
 *
 * How many routes are there through a 20×20 grid?
 */

using System;
using System.Collections.Generic;

namespace Euler {
    /// <summary>
    /// This class represents the problem 15.
    /// </summary>
    public class PE015 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int SIZE = 20;

            var pascal = new PascalTriangle();
            var result = pascal.Get(SIZE * 2, SIZE);

            Console.WriteLine($"The number of routes in a {SIZE}x{SIZE} grid are {result}.");
        }
    }

    /// <summary>
    /// This type represents the <see href="https://en.wikipedia.org/wiki/Pascal's_triangle">pascal triangle</see>.
    /// </summary>
    public class PascalTriangle {
        /// <summary>
        /// The cache of number previously obtained.
        /// </summary>
        private Dictionary<(int, int), ulong> numbers = new Dictionary<(int, int), ulong>();

        public ulong Get(int n, int k) {
            if (n < 0 || k < 0) {
                throw new ArgumentException($"The arguments (n,k) can't be negative.");
            } else if (n == 0) {
                return 1;
            } else if (0 < k && k < n) {
                var key = (n, k);
                if (!numbers.ContainsKey(key)) {
                    var left = Get(n - 1, k - 1);
                    var right = Get(n - 1, k);
                    numbers[key] = left + right;
                }
                return numbers[key];
            } else {
                return 1;
            }
        }
    }
}
