//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * Starting with the number 1 and moving to the right in a
 * clockwise direction a 5 by 5 spiral is formed as follows:
 * 
 *     21 22 23 24 25    21  .  .  . 25
 *     20  7  8  9 10     .  7  .  9  .
 *     19  6  1  2 11     .  .  1  .  .
 *     18  5  4  3 12     .  5  .  3  .
 *     17 16 15 14 13    17  .  .  . 13
 * 
 * It can be verified that the sum of the numbers on the
 * diagonals is 101.
 * 
 * What is the sum of the numbers on the diagonals in a
 * 1001 by 1001 spiral formed in the same way?
 */

using System;

namespace Euler {
    /// <summary>
    /// This class represents the problem 28.
    /// </summary>
    public class PE028 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int SIZE = 1001;

            var result = SumDiagonalNumbers(SIZE);

            Console.WriteLine($"The sum of the numbers on the diagonals in a {SIZE}^2 spiral is {result}.");
        }

        /// <summary>
        /// Sums the diagonal numbers in a spiral inside a square.
        /// </summary>
        /// <param name="size">The side size of the square.</param>
        /// <returns>The final sum of the diagonal numbers.</returns>
        int SumDiagonalNumbers (int size) {
            if ((size % 2) == 0) {
                throw new ArgumentException("The size must be an odd number.");
            }
            // Set the initial data of the algorithm:
            const int OFFSET_INITIAL = 2;
            const int OFFSET_STEP = 2;
            const int STEPS_PER_CYCLE = 4;
            int result = 1;
            int current = 1;
            int offset = OFFSET_INITIAL;
            // Visit all the diagonal positions from the center to the
            // outside of the square, calculating the numbers of each
            // corner in each cycle, and add them to the result:
            for (int cycle = (size - 1) / 2; cycle > 0; cycle--) {
                for (int step = STEPS_PER_CYCLE; step > 0; step--) {
                    current += offset;
                    result += current;
                }
                offset += OFFSET_STEP;
            }
            // Return the final obtained result:
            return result;
        }
    }
}
