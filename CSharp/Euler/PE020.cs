//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * n! means n × (n - 1) × ... × 3 × 2 × 1
 * 
 * For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
 * and the sum of the digits in the number 10! is
 * 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
 * 
 * Find the sum of the digits in the number 100!
 */

using System;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 20.
    /// </summary>
    public class PE020 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int GOAL = 100;

            var result = Tools.Factorial(GOAL)
                              .GetDigits()
                              .Sum();

            Console.WriteLine($"The sum of the digits in the number {GOAL}! is {result}.");
        }
    }
}
