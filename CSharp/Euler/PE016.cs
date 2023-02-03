//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 * 
 * What is the sum of the digits of the number 2^1000?
 */

using System;
using System.Linq;
using System.Numerics;

namespace Euler {
    /// <summary>
    /// This class represents the problem 16.
    /// </summary>
    public class PE016 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run() {
            const int GOAL = 1000;

            var result = BigInteger.Pow(new BigInteger(2), GOAL)
                                   .ToString()
                                   .Select(x => x.ParseDigit())
                                   .Sum();

            Console.WriteLine($"The sum of the digits of the number 2^{GOAL} is {result}.");
        }
    }
}
