//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

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

            var result = Sequences.Primes().Skip(CANDIDATE - 1).First();

            Console.WriteLine($"The 10,001st prime number is {result}.");
        }
    }
}
