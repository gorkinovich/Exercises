﻿//======================================================================
// Copyright (C) 2023, Gorka Suárez García
//======================================================================

/*
 * Each new term in the Fibonacci sequence is generated by adding
 * the previous two terms. By starting with 1 and 2, the first 10
 * terms will be: 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
 * 
 * By considering the terms in the Fibonacci sequence whose
 * values do not exceed four million, find the sum of the
 * even-valued terms.
 */

using System;
using System.Collections.Generic;

namespace Euler {
    /// <summary>
    /// This class represents the problem 2.
    /// </summary>
    public class PE002 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const int LIMIT = 4_000_000;
            ulong result = 0;

            foreach (var current in Fibonacci()) {
                if (current >= LIMIT) {
                    break;
                } else if (current % 2 == 0) {
                    result += current;
                }
            }

            Console.WriteLine($"The sum of the even-valued terms below {LIMIT} is {result}.");
        }

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
    }
}
