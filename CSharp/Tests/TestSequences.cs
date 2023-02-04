//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

using Euler;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.Linq;

namespace Tests {
    [TestClass]
    public class TestSequences {
        //----------------------------------------------------------------------
        // Fibonacci numbers
        //----------------------------------------------------------------------

        private readonly ulong[] fibonacciValues = new ulong[] {
            0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181
        };

        [TestMethod]
        public void FibonacciTest () {
            checkSequences(Sequences.Fibonacci(true), fibonacciValues,
                "Fibonacci starting with number zero failed!");
            checkSequences(Sequences.Fibonacci(), fibonacciValues.Skip(1),
                "Fibonacci starting with number one failed!");
        }

        [TestMethod]
        public void LazyFibonacciTest () {
            checkSequences(Sequences.LazyFibonacci(true), fibonacciValues,
                "Lazy fibonacci starting with number zero failed!");
            checkSequences(Sequences.LazyFibonacci(), fibonacciValues.Skip(1),
                "Lazy fibonacci starting with number one failed!");
        }

        //----------------------------------------------------------------------
        // Prime numbers
        //----------------------------------------------------------------------

        private readonly ulong[] primesValues = new ulong[] {
            1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41,
            43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
        };

        [TestMethod]
        public void PrimesTest () {
            checkSequences(Sequences.Primes(true), primesValues,
                "Primes starting with number one failed!");
            checkSequences(Sequences.Primes(), primesValues.Skip(1),
                "Primes starting with number two failed!");
        }

        [TestMethod]
        public void LazyPrimesTest () {
            checkSequences(Sequences.LazyPrimes(true), primesValues,
                "Primes starting with number one failed!");
            checkSequences(Sequences.LazyPrimes(), primesValues.Skip(1),
                "Primes starting with number two failed!");
        }

        //----------------------------------------------------------------------
        // Triangular numbers
        //----------------------------------------------------------------------

        private readonly ulong[] triangularValues = new ulong[] {
            0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, 153, 171, 190,
            210, 231, 253, 276, 300, 325, 351, 378, 406, 435, 465, 496, 528, 561, 595, 630, 666
        };

        [TestMethod]
        public void TriangularTest () {
            checkSequences(Sequences.Triangular(true), triangularValues,
                "Triangular starting with number zero failed!");
            checkSequences(Sequences.Triangular(), triangularValues.Skip(1),
                "Triangular starting with number zero failed!");
        }

        [TestMethod]
        public void LazyTriangularTest () {
            checkSequences(Sequences.LazyTriangular(true), triangularValues,
                "Triangular starting with number zero failed!");
            checkSequences(Sequences.LazyTriangular(), triangularValues.Skip(1),
                "Triangular starting with number zero failed!");
        }

        //----------------------------------------------------------------------
        // Shared functions
        //----------------------------------------------------------------------

        private void checkSequences (IEnumerable<ulong> victim, IEnumerable<ulong> data, string message) {
            var numbers = victim.Take(data.Count());
            var tuples = numbers.Zip(data).All(x => x.First == x.Second);
            Assert.IsTrue(tuples, message);
        }
    }
}
