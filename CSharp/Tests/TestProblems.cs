//======================================================================
// Copyright (C) 2023, Gorka Suárez García
//======================================================================

using Euler;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;
using System.Linq;

namespace Tests {
    [TestClass]
    public class TestProblems {
        //----------------------------------------------------------------------
        // Problems tests
        //----------------------------------------------------------------------

        [TestMethod]
        public void Vol001_Test () {
            execute<PE001>("The sum of all the multiples of 3 or 5 below 1000 is 233168.");
            execute<PE002>("The sum of the even-valued terms below 4000000 is 4613732.");
            execute<PE003>("The largest prime factor of 600851475143 is 6857.");
            execute<PE004>("The largest palindrome made from the product of two 3-digit numbers is 913 * 993 = 906609.");
            execute<PE005>("The smallest positive number that is a multiple of all numbers from 1 to 20 is 232792560.");
            execute<PE006>("The difference between the sum of the squares and the square of the sum of the first 100 numbers is 25164150.");
            execute<PE007>("The 10,001st prime number is 104743.");
            execute<PE008>("The greatest product of 13 consecutive digits is 23514624000.");
            execute<PE009>("The Pythagorean triplet is (200, 375, 425).", "200 + 375 + 425 = 1000", "200 x 375 x 425 = 31875000");
            execute<PE010>("The sum of all the primes below 2000000 is 142913828922.");
            execute<PE011>("The greatest product of four adjacent numbers is 70600674.");
            execute<PE012>("The first triangle number to have over 500 divisors is 76576500.");
        }

        //----------------------------------------------------------------------
        // Shared functions
        //----------------------------------------------------------------------

        private void execute<T> (params string[] messages) where T : IRunnable, new() {
            using StringWriter writer = new StringWriter();
            Console.SetOut(writer);

            var instance = new T();
            instance.Run();

            var message = string.Concat(messages.Select(x => $"{x}{Environment.NewLine}"));
            Assert.AreEqual(message, writer.ToString());
        }
    }
}
