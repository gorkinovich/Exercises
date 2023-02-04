//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

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
        }

        [TestMethod]
        public void Vol002_Test () {
            execute<PE011>("The greatest product of four adjacent numbers is 70600674.");
            execute<PE012>("The first triangle number to have over 500 divisors is 76576500.");
            execute<PE013>("The first ten digits of the sum of the following one-hundred 50-digit numbers is 5537376230.");
            execute<PE014>("The starting number, under 1000000, with the longest chain is 837799.");
            execute<PE015>("The number of routes in a 20x20 grid are 137846528820.");
            execute<PE016>("The sum of the digits of the number 2^1000 is 1366.");
            execute<PE017>("Number of letters of the written numbers from 1 to 1000 is 21124.");
            execute<PE018>("The maximum total from top to bottom of the triangle is 1074.", "Final path: [75, 64, 82, 87, 82, 75, 73, 28, 83, 32, 91, 78, 58, 73, 93]");
            execute<PE019>("The number of Sundays that fell on the month's 1st during the 20th century is 171.");
            execute<PE020>("The sum of the digits in the number 100! is 648.");
        }

        [TestMethod]
        public void Vol003_Test () {
            // execute<PE021>("The sum of all the amicable numbers under 10000 is 31626.", "Amicable numbers: [220, 284, 1184, 1210, 2620, 2924, 5020, 5564, 6232, 6368]");
            // execute<PE022>("The total of all the name scores in the file is 871198282.");
            // execute<PE023>("The sum of all the naturals which cannot be written as the sum of two abundant numbers is 4179871.");
            // execute<PE024>("The millionth lexicographic permutation is 2783915460.");
            // execute<PE025>("The first term in the Fibonacci sequence to contain 1000 digits 1070066266382758936764980584457396885083683896632151665013235203375314520604694040621889147582489792657804694888177591957484336466672569959512996030461262748092482186144069433051234774442750273781753087579391666192149259186759553966422837148943113074699503439547001985432609723067290192870526447243726117715821825548491120525013201478612965931381792235559657452039506137551467837543229119602129934048260706175397706847068202895486902666185435124521900369480641357447470911707619766945691070098024393439617474103736912503231365532164773697023167755051595173518460579954919410967778373229665796581646513903488154256310184224190259846088000110186255550245493937113651657039447629584714548523425950428582425306083544435428212611008992863795048006894330309773217834864543113205765659868456288616808718693835297350643986297640660000723562917905207051164077614812491885830945940566688339109350944456576357666151619317753792891661581327159616877487983821820492520348473874384736771934512787029218636250627816.");
            // execute<PE026>("The value 1/983 contains the longest recurring cycle in its decimal fraction part.");
            // execute<PE027>("The product of the coefficients, 1 and 41, is 41.", "Primes chain (40) = [41, 43, 47, 53, 61, 71, 83, 97, 113, 131, 151, 173, 197, 223, 251, 281, 313, 347, 383, 421, 461, 503, 547, 593, 641, 691, 743, 797, 853, 911, 971, 1033, 1097, 1163, 1231, 1301, 1373, 1447, 1523, 1601]");
            // execute<PE028>("The sum of the numbers on the diagonals in a 1001^2 spiral is 669171001.");
            // execute<PE029>("The distinct terms in the sequence generated are 9183.");
            // execute<PE030>("The sum of all the numbers that can be written as the sum of fifth powers of their digits are {93084, 54748, 92727}.");
        }

        [TestMethod]
        public void Vol004_Test () {
            // execute<PE031>("There are 73651 different ways £2 can be made using any number of coins.");
            // execute<PE032>("The sum of all pandigital products is 56370.");
            // execute<PE033>("The value of the denominator of the product is 800.");
            // execute<PE034>("The sum of all numbers which are equal to the sum of the factorial of their digits is 40730.");
            // execute<PE035>("The circular primes below 1000000 are: [2, 3, 5, 7, (13, 31), (17, 71), (37, 73), (79, 97), (113, 131, 311), (197, 971, 719), (199, 991, 919), (733, 337, 373), (9311, 3119, 1193, 1931), (9377, 3779, 7793, 7937), (99371, 93719, 37199, 71993, 19937), (39119, 91193, 11939, 19391, 93911), (393919, 939193, 391939, 919393, 193939, 939391), (933199, 331999, 319993, 199933, 999331, 993319)]");
            // execute<PE036>("The sum of all numbers, less than 1000000, which are palindromic in base 10 and base 2 is 872187.");
            // execute<PE037>("The sum of the primes that are both truncatable is 748317.");
            // execute<PE038>("The largest pandigital 9-digit number is 9327.");
            // execute<PE039>("The perimeter with the maximum number of solutions is 840.");
            // execute<PE040>("The product of the digits in the irrational decimal fraction is 210.");
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
