//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * If the numbers 1 to 5 are written out in words: one, two,
 * three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19
 * letters used in total.
 * 
 * If all the numbers from 1 to 1000 (one thousand) inclusive
 * were written out in words, how many letters would be used?
 * 
 * NOTE: Do not count spaces or hyphens. For example, 342 (three
 * hundred and forty-two) contains 23 letters and 115 (one
 * hundred and fifteen) contains 20 letters. The use of "and"
 * when writing out numbers is in compliance with British usage.
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 17.
    /// </summary>
    public class PE017 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run() {
            const int BEGIN = 1;
            const int FINAL = 1000;

            var numbers = from number in Tools.Range<long>(BEGIN, FINAL)
                          select WordNumber.FromInt64(number);
            var result = string.Concat(numbers)
                               .Select(x => char.IsLetter(x) ? 1 : 0)
                               .Sum();

            Console.WriteLine($"Number of letters of the written numbers from {BEGIN} to {FINAL} is {result}.");
        }
    }

    /// <summary>
    /// This class represents a converter of numbers into words.
    /// </summary>
    public class WordNumber {
        //----------------------------------------------------------------------
        // Constants
        //----------------------------------------------------------------------

        const char SPACE = ' ';

        const long NUMBER_TEN = 10;
        const long NUMBER_TWENTY = 20;
        const long NUMBER_HUNDRED = 100;
        const long NUMBER_THOUSAND = 1_000;
        const long NUMBER_MILLION = 1_000_000;
        const long NUMBER_BILLION = 1_000_000_000;
        const long NUMBER_LIMIT = 1_000_000_000_000;

        const string WORD_AND = "and";
        const string WORD_HUNDRED = "hundred";
        const string WORD_THOUSAND = "thousand";
        const string WORD_MILLION = "million";
        const string WORD_BILLION = "billion";

        static readonly string[] WORDS_UNITS = new[] {
            "zero", "one", "two", "three", "four",
            "five", "six", "seven", "eight", "nine",
            "ten", "eleven", "twelve", "thirteen",
            "fourteen", "fifteen", "sixteen",
            "seventeen", "eighteen", "nineteen"
        };

        static readonly string[] WORDS_TENS = new[] {
            "", "", "twenty", "thirty", "forty", "fifty",
            "sixty", "seventy", "eighty", "ninety"
        };

        //----------------------------------------------------------------------
        // Methods
        //----------------------------------------------------------------------

        /// <summary>
        /// Converts a number from digits to words.
        /// </summary>
        /// <param name="number">The number to convert.</param>
        /// <returns>The number in words.</returns>
        public static string FromInt64(long number) {
            return number switch {
                < 0 or >= NUMBER_LIMIT =>
                    throw new ArgumentException("The input number is outside the valid range."),
                >= NUMBER_BILLION =>
                    transform(number, NUMBER_BILLION, x => FromInt64(x), WORD_BILLION),
                >= NUMBER_MILLION =>
                    transform(number, NUMBER_MILLION, x => FromInt64(x), WORD_MILLION),
                >= NUMBER_THOUSAND =>
                    transform(number, NUMBER_THOUSAND, x => FromInt64(x), WORD_THOUSAND),
                >= NUMBER_HUNDRED =>
                    transform(number, NUMBER_HUNDRED, x => WORDS_UNITS[x], WORD_HUNDRED, WORD_AND),
                >= NUMBER_TWENTY =>
                    transform(number, NUMBER_TEN, x => WORDS_TENS[x]),
                _ =>
                    WORDS_UNITS[number]
            };
        }

        /// <summary>
        /// Transform a number from digits to words.
        /// </summary>
        /// <param name="number">The number to transform.</param>
        /// <param name="divisor">The divisor limit.</param>
        /// <param name="leftToWord">The left side transformation.</param>
        /// <param name="middleWords">The middle words in the transform.</param>
        /// <returns>The number in words.</returns>
        private static string transform(long number, long divisor,
            Func<long, string> leftToWord, params string[] middleWords) {
            var left = Math.DivRem(number, divisor, out long right);
            var leftWord = leftToWord(left);
            var rightWord = right != 0 ? FromInt64(right) : "";
            var terms = new List<string>() { leftWord };
            if (!string.IsNullOrEmpty(rightWord)) {
                terms.AddRange(middleWords);
                terms.Add(rightWord);
            } else if (middleWords.Length > 0) {
                terms.Add(middleWords[0]);
            }
            return string.Join(SPACE, terms);
        }
    }
}
