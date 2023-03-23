//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * A unit fraction contains 1 in the numerator. The decimal
 * representation of the unit fractions with denominators
 * 2 to 10 are given:
 * 
 *     1/2  = 0.5
 *     1/3  = 0.(3)
 *     1/4  = 0.25
 *     1/5  = 0.2
 *     1/6  = 0.1(6)
 *     1/7  = 0.(142857)
 *     1/8  = 0.125
 *     1/9  = 0.(1)
 *     1/10 = 0.1
 * 
 * Where 0.1(6) means 0.166666..., and has a 1-digit recurring
 * cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
 * 
 * Find the value of d < 1000 for which 1/d contains the
 * longest recurring cycle in its decimal fraction part.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Euler {
    /// <summary>
    /// This class represents the problem 26.
    /// </summary>
    public class PE026 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const uint LIMIT = 1000;

            var result = FindNumber(LIMIT);

            Console.WriteLine($"The value 1/{result} contains the longest recurring cycle in its decimal fraction part.");
        }

        /// <summary>
        /// Finds the number with the longest recurring cycle in its decimal
        /// fraction part in the value (1 / number).
        /// </summary>
        /// <param name="limit">The limit number in the range to check.</param>
        /// <returns>The number with the longest recurring cycle.</returns>
        uint FindNumber (uint limit) {
            int size = 0;
            uint result = 0;
            for (uint number = 2; number < limit; number++) {
                var parts = GetDivisionParts(1, number);
                if (size < parts.Item3.Length) {
                    result = number;
                    size = parts.Item3.Length;
                }
            }
            return result;
        }

        /// <summary>
        /// Gets the parts of a division.
        /// </summary>
        /// <param name="left">The left operand in the division.</param>
        /// <param name="right">The right operand in the division.</param>
        /// <returns>A triplet with the integer part, the decimal part,
        /// and the ending cycle part.</returns>
        (string, string, string) GetDivisionParts (uint left, uint right) {
            uint integerPart, remainder, initialKey, cycleKey = 0;
            var table = new Dictionary<uint, (uint, uint)>();
            var generator = DivisionGenerator(left, right).GetEnumerator();
            Func<(uint, uint)> generatorNext = () => {
                generator.MoveNext();
                return generator.Current;
            };
            (integerPart, remainder) = generatorNext();
            initialKey = remainder;
            // Calculate the decimal digits of the division:
            while (remainder != 0) {
                // We'll check if the current remainder exists as a key in the table,
                // if so we'll set the cycle key and exit the current loop:
                if (table.ContainsKey(remainder)) {
                    cycleKey = remainder;
                    break;
                } else {
                    // We need to save the previous remainder as the current key, and
                    // then calculate the current digit and remainder of the division,
                    // to set in the table the obtained values:
                    uint digit, key = remainder;
                    (digit, remainder) = generatorNext();
                    table[key] = (digit, remainder);
                }
            }
            // Return the result as a tuple of strings:
            (string decimals, string ending) parts = ExtractDecimalParts(table, initialKey, cycleKey);
            return (integerPart.ToString(), parts.decimals, parts.ending);
        }

        /// <summary>
        /// Extracts the decimal ending cycle part from the initial part.
        /// </summary>
        /// <param name="table">The table with the decimal digits.</param>
        /// <param name="initialKey">The initial remainder key.</param>
        /// <param name="cycleKey">The initial remainder cycle key.</param>
        /// <returns>A tuple with two string, where the first component is
        /// the initial decimal part, and the second component is the cycle
        /// ending part of the decimals.</returns>
        (string, string) ExtractDecimalParts (IDictionary<uint, (uint, uint)> table,
            uint initialKey, uint cycleKey) {
            Action<StringBuilder, uint> append = (x, v) => x.Append((char) ('0' + v));
            var decimalPart = new StringBuilder();
            var endingPart = new StringBuilder();
            uint digit, key = initialKey;
            // Here we'll extract the initial decimals outside the cycle:
            while ((key != 0) && (key != cycleKey)) {
                (digit, key) = table[key];
                append(decimalPart, digit);
            }
            // If the decimals don't stop, we'll extract the cycle:
            if (key != 0) {
                bool loop = true;
                while (loop) {
                    (digit, key) = table[key];
                    append(endingPart, digit);
                    if (key == cycleKey) {
                        loop = false;
                    }
                }
            }
            // Return the result as a tuple of strings:
            return (decimalPart.ToString(), endingPart.ToString());
        }

        /// <summary>
        /// Gets the values of a given division, step by step.
        /// </summary>
        /// <param name="left">The left value.</param>
        /// <param name="right">The right value.</param>
        /// <returns>A tuple with the integer division result, the remainder,
        /// the current left operand and the current right operand.</returns>
        IEnumerable<(uint, uint)> DivisionGenerator (uint left, uint right) {
            uint division, remainder;
            while (true) {
                if (left < right) {
                    division = 0;
                    remainder = left;
                } else {
                    division = left / right;
                    remainder = left % right;
                }
                yield return (division, remainder);
                if (remainder == 0) {
                    yield break;
                }
                left = remainder * 10;
            }
        }
    }
}
