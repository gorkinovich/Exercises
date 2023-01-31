//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

using System;
using System.Collections.Generic;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents a collection extension methods.
    /// </summary>
    public static class Extensions {
        //----------------------------------------------------------------------
        // IEnumerable
        //----------------------------------------------------------------------

        /// <summary>
        /// Computes the product of a sequence of numeric values.
        /// </summary>
        /// <param name="values">The sequence of numeric values.</param>
        /// <returns>The product of the values in the sequence</returns>
        public static int Product (this IEnumerable<int> values) {
            return values.Aggregate(1, (x, y) => x * y);
        }

        /// <summary>
        /// Computes the product of a sequence of numeric values.
        /// </summary>
        /// <param name="values">The sequence of numeric values.</param>
        /// <returns>The product of the values in the sequence</returns>
        public static long Product (this IEnumerable<long> values) {
            return values.Aggregate(1L, (x, y) => x * y);
        }

        /// <summary>
        /// Computes the product of a sequence of numeric values.
        /// </summary>
        /// <param name="values">The sequence of numeric values.</param>
        /// <returns>The product of the values in the sequence</returns>
        public static uint Product (this IEnumerable<uint> values) {
            return values.Aggregate(1U, (x, y) => x * y);
        }

        /// <summary>
        /// Computes the product of a sequence of numeric values.
        /// </summary>
        /// <param name="values">The sequence of numeric values.</param>
        /// <returns>The product of the values in the sequence</returns>
        public static ulong Product (this IEnumerable<ulong> values) {
            return values.Aggregate(1UL, (x, y) => x * y);
        }

        //----------------------------------------------------------------------
        // String
        //----------------------------------------------------------------------

        /// <summary>
        /// Splits a string using white spaces and returning the non-empty elements.
        /// </summary>
        /// <param name="value">The value to tranform.</param>
        /// <returns>A sequence with the non-empty elements.</returns>
        public static IEnumerable<string> SplitWithSpaces (this string value) {
            return value.Split(' ', '\n', '\r', '\t')
                        .Select(x => x.Trim())
                        .Where(x => !string.IsNullOrEmpty(x));
        }

        //----------------------------------------------------------------------
        // String
        //----------------------------------------------------------------------

        /// <summary>
        /// Converts a number digit character into an integer number.
        /// </summary>
        /// <param name="value">The character to convert.</param>
        /// <returns>The integer value of the digit.</returns>
        /// <exception cref="ArithmeticException">
        /// The character isn't a valid digit.
        /// </exception>
        public static int ParseDigit (this char value) {
            if ('0' <= value || value <= '9') {
                return value - '0';
            } else {
                throw new ArithmeticException($"The character {value} isn't a valid digit.");
            }
        }
    }
}
