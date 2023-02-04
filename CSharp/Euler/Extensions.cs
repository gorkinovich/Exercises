//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

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
        /// <typeparam name="T">The type of the elements of values.</typeparam>
        /// <param name="values">The sequence of numeric values.</param>
        /// <returns>The product of the values in the sequence</returns>
        public static T Product<T> (this IEnumerable<T> values)
            where T : INumber<T> {
            return values.Aggregate((x, y) => x * y);
        }

        /// <summary>
        /// Gets the maximum value in a generic sequence according to a specified key selector function.
        /// </summary>
        /// <typeparam name="TSource">The type of the elements of values.</typeparam>
        /// <typeparam name="TKey">The type of key to compare elements by.</typeparam>
        /// <param name="values">A sequence of values to determine the maximum value of.</param>
        /// <param name="selector">A function to extract the key for each element.</param>
        /// <returns>The value with the maximum key in the sequence.</returns>
        public static TSource MaxByField<TSource, TKey> (this IEnumerable<TSource> values,
            Func<TSource, TKey> selector) where TKey : IComparable {
            return values.Aggregate((r, x) => (selector(r).CompareTo(selector(x)) >= 0) ? r : x);
        }

        /// <summary>
        /// Gets the minimum value in a generic sequence according to a specified key selector function.
        /// </summary>
        /// <typeparam name="TSource">The type of the elements of values.</typeparam>
        /// <typeparam name="TKey">The type of key to compare elements by.</typeparam>
        /// <param name="values">A sequence of values to determine the minimum value of.</param>
        /// <param name="selector">A function to extract the key for each element.</param>
        /// <returns>The value with the minimum key in the sequence.</returns>
        public static TSource MinByField<TSource, TKey> (this IEnumerable<TSource> values,
            Func<TSource, TKey> selector) where TKey : IComparable {
            return values.Aggregate((r, x) => (selector(r).CompareTo(selector(x)) <= 0) ? r : x);
        }

        //----------------------------------------------------------------------
        // INumber
        //----------------------------------------------------------------------

        /// <summary>
        /// Gets a sequence with the digits of a number.
        /// </summary>
        /// <typeparam name="T">The type of the input value.</typeparam>
        /// <param name="value">The number to transform.</param>
        /// <returns>The sequence with the digits.</returns>
        public static IEnumerable<int> GetDigits<T> (this INumber<T> value) where T : INumber<T> {
            return value.ToString().Select(x => x.ParseDigit());
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
        // Char
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
