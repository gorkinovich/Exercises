//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Euler {
    /// <summary>
    /// This class represents a collection of utility operations.
    /// </summary>
    public static class Tools {
        //----------------------------------------------------------------------
        // Math Formulas
        //----------------------------------------------------------------------

        /// <summary>
        /// Gets the factorial of a natural number.
        /// </summary>
        /// <param name="value">The input number.</param>
        /// <returns>The factorial of the input number.</returns>
        /// <exception cref="ArithmeticException"></exception>
        public static BigInteger Factorial (int value) {
            if (value < 0) {
                throw new ArithmeticException("Factorial doesn't allow negative numbers as input values.");
            } else if (value == 0) {
                return 1;
            } else {
                return Enumerable.Range(1, value)
                                 .Select(x => new BigInteger(x))
                                 .Product();
            }
        }

        /// <summary>
        /// Returns a specified number raised to the specified power.
        /// </summary>
        /// <typeparam name="T">The type of the operands.</typeparam>
        /// <param name="left">The number to be raised to a power.</param>
        /// <param name="right">The number that specifies the power</param>
        /// <returns>The number left raised to the power right.</returns>
        public static T Pow<T> (T left, T right) {
            double x = (double) Convert.ChangeType(left, typeof(double));
            double y = (double) Convert.ChangeType(right, typeof(double));
            return (T) Convert.ChangeType(Math.Pow(x, y), typeof(T));
        }

        /// <summary>
        /// Returns a specified number raised to the specified power.
        /// </summary>
        /// <param name="left">The number to be raised to a power.</param>
        /// <param name="right">The number that specifies the power</param>
        /// <returns>The number left raised to the power right.</returns>
        public static T IntPow<T> (T left, T right) where T : INumber<T> {
            T result = (T) Convert.ChangeType(1, typeof(T));
            while (0.CompareTo(right--) < 0) {
                result *= left;
            }
            return result;
        }

        /// <summary>
        /// Gets a triangular number.
        /// </summary>
        /// <param name="index">The index of the number.</param>
        /// <returns>The triangular number.</returns>
        public static ulong Triangular (ulong index) {
            return index * (index + 1) / 2;
        }

        //----------------------------------------------------------------------
        // Sequences Tools
        //----------------------------------------------------------------------

        /// <summary>
        /// Gets the combinations of an array of elements.
        /// </summary>
        /// <typeparam name="T">The type of the elements.</typeparam>
        /// <param name="elements">The array with the elements.</param>
        /// <param name="size">The size of elements to combine.</param>
        /// <returns>A enumerable of arrays with the combinations.</returns>
        public static IEnumerable<T[]> Combinations<T> (T[] elements, int size) {
            var result = new LinkedList<T>();
            var stack = new Stack<(int index, int step)>();
            stack.Push((0, 0));
            while (stack.Count > 0) {
                var current = stack.Pop();
                if (current.step >= size) {
                    yield return result.ToArray();
                    result.RemoveLast();
                } else if (current.index >= elements.Length) {
                    if (result.Count > 0) {
                        result.RemoveLast();
                    }
                } else {
                    result.AddLast(elements[current.index]);
                    current.index++;
                    stack.Push(current);
                    stack.Push((current.index, current.step + 1));
                }
            }
        }

        /// <summary>
        /// Gets a sequence of numbers using the C# range function.
        /// </summary>
        /// <typeparam name="T">The return type of the sequence.</typeparam>
        /// <param name="start">The start number of the sequence.</param>
        /// <param name="count">The count number of elements.</param>
        /// <returns>A enumerable with the sequence of numbers.</returns>
        public static IEnumerable<BigInteger> BigRange (int start, int count) {
            foreach (int number in Enumerable.Range(start, count)) {
                yield return new BigInteger(number);
            }
        }

        /// <summary>
        /// Gets a sequence of numbers using the C# range function.
        /// </summary>
        /// <typeparam name="T">The return type of the sequence.</typeparam>
        /// <param name="start">The start number of the sequence.</param>
        /// <param name="count">The count number of elements.</param>
        /// <returns>A enumerable with the sequence of numbers.</returns>
        public static IEnumerable<T> Range<T> (int start, int count) {
            foreach (int number in Enumerable.Range(start, count)) {
                yield return (T) Convert.ChangeType(number, typeof(T));
            }
        }

        /// <summary>
        /// Gets a sequence of numbers like the range function in Python.
        /// </summary>
        /// <param name="start">The start number of the sequence.</param>
        /// <param name="limit">The limit number of the secuence.</param>
        /// <returns>A enumerable with the sequence of numbers.</returns>
        public static IEnumerable<int> Sequence (int start, int limit) {
            return Enumerable.Range(start, limit - start);
        }

        /// <summary>
        /// Gets a sequence of numbers like the range function in Python.
        /// </summary>
        /// <param name="limit">The limit number of the secuence.</param>
        /// <returns>A enumerable with the sequence of numbers.</returns>
        public static IEnumerable<int> Sequence (int limit) {
            return Enumerable.Range(0, limit);
        }

        //----------------------------------------------------------------------
        // String Tools
        //----------------------------------------------------------------------

        /// <summary>
        /// Gets a string from a collection of values.
        /// </summary>
        /// <typeparam name="T">The type of the values.</typeparam>
        /// <param name="values">The collection with the values.</param>
        /// <param name="separator">The separator string.</param>
        /// <param name="initial">The initial string.</param>
        /// <param name="ending">The ending string.</param>
        /// <returns></returns>
        public static string ToString<T> (IEnumerable<T> values, string separator = "",
            string initial = "", string ending = "") {
            return initial + string.Join(separator, values) + ending;
        }
    }
}
