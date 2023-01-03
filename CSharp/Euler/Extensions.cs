//======================================================================
// Copyright (C) 2023, Gorka Suárez García
//======================================================================

using System.Collections.Generic;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents a collection extension methods.
    /// </summary>
    public static class Extensions {
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
    }
}
