//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * By starting at the top of the triangle below and moving to
 * adjacent numbers on the row below, the maximum total from top
 * to bottom is 23.
 * 
 *        3              3
 *       7 4            7 .
 *      2 4 6          . 4 .
 *     8 5 9 3        . . 9 .
 * 
 * That is, 3 + 7 + 4 + 9 = 23.
 * 
 * Find the maximum total from top to bottom of the triangle
 * below:
 * 
 *                   75
 *                  95 64
 *                 17 47 82
 *                18 35 87 10
 *               20 04 82 47 65
 *              19 01 23 75 03 34
 *             88 02 77 73 07 63 67
 *            99 65 04 28 06 16 70 92
 *           41 41 26 56 83 40 80 70 33
 *          41 48 72 33 47 32 37 16 94 29
 *         53 71 44 65 25 43 91 52 97 51 14
 *        70 11 33 28 77 73 17 78 39 68 17 57
 *       91 71 52 38 17 14 91 43 58 50 27 29 48
 *      63 66 04 68 89 53 67 30 73 16 69 87 40 31
 *     04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
 * 
 * NOTE: As there are only 16384 routes, it is possible to solve
 * this problem by trying every route. However, Problem 67, is
 * the same challenge with a triangle containing one-hundred
 * rows; it cannot be solved by brute force, and requires a
 * clever method! ;o)
 */

using System;
using System.Collections.Generic;
using System.Linq;

namespace Euler {
    /// <summary>
    /// This class represents the problem 18.
    /// </summary>
    public class PE018 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run() {
            var WORLD = new int[][] {
                new int[] {75},
                new int[] {95, 64},
                new int[] {17, 47, 82},
                new int[] {18, 35, 87, 10},
                new int[] {20, 04, 82, 47, 65},
                new int[] {19, 01, 23, 75, 03, 34},
                new int[] {88, 02, 77, 73, 07, 63, 67},
                new int[] {99, 65, 04, 28, 06, 16, 70, 92},
                new int[] {41, 41, 26, 56, 83, 40, 80, 70, 33},
                new int[] {41, 48, 72, 33, 47, 32, 37, 16, 94, 29},
                new int[] {53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14},
                new int[] {70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57},
                new int[] {91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48},
                new int[] {63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31},
                new int[] {04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23}
            };

            var result = FindPath(WORLD);

            Console.WriteLine($"The maximum total from top to bottom of the triangle is {result.Sum}.");
            Console.WriteLine($"Final path: {Tools.ToString(result.Path, ", ", "[", "]")}");
        }

        /// <summary>
        /// This type represents a path inside a world.
        /// </summary>
        record WorldPath(int Sum = -1, List<int> Path = null);

        /// <summary>
        /// Finds the maximum valued path inside a triangular world.
        /// </summary>
        /// <param name="world">The data that represents the world.</param>
        /// <returns>A world path object where 'Sum' is the value of the path, and 'Path'
        /// is the current list of elements of that path.</returns>
        WorldPath FindPath(int[][] world) {
            // On the first row of the world, just add to the values
            // of the initial path object with the root of the world:
            var wildcard = new WorldPath();
            var previous = new List<WorldPath>() {
                new WorldPath {
                    Sum = world[0][0],
                    Path = new List<int> { world[0][0] }
                }
            };
            // Check the following rows inside the given world:
            foreach (var row in world.Skip(1)) {
                var current = new List<WorldPath>();
                // Check each column of the row:
                foreach (var index in Tools.Sequence(row.Length)) {
                    // When the row isn't the first one, we'll select the
                    // left and right path objects from the previous results:
                    var left = (index - 1 >= 0) ? previous[index - 1] : wildcard;
                    var right = (index < previous.Count) ? previous[index] : wildcard;
                    // Then, we'll select the maximum value between the left
                    // and the right, to create a new path object result and
                    // add it to the current results:
                    var selected = (left.Sum >= right.Sum) ? left : right;
                    current.Add(new WorldPath {
                        Sum = selected.Sum + row[index],
                        Path = selected.Path.Append(row[index]).ToList()
                    });
                }
                // Update the previous results with the current ones:
                previous = current;
            }
            // Select the maximum valued path from the previous results:
            return previous.MaxBy(x => x.Sum);
        }
    }
}
