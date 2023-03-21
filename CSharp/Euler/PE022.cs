//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

/*
 * Using names.txt (right click and 'Save Link/Target As...'),
 * a 46K text file containing over five-thousand first names,
 * begin by sorting it into alphabetical order. Then working out
 * the alphabetical value for each name, multiply this value by
 * its alphabetical position in the list to obtain a name score.
 * 
 * For example, when the list is sorted into alphabetical order,
 * COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th
 * name in the list. So, COLIN would obtain a score of
 * 938 × 53 = 49714.
 * 
 * What is the total of all the name scores in the file?
 */

using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;

namespace Euler {
    /// <summary>
    /// This class represents the problem 22.
    /// </summary>
    public class PE022 : IRunnable {
        /// <summary>
        /// Main entry for the problem solver.
        /// </summary>
        public void Run () {
            const string URL = "https://projecteuler.net/project/resources/p022_names.txt";

            try {
                var names = LoadRemoteNames(URL);
                var scores = names.Order()
                                  .Select((name, index) => GetScore(index + 1, name));
                var result = scores.Select(score => score.Item2)
                                   .Sum();

                Console.WriteLine($"The total of all the name scores in the file is {result}.");
            } catch (Exception e) {
                Console.WriteLine($"ERROR: {e.Message}");
            }
        }

        /// <summary>
        /// Load a remote text in the web with the names.
        /// </summary>
        /// <param name="url">The URL of the text.</param>
        /// <returns>The sequence of strings with the names.</returns>
        IEnumerable<string> LoadRemoteNames(string url) {
            using var client = new HttpClient();
            var text = client.GetStringAsync(url).Result;
            return text.Split('\n', '\r', '\t', ',')
                       .Select(x => x.Trim('"'));
        }

        /// <summary>
        /// Gets the score for a name in a position.
        /// </summary>
        /// <param name="position">The position of the name.</param>
        /// <param name="name">The name to check.</param>
        /// <returns>A tuple with the name and the score.</returns>
        (string, int) GetScore (int position, string name) {
            return (name, position * name.Select(c => 1 + c - 'A').Sum());
        }
    }
}
