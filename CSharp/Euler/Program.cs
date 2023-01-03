//======================================================================
// Copyright (C) 2023, Gorka Suárez García
//======================================================================

using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace Euler {
    /// <summary>
    /// This class represents the main program entry.
    /// </summary>
    public static class Program {
        /// <summary>
        /// Main entry for the program to select a problem to solve.
        /// </summary>
        /// <param name="args">Command arguments.</param>
        public static void Main (string[] args) {
            problems = getProblems();
            try {
                bool run = true;
                while (run) {
                    // Get a number of problem as an input:
                    Console.Write($"Select a problem between 1 and {problems.Count()}: ");
                    var input = Console.ReadLine();
                    Console.WriteLine();
                    if (string.IsNullOrEmpty(input) || input == "0") {
                        run = false;
                    } else {
                        input = input.ToLower();
                        if (input == "a" || input == "all") {
                            runProblems();
                        } else {
                            runProblem(input);
                        }
                        Console.WriteLine();
                    }
                }
            } catch (Exception error) {
                Console.WriteLine(error);
            }
        }

        /// <summary>
        /// Runs all the types of problems.
        /// </summary>
        private static void runProblems () {
            foreach (var type in problems) {
                var problem = getInstance(type);
                Console.WriteLine($"{SEPARATOR}\nRUN: {type.Name}\n");
                problem?.Run();
                Console.WriteLine();

            }
            Console.WriteLine(SEPARATOR);
        }

        /// <summary>
        /// Runs a type of problem.
        /// </summary>
        /// <param name="input">The user input.</param>
        private static void runProblem (string input) {
            // Parse the text input into an integer:
            if (!int.TryParse(input, out int index)) {
                index = -1;
            }
            // Check if the input is a valid problem to run:
            if (1 <= index && index <= problems.Count()) {
                var name = $"PE{index:D3}";
                var problem = getInstance(name);
                problem?.Run();
            } else {
                Console.WriteLine($"[ERROR] Invalid input: {input}");
            }
        }

        /// <summary>
        /// Gets an instance of a type of problem.
        /// </summary>
        /// <param name="name">The name of the class.</param>
        /// <returns>The instance of the problem or null.</returns>
        private static IRunnable getInstance (string name) {
            var type = problems.FirstOrDefault(x => x.Name == name);
            return getInstance(type);
        }

        /// <summary>
        /// Gets an instance of a type of problem.
        /// </summary>
        /// <param name="type">The type of the class.</param>
        /// <returns>The instance of the problem or null.</returns>
        private static IRunnable getInstance (Type type) {
            if (type != null) {
                var constructor = type.GetConstructor(Type.EmptyTypes);
                var instance = constructor.Invoke(null);
                return instance as IRunnable;
            }
            return null;
        }

        /// <summary>
        /// Gets the current list of types of problems.
        /// </summary>
        /// <returns>The list of types of problems.</returns>
        private static IEnumerable<Type> getProblems () {
            const string space = "Euler";
            var types = Assembly.GetExecutingAssembly().GetTypes();
            return types.Where(x => x.IsClass && x.Name.StartsWith(PREFIX)
                                              && x.Namespace == space)
                        .OrderBy(x => x.Name)
                        .ToArray();
        }

        /// <summary>
        /// The problems prefix name.
        /// </summary>
        private const string PREFIX = "PE";

        /// <summary>
        /// The problems output separator.
        /// </summary>
        private const string SEPARATOR = "============================================================";

        /// <summary>
        /// The list of types with the problems.
        /// </summary>
        private static IEnumerable<Type> problems;
    }
}
