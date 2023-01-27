//==============================================================================
// Copyright (C) 2023, Gorka Suárez García
//==============================================================================

using System;
using System.Collections;
using System.Collections.Generic;

namespace Euler {
    /// <summary>
    /// This class representes a generic lazy sequence of elements.
    /// </summary>
    /// <typeparam name="T">The type of the elements.</typeparam>
    public class LazySequence<T> : IEnumerable<T> {
        //----------------------------------------------------------------------
        // Fields
        //----------------------------------------------------------------------

        /// <summary>
        /// The inner buffer with the current values of the sequence.
        /// </summary>
        private IList<T> values;

        /// <summary>
        /// The default value when requesting out of bounds indexes.
        /// </summary>
        private T defaultValue;

        /// <summary>
        /// The function that makes the inner list value.
        /// </summary>
        private Func<IList<T>> makeNewList;

        /// <summary>
        /// The function that makes the next value.
        /// </summary>
        private Func<IList<T>, T> getNextValue;

        /// <summary>
        /// The default function that makes the inner list value.
        /// </summary>
        private readonly Func<IList<T>> defaultMakeNewList = () => new List<T>();

        //----------------------------------------------------------------------
        // Properties
        //----------------------------------------------------------------------

        /// <summary>
        /// Gets the current number of elements contained in the sequence.
        /// </summary>
        public int Count => values.Count;

        /// <summary>
        /// Gets the element at the specified index in the sequence.
        /// </summary>
        /// <param name="index">The zero-based index of the element to get.</param>
        /// <returns>The element at the specified index.</returns>
        public T this[int index] {
            get => Get(index);
        }

        //----------------------------------------------------------------------
        // Methods
        //----------------------------------------------------------------------

        /// <summary>
        /// Makes a new lazy sequence object.
        /// </summary>
        /// <param name="getNextValue">The function that makes the next value.</param>
        /// <param name="makeNewList">The function that makes the inner list value.</param>
        /// <param name="defaultValue">The default value when requesting out of bounds indexes.</param>
        public LazySequence (Func<IList<T>, T> getNextValue,
            Func<IList<T>> makeNewList = null, T defaultValue = default) {
            this.getNextValue = getNextValue;
            this.makeNewList = makeNewList ?? defaultMakeNewList;
            this.defaultValue = defaultValue;
            this.values = makeNewList();
        }

        /// <summary>
        /// Gets the element at the specified index in the sequence.
        /// </summary>
        /// <param name="index">The zero-based index of the element to get.</param>
        /// <returns>The element at the specified index.</returns>
        public T Get (int index) {
            if (index < 0) {
                return defaultValue;
            }
            while (index >= values.Count) {
                values.Add(getNextValue(values));
            }
            return values[index];
        }

        /// <summary>
        /// Resets the items from the list.
        /// </summary>
        public void Reset () {
            values = makeNewList();
        }

        /// <summary>
        /// Determines whether the list contains a specific value.
        /// </summary>
        /// <param name="item">The object to locate in the list.</param>
        /// <returns>True if item is found in the list; otherwise, false.</returns>
        public bool Contains (T item) {
            return values.Contains(item);
        }

        /// <summary>
        /// Determines the index of a specific item in the list.
        /// </summary>
        /// <param name="item">The object to locate in the list.</param>
        /// <returns>The index of item if found in the list; otherwise, -1.</returns>
        public int IndexOf (T item) {
            return values.IndexOf(item);
        }

        //----------------------------------------------------------------------
        // Interface Methods
        //----------------------------------------------------------------------

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>An enumerator that can be used to iterate through the collection.</returns>
        public IEnumerator<T> GetEnumerator () {
            return new Enumerator(this);
        }

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>An enumerator that can be used to iterate through the collection.</returns>
        IEnumerator IEnumerable.GetEnumerator () {
            return new Enumerator(this);
        }

        //----------------------------------------------------------------------
        // Inner Types
        //----------------------------------------------------------------------

        /// <summary>
        /// This class supports a simple iteration over the sequence.
        /// </summary>
        public class Enumerator : IEnumerator<T> {
            /// <summary>
            /// The current index to query.
            /// </summary>
            private int index;

            /// <summary>
            /// The lazy sequence to enumerate.
            /// </summary>
            private LazySequence<T> values;

            /// <summary>
            /// Makes a new enumerator object for a lazy sequence.
            /// </summary>
            /// <param name="victim">The lazy sequence to enumerate.</param>
            public Enumerator (LazySequence<T> victim) {
                values = victim;
                index = -1;
            }

            /// <summary>
            /// Gets the element in the collection at the current position of the enumerator.
            /// </summary>
            public T Current => values.Get(index);

            /// <summary>
            /// Gets the element in the collection at the current position of the enumerator.
            /// </summary>
            object IEnumerator.Current => values.Get(index);

            /// <summary>
            /// Performs application-defined tasks associated with freeing, releasing,
            /// or resetting unmanaged resources.
            /// </summary>
            public void Dispose () {
                values = null;
            }

            /// <summary>
            /// Advances the enumerator to the next element of the collection.
            /// </summary>
            /// <returns>true if the enumerator was successfully advanced to the next element;
            /// false if the enumerator has passed the end of the collection.</returns>
            public bool MoveNext () {
                index++;
                return true;
            }

            /// <summary>
            /// Sets the enumerator to its initial position, which is before the first
            /// element in the collection.
            /// </summary>
            public void Reset () {
                index = -1;
            }
        }
    }
}
