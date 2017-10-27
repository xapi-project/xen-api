/*
 * Copyright (c) Citrix Systems, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1) Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2) Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following
 *      disclaimer in the documentation and/or other materials
 *      provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

using System;
using System.Collections;
using System.Collections.Generic;


namespace XenAPI
{
    public static partial class Helper
    {
        public const string NullOpaqueRef = "OpaqueRef:NULL";

        /// <summary>
        /// Test to see if two objects are equal.  If the objects implement ICollection, then we will
        /// call AreCollectionsEqual to compare elements within the collection.
        /// </summary>
        public static bool AreEqual(object o1, object o2)
        {
            if (o1 == null && o2 == null)
                return true;
            if (o1 == null || o2 == null)
                return false;
            if (o1 is IDictionary)
                return AreDictEqual((IDictionary)o1, (IDictionary)o2);
            if (o1 is System.Collections.ICollection)
                return AreCollectionsEqual((ICollection)o1, (ICollection)o2);
            return o1.Equals(o2);
        }

        /// <summary>
        /// Test to see if two objects are equal. Different from AreEqual in that this function
        /// considers an empty Collection and null to be equal.
        /// </summary>
        public static bool AreEqual2<T>(T o1, T o2)
        {
            if (o1 == null && o2 == null)
                return true;
            if (o1 == null || o2 == null)
                return o1 == null && IsEmptyCollection(o2) || o2 == null && IsEmptyCollection(o1);
            if (typeof(T) is IDictionary)
                return AreDictEqual((IDictionary)o1, (IDictionary)o2);
            if (typeof(T) is System.Collections.ICollection)
                return AreCollectionsEqual((ICollection)o1, (ICollection)o2);
            return o1.Equals(o2);
        }

        private static bool IsEmptyCollection(object obj)
        {
            ICollection collection = obj as ICollection;
            return collection != null && collection.Count == 0;
        }

        /// <summary>
        /// Test to see if two dictionaries are equal.  This dictionary comparison method places a call to AreEqual
        /// for underlying objects.
        /// </summary>
        private static bool AreDictEqual(IDictionary d1, IDictionary d2)
        {
            if (d1.Count != d2.Count)
                return false;
            foreach (object k in d1.Keys)
            {
                if (!d2.Contains(k) || !AreEqual(d2[k], d1[k]))
                    return false;
            }
            return true;
        }

        /// <summary>
        /// Test to see if two collections are equal.  The collections are equal if they have the same elements,
        /// in the same order, and quantity.  Elements are equal if their values are equal.
        /// </summary>
        private static bool AreCollectionsEqual(ICollection c1, ICollection c2)
        {
            if (c1.Count != c2.Count)
                return false;

            IEnumerator c1Enum = c1.GetEnumerator();
            IEnumerator c2Enum = c2.GetEnumerator();

            while (c1Enum.MoveNext() && c2Enum.MoveNext())
            {
                if (!AreEqual(c1Enum.Current, c2Enum.Current))
                    return false;
            }

            return true;
        }

        public static bool DictEquals<K, V>(Dictionary<K, V> d1,
                                              Dictionary<K, V> d2)
        {
            if (d1 == null && d2 == null)
                return true;
            if (d1 == null || d2 == null)
                return false;

            if (d1.Count != d2.Count)
                return false;
            foreach (K k in d1.Keys)
            {
                if (!d2.ContainsKey(k) || !EqualOrEquallyNull(d2[k], d1[k]))
                    return false;
            }
            return true;
        }

        internal static bool EqualOrEquallyNull(object o1, object o2)
        {
            return o1 == null ? o2 == null : o1.Equals(o2);
        }

        /// <summary>
        ///
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="opaqueRefs">Must not be null.</param>
        /// <returns></returns>
        internal static string[] RefListToStringArray<T>(List<XenRef<T>> opaqueRefs) where T : XenObject<T>
        {
            string[] result = new string[opaqueRefs.Count];
            int i = 0;
            foreach (XenRef<T> opaqueRef in opaqueRefs)
                result[i++] = opaqueRef.opaque_ref;
            return result;
        }

        public static bool IsNullOrEmptyOpaqueRef(string opaqueRef)
        {
            return string.IsNullOrEmpty(opaqueRef) || (string.Compare(opaqueRef, NullOpaqueRef, true) == 0);
        }

        /// <summary>
        /// Converts a List of objects into a string array by calling the ToString() method of each list element.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="list">Must not be null. Must not contain null. May contain no elements.</param>
        /// <returns></returns>
        internal static string[] ObjectListToStringArray<T>(List<T> list)
        {
            string[] result = new string[list.Count];
            int i = 0;
            foreach (T t in list)
                result[i++] = t.ToString();
            return result;
        }

        /// <summary>
        /// Parses an array of strings into a List of members of the given enum T.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="input">Must not be null. Must not contain null. May have Length zero.</param>
        /// <returns></returns>
        internal static List<T> StringArrayToEnumList<T>(string[] input)
        {
            List<T> result = new List<T>();
            foreach (string s in input)
            {
                try
                {
                    result.Add((T)Enum.Parse(typeof(T), s));
                }
                catch (ArgumentException)
                {
                }
            }
            return result;
        }

        /// <summary>
        /// Parses an array of strings into an Array of longs
        /// </summary>
        /// <param name="input">Must not be null. Must not contain null. May have Length zero.</param>
        /// <returns></returns>
        internal static long[] StringArrayToLongArray(string[] input)
        {
            long[] result = new long[input.Length];
            for(int i=0; i<input.Length; i++)
            {
                try
                {
                    result[i]=long.Parse(input[i]);
                }
                catch (ArgumentException)
                {
                }
            }
            return result;
        }

        /// <summary>
        /// Parses an array of longs into an Array of strings
        /// </summary>
        /// <param name="input">Must not be null. Must not contain null. May have Length zero.</param>
        /// <returns></returns>
        internal static string[] LongArrayToStringArray(long[] input)
        {
            string[] result = new string[input.Length];
            for(int i=0; i<input.Length; i++)
            {
                try
                {
                    result[i]=input[i].ToString();
                }
                catch (ArgumentException)
                {
                }
            }
            return result;
        }


        /// <summary>
        /// Parses an array of objects into a List of members of the given enum T by first calling ToString() on each array element.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="input">Must not be null. Must not contain null. May have Length zero.</param>
        /// <returns></returns>
        internal static List<T> ObjectArrayToEnumList<T>(object[] input)
        {
            List<T> result = new List<T>();
            foreach (object o in input)
            {
                try
                {
                    result.Add((T)Enum.Parse(typeof(T), o.ToString()));
                }
                catch (ArgumentException)
                {
                }
            }
            return result;
        }

        internal static List<Message> Proxy_MessageArrayToMessageList(Proxy_Message[] input)
        {
            List<Message> result = new List<Message>();
            foreach (Proxy_Message pm in input)
            {
                result.Add(new Message(pm));
            }
            return result;
        }

        internal static Object EnumParseDefault(Type t, string s)
        {
            try
            {
                return Enum.Parse(t, s == null ? null : s.Replace('-','_'));
            }
            catch (ArgumentException)
            {
                try
                {
                    return Enum.Parse(t, "unknown");
                }
                catch (ArgumentException)
                {
                    try
                    {
                        return Enum.Parse(t, "Unknown");
                    }
                    catch (ArgumentException)
                    {
                        return 0;
                    }
                }
            }
        }
    }
}

