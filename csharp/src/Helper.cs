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
    public enum API_Version
    {
        API_1_1 = 1,  // XenServer 4.0 (codename Rio)
        API_1_2 = 2,  // XenServer 4.1 (Miami)
        API_1_3 = 3,  // XenServer 5.0 up to update 2 (Orlando)
        API_1_4 = 4,  // Unreleased
        API_1_5 = 5,  // XenServer 5.0 update 3 and above (Floodgate)
        API_1_6 = 6,  // XenServer 5.5 (George)
        API_1_7 = 7,  // XenServer 5.6 (Midnight Ride)
        API_1_8 = 8,  // XenServer 5.6.1 (Cowley)
        API_1_9 = 9,  // XenServer 6.0 (Boston)
        API_1_10 = 10, // XenServer 6.1 (Tampa)
        API_2_0 = 11, // XenServer 6.2 (Clearwater)
        API_2_1 = 12, // XenServer 6.2 with vGPU (vGPU)
        API_2_2 = 13, // XenServer 6.2 Hotfix XS62ESP1004 (Felton)
        LATEST = 13,
        // Don't forget to change LATEST above, and APIVersionString below.
        UNKNOWN = 99
    }

    public static class Helper
    {
        public const string NullOpaqueRef = "OpaqueRef:NULL";

        public static string APIVersionString(API_Version v)
        {
            switch (v)
            {
                case API_Version.API_1_1:
                    return "1.1";
                case API_Version.API_1_2:
                    return "1.2";
                case API_Version.API_1_3:
                    return "1.3";
                case API_Version.API_1_4:
                    return "1.4";
                case API_Version.API_1_5:
                    return "1.5";
                case API_Version.API_1_6:
                    return "1.6";
                case API_Version.API_1_7:
                    return "1.7";
                case API_Version.API_1_8:
                    return "1.8";
                case API_Version.API_1_9:
                    return "1.9";
                case API_Version.API_1_10:
                    return "1.10";
                case API_Version.API_2_0:
                    return "2.0";
                case API_Version.API_2_1:
                    return "2.1";
                case API_Version.API_2_2:
                    return "2.2";
                default:
                    return "Unknown";
            }
        }

        public static API_Version GetAPIVersion(long major, long minor)
        {
            try
            {
                return (API_Version)Enum.Parse(typeof(API_Version),
                    string.Format("API_{0}_{1}", major, minor));
            }
            catch (ArgumentException)
            {
                return API_Version.UNKNOWN;
            }
        }

        /// <summary>
        /// Converts the string representation of an API version number to its API_Version equivalent.
        /// This function assumes that API version numbers are of form a.b
        /// </summary>
        public static API_Version GetAPIVersion(string version)
        {
            if (version != null)
            {
                string[] tokens = version.Split('.');
                int major, minor;
                if (tokens.Length == 2 && int.TryParse(tokens[0], out major) && int.TryParse(tokens[1], out minor))
                {
                    return GetAPIVersion(major, minor);
                }
            }
            return API_Version.UNKNOWN;
        }

        /// <summary>
        /// Return a positive number if the given session's API version is greater than the given
        /// API_version, negative if it is less, and 0 if they are equal.
        /// </summary>
        internal static int APIVersionCompare(Session session, API_Version v)
        {
            return (int)session.APIVersion - (int)v;
        }

        /// <summary>
        /// Return true if the given session's API version is greater than or equal to the given
        /// API_version.
        /// </summary>
        internal static bool APIVersionMeets(Session session, API_Version v)
        {
            return APIVersionCompare(session, v) >= 0;
        }

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

        internal static List<Data_source> Proxy_Data_sourceArrayToData_sourceList(Proxy_Data_source[] input)
        {
            List<Data_source> result = new List<Data_source>();
            foreach (Proxy_Data_source pd in input)
            {
                result.Add(new Data_source(pd));
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

