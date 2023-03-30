/*
 * Copyright (c) Cloud Software Group, Inc.
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
using System.Collections.Generic;
using System.Collections;
using System.Linq;


namespace XenAPI
{
    public class Marshalling
    {
        /// <summary>
        /// Takes a Hashtable, creates a new t, and populates the fields of
        /// that t with the values from the Hashtable.
        /// </summary>
        /// <param name="t">A XenAPI type, VM for example.  t must have an associated XenAPI.Proxy_t.</param>
        /// <param name="table"></param>
        /// <returns></returns>
        public static object convertStruct(Type t, Hashtable table)
        {
            return t.GetConstructor(new Type[] { typeof(Hashtable) }).Invoke(new object[] { table });
        }

        public static Type GetXenAPIType(string name)
        {
            return Type.GetType(string.Format("XenAPI.{0}", name), false, true);
        }

        public static bool ParseBool(Hashtable table, string key)
        {
            bool.TryParse((string)table[key], out var result);
            return result;
        }

        public static DateTime ParseDateTime(Hashtable table, string key)
        {
            DateTime.TryParse((string)table[key], out var result);
            return result;
        }

        public static double ParseDouble(Hashtable table, string key)
        {
            double.TryParse((string)table[key], out var result);
            return result;
        }

        public static Hashtable ParseHashTable(Hashtable table, string key)
        {
            return ParseSxpDict((string)table[key]);
        }

        public static long ParseLong(Hashtable table, string key)
        {
            long.TryParse((string)table[key], out var result);
            return result;
        }

        public static string ParseString(Hashtable table, string key)
        {
            return (string)table[key];
        }

        public static string[] ParseStringArray(Hashtable table, string key)
        {
            return ParseSxpList((string)table[key]).ToArray();
        }

        public static long[] ParseLongArray(Hashtable table, string key)
        {
            return ParseSxpList((string)table[key]).Select(long.Parse).ToArray();
        }

        public static XenRef<T> ParseRef<T>(Hashtable table, string key) where T : XenObject<T>
        {
            var val = (string)table[key];
            return val == null ? null : XenRef<T>.Create(val);
        }

        public static List<XenRef<T>> ParseSetRef<T>(Hashtable table, string key) where T : XenObject<T>
        {
            return ParseSxpList((string)table[key]).Select(XenRef<T>.Create).ToList();
        }


        private static Hashtable ParseSxpDict(string p)
        {
            var result = new Hashtable();

            using (var enumerator = Tokenize(p).GetEnumerator())
            {
                if (!enumerator.MoveNext())
                    return result;

                while (enumerator.MoveNext())
                {
                    if (enumerator.Current == ")")
                        break;

                    enumerator.MoveNext();
                    var key = enumerator.Current;
                    enumerator.MoveNext();
                    var value = enumerator.Current;
                    enumerator.MoveNext();

                    result[key] = value;
                }

                return result;
            }
        }

        private static List<string> ParseSxpList(string p)
        {
            var result = new List<string>();

            foreach (var token in Tokenize(p))
            {
                if (token == "(" || token == ")")
                    continue;

                result.Add(token);
            }

            return result;
        }

        private static IEnumerable<string> Tokenize(string str)
        {
            bool inStr = false;
            int j = 0;

            for (int i = 0; i < str.Length; i++)
            {
                switch (str[i])
                {
                    case '(':
                        if (!inStr)
                            yield return "(";
                        break;
                    case ')':
                        if (!inStr)
                            yield return ")";
                        break;

                    case '\'':
                    case '"':
                        if (!inStr)
                        {
                            inStr = true;
                            j = i;
                        }
                        else if (str[i - 1] != '\\')
                        {
                            inStr = false;
                            yield return str.Substring(j + 1, i - j - 1).Replace("\\\"", "\"").Replace("\\\'", "\'");
                        }

                        break;
                }
            }
        }
    }
}
