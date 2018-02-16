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
using System.Collections.Generic;
using System.Collections;


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
            return t.GetConstructor(new Type[] {typeof(Hashtable)}).Invoke(new object[] {table});
        }

        public static Type GetXenAPIType(string name)
        {
            return Type.GetType(string.Format("XenAPI.{0}", name), false, true);
        }

        public static bool ParseBool(Hashtable table, string key)
        {
            return table.ContainsKey(key)
                ? (bool)table[key]
                : false;
        }

        public static DateTime ParseDateTime(Hashtable table, string key)
        {
            return table.ContainsKey(key)
                ? (DateTime)table[key]
                : DateTime.MinValue;
        }

        public static double ParseDouble(Hashtable table, string key)
        {
            return table.ContainsKey(key)
                ? (double)table[key]
                : 0.0;
        }

        public static Hashtable ParseHashTable(Hashtable table, string key)
        {
            return table.ContainsKey(key)
                ? (Hashtable)table[key]
                : null;
        }

        public static long ParseLong(Hashtable table, string key)
        {
            return table.ContainsKey(key)
                ? long.Parse((string)table[key])
                : 0;
        }

        public static string ParseString(Hashtable table, string key)
        {
            return table.ContainsKey(key)
                ? (string)table[key]
                : null;
        }

        public static string[] ParseStringArray(Hashtable table, string key)
        {
            return table.ContainsKey(key)
                ? Array.ConvertAll((object[])table[key], o => o.ToString())
                : new string[0];
        }

        public static long[] ParseLongArray(Hashtable table, string key)
        {
            return table.ContainsKey(key)
                ? Array.ConvertAll((object[])table[key], o => long.Parse(o.ToString()))
                : new long[0];
        }

        public static XenRef<T> ParseRef<T>(Hashtable table, string key) where T : XenObject<T>
        {
            return table.ContainsKey(key)
                ? XenRef<T>.Create((string)table[key])
                : null;
        }

        public static List<XenRef<T>> ParseSetRef<T>(Hashtable table, string key) where T : XenObject<T>
        {
            return table.ContainsKey(key)
                ? XenRef<T>.Create((object[])table[key])
                : null;
        }

        public static Dictionary<XenRef<T>, T> ParseMapRefRecord<T, U>(Hashtable table, string key) where T : XenObject<T>
        {
            return table.ContainsKey(key)
                ? XenRef<T>.Create<U>((Hashtable)table[key])
                : null;
        }
    }
}
