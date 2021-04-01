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
using System.Text;

namespace XenAPI
{
    public partial class Subject
    {
        public static readonly string SUBJECT_NAME_KEY = "subject-name";
        public static readonly string SUBJECT_DISPLAYNAME_KEY = "subject-displayname";
        public static readonly string SUBJECT_IS_GROUP_KEY = "subject-is-group";

        /// <summary>
        /// Will return null if there is no subject-displayname in other_config.
        /// </summary>
        public string DisplayName
        {
            get
            {
                string result;
                if (other_config.TryGetValue(SUBJECT_DISPLAYNAME_KEY, out result))
                    return result;
                else
                    return null;
            }
        }

        /// <summary>
        /// Will return null if there is no subject-name in other_config.
        /// </summary>
        public string SubjectName
        {
            get
            {
                string result;
                if (other_config.TryGetValue(SUBJECT_NAME_KEY, out result))
                    return result;
                else
                    return null;
            }
        }

        /// <summary>
        /// Will return false if there is no subject-is-group in other_config.
        /// </summary>
        public bool IsGroup
        {
            get
            {
                string value;
                if (other_config.TryGetValue(SUBJECT_IS_GROUP_KEY, out value))
                {
                    bool result;
                    if (bool.TryParse(value, out result))
                        return result;
                }
                return false;
            }
        }
    }
}
