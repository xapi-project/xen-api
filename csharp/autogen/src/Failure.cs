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
using System.Resources;
using System.Collections;
using System.Text.RegularExpressions;
using System.Xml;
using System.Runtime.Serialization;

namespace XenAPI
{
    [Serializable]
    public partial class Failure : Exception
    {
        public const string INTERNAL_ERROR = "INTERNAL_ERROR";
        public const string MESSAGE_PARAMETER_COUNT_MISMATCH = "MESSAGE_PARAMETER_COUNT_MISMATCH";

        private static ResourceManager errorDescriptions = XenAPI.FriendlyErrorNames.ResourceManager;

        private readonly List<string> errorDescription;
        private string errorText;
        private string shortError;

        public List<string> ErrorDescription 
        {
            get 
            {
                return errorDescription;
            }
        }

        public string ShortMessage
        {
            get
            {
                return shortError;
            }
        }

        public override string Message
        {
            get
            {
                return errorText;
            }
        }

        public Failure() : base() { }

        public Failure(params string[] err)
            : this(new List<string>(err))
        {}

        public Failure(string message, Exception exception)
            : base(message, exception)
        {
            errorDescription = new List<string>() { message };
            Setup();
        }

        protected Failure(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
            errorDescription = (List<string>)info.GetValue("errorDescription", typeof(List<string>));
            errorText = info.GetString("errorText");
            shortError = info.GetString("shortError");
        }

        public Failure(List<string> errDescription)
        {
            errorDescription = errDescription;
            Setup();
        }

        public void Setup()
        {
            if (ErrorDescription.Count > 0)
            {
                try
                {
                    string formatString;
                    try
                    {
                        formatString = errorDescriptions.GetString(ErrorDescription[0]);
                    }
                    catch
                    {
                        formatString = null;
                    }

                    if (formatString == null)
                    {
                        // If we don't have a translation, just combine all the error results from the server
                        List<string> cleanBits = new List<string>();
                        foreach (string s in ErrorDescription)
                        {
                            // Only show non-empty bits of ErrorDescription.
                            // Also, trim the bits, since the server occasionally sends spurious newlines.
                            if (s.Trim().Length > 0)
                            {
                                cleanBits.Add(s.Trim());
                            }
                        }

                        this.errorText = string.Join(" - ", cleanBits.ToArray());
                    }
                    else
                    {

                        // We need a string array to pass to String.Format, and it must not contain the 0th element.

                        string[] objects = new string[ErrorDescription.Count - 1];

                        for (int i = 1; i < ErrorDescription.Count; i++)
                            objects[i - 1] = ErrorDescription[i];

                        this.errorText = String.Format(formatString, objects);
                    }
                }
                catch (Exception)
                {
                    this.errorText = ErrorDescription[0];
                }

                try
                {
                    shortError = errorDescriptions.GetString(ErrorDescription[0] + "-SHORT") ?? errorText;
                }
                catch (Exception)
                {
                    shortError = this.errorText;
                }

                // now try and parse CSLG failures (these have embedded xml)
                TryParseCslg();
            }
        }

        /// <summary>
        /// Tries the parse CSLG failures. These have embedded xml. The useful part (from the user's perspective) is copied to errorText.
        /// </summary>
        /// <returns>A value specifying whether a CSLG error was found.</returns>
        private bool TryParseCslg()
        {
            //failure.ErrorDescription[2]:
            //<StorageLinkServiceError>
            //    <Fault>Host ivory has not yet been added to the service. [err=Object was not found]</Fault>
            //    <Detail>
            //        <errorCode>6</errorCode>
            //        <messageId></messageId>
            //        <defaultMessage>Host ivory has not yet been added to the service. [err=Object was not found]</defaultMessage>
            //        <severity>2</severity>
            //        <errorFunction>CXSSHostUtil::getHost</errorFunction>
            //        <errorLine>113</errorLine>
            //        <errorFile>.\\xss_util_host.cpp</errorFile>
            //    </Detail>
            // </StorageLinkServiceError>

            if (ErrorDescription.Count > 2 && ErrorDescription[2] != null && ErrorDescription[0] != null && ErrorDescription[0].StartsWith("SR_BACKEND_FAILURE"))
            {
                Match m = Regex.Match(ErrorDescription[2], @"<StorageLinkServiceError>.*</StorageLinkServiceError>", RegexOptions.Singleline);

                if (m.Success)
                {
                    XmlDocument doc = new XmlDocument();

                    try
                    {
                        doc.LoadXml(m.Value);
                    }
                    catch (XmlException)
                    {
                        return false;
                    }

                    XmlNodeList nodes = doc.SelectNodes("/StorageLinkServiceError/Fault");

                    if (nodes != null && nodes.Count > 0 && !string.IsNullOrEmpty(nodes[0].InnerText))
                    {
                        if (string.IsNullOrEmpty(errorText))
                        {
                            errorText = nodes[0].InnerText;
                        }
                        else
                        {
                            errorText = string.Format("{0} ({1})", errorText, nodes[0].InnerText);
                        }
                        return true;
                    }
                }
            }
            return false;
        }

        public override string ToString()
        {
            return Message;
        }

        public override void GetObjectData(SerializationInfo info, StreamingContext context)
        {
            if (info == null)
            {
                throw new ArgumentNullException("info");
            }

            info.AddValue("errorDescription", errorDescription, typeof(List<string>));
            info.AddValue("errorText", errorText);
            info.AddValue("shortError", shortError);

            base.GetObjectData(info, context);
        }
    }
}
