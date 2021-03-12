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
using System.Linq;
using System.Resources;
using System.Runtime.Serialization;
using System.Text.RegularExpressions;
using System.Xml;
using Newtonsoft.Json.Linq;


namespace XenAPI
{
    [Serializable]
    public partial class Failure : Exception
    {
        public const string INTERNAL_ERROR = "INTERNAL_ERROR";
        public const string MESSAGE_PARAMETER_COUNT_MISMATCH = "MESSAGE_PARAMETER_COUNT_MISMATCH";

        private static ResourceManager errorDescriptions = FriendlyErrorNames.ResourceManager;

        private readonly List<string> errorDescription = new List<string>();
        private string errorText;
        private string shortError;

        public List<string> ErrorDescription
        {
            get { return errorDescription; }
        }

        public string ShortMessage
        {
            get { return shortError; }
        }

        public override string Message
        {
            get { return errorText; }
        }

        #region Constructors

        public Failure()
        {}

        public Failure(params string[] err)
            : this(new List<string>(err))
        {}

        public Failure(List<string> errDescription)
        {
            errorDescription = errDescription;
            ParseExceptionMessage();
        }

        public Failure(string message, Exception exception)
            : base(message, exception)
        {
            errorDescription = new List<string> {message};
            ParseExceptionMessage();
        }

        protected Failure(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
            errorDescription = (List<string>)info.GetValue("errorDescription", typeof(List<string>));
            errorText = info.GetString("errorText");
            shortError = info.GetString("shortError");
        }

        #endregion

        private void ParseExceptionMessage()
        {
            if (ErrorDescription.Count <= 0)
                return;

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
                    // Only show non-empty bits of ErrorDescription.
                    // Also, trim the bits because the server occasionally sends spurious newlines.

                    var cleanBits = (from string s in ErrorDescription
                        let trimmed = s.Trim()
                        where trimmed.Length > 0
                        select trimmed).ToArray();

                    errorText = string.Join(" - ", cleanBits);
                }
                else
                {
                    // We need a string array to pass to String.Format, and it must not contain the 0th element
                    errorText = string.Format(formatString, ErrorDescription.Skip(1).Cast<object>().ToArray());
                }
            }
            catch (Exception)
            {
                errorText = ErrorDescription[0];
            }

            //call these before setting the shortError because they modify the errorText
            ParseSmapiV3Failures();
            ParseCslgFailures();

            try
            {
                shortError = errorDescriptions.GetString(ErrorDescription[0] + "-SHORT") ?? errorText;
            }
            catch (Exception)
            {
                shortError = errorText;
            }
        }

        /// <summary>
        /// The ErrorDescription[2] of SmapiV3 failures contains embedded json.
        /// This method parses it and copies the user friendly part to errorText.
        /// </summary>
        private void ParseSmapiV3Failures()
        {
            /* Example ErrorDescription:
             * [
             *     "SR_BACKEND_FAILURE",
             *     "TransportException",
             *     "{\"error\": \"Unable to connect to iSCSI service on target\"}"
             * ]
             */

            if (ErrorDescription.Count < 3 || string.IsNullOrEmpty(ErrorDescription[0]) || !ErrorDescription[0].StartsWith("SR_BACKEND_FAILURE"))
                return;

            try
            {
                var obj = JObject.Parse(ErrorDescription[2]); //will throw exception if ErrorDescription[2] is a simple string
                errorText = (string)obj.SelectToken("error") ?? errorText;
            }
            catch
            {
                //ignore
            }
        }

        /// <summary>
        /// The ErrorDescription[2] of Cslg failures contains embedded xml.
        /// This method parses it and copies the user friendly part to errorText.
        /// </summary>
        private void ParseCslgFailures()
        {
            /* ErrorDescription[2] example:

            <StorageLinkServiceError>
                <Fault>Host ivory has not yet been added to the service. [err=Object was not found]</Fault>
                <Detail>
                    <errorCode>6</errorCode>
                    <messageId></messageId>
                    <defaultMessage>Host ivory has not yet been added to the service. [err=Object was not found]</defaultMessage>
                    <severity>2</severity>
                    <errorFunction>CXSSHostUtil::getHost</errorFunction>
                    <errorLine>113</errorLine>
                    <errorFile>.\\xss_util_host.cpp</errorFile>
                </Detail>
            </StorageLinkServiceError>
            */

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
                        return;
                    }

                    XmlNodeList nodes = doc.SelectNodes("/StorageLinkServiceError/Fault");

                    if (nodes != null && nodes.Count > 0 && !string.IsNullOrEmpty(nodes[0].InnerText))
                    {
                        errorText = string.IsNullOrEmpty(errorText)
                            ? nodes[0].InnerText
                            : string.Format("{0} ({1})", errorText, nodes[0].InnerText);
                    }
                }
            }
        }

        public override string ToString()
        {
            return Message;
        }

        public override void GetObjectData(SerializationInfo info, StreamingContext context)
        {
            if (info == null)
                throw new ArgumentNullException("info");

            info.AddValue("errorDescription", errorDescription, typeof(List<string>));
            info.AddValue("errorText", errorText);
            info.AddValue("shortError", shortError);

            base.GetObjectData(info, context);
        }
    }
}