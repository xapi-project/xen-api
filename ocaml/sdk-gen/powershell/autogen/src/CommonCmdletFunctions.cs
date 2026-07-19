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
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Management.Automation;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using XenAPI;

namespace Citrix.XenServer
{
    internal class CommonCmdletFunctions
    {
        private const string SessionsVariable = "global:Citrix.XenServer.Sessions";
        private const string DefaultSessionVariable = "global:XenServer_Default_Session";
        private const string CertificatesPathVariable = "global:KnownServerCertificatesFilePath";

        internal static Dictionary<string, Session> GetAllSessions(PSCmdlet cmdlet)
        {
            object obj = cmdlet.SessionState.PSVariable.GetValue(SessionsVariable);
            return obj as Dictionary<string, Session> ?? new Dictionary<string, Session>();
        }

        /// <summary>
        /// Save the session dictionary as a PowerShell variable. It includes the
        /// PSCredential so, in case the session times out, any cmdlet can try to
        /// remake it
        /// </summary>
        internal static void SetAllSessions(PSCmdlet cmdlet, Dictionary<string, Session> sessions)
        {
            cmdlet.SessionState.PSVariable.Set(SessionsVariable, sessions);
        }

        internal static Session GetDefaultXenSession(PSCmdlet cmdlet)
        {
            return cmdlet.SessionState.PSVariable.GetValue(DefaultSessionVariable) as Session;
        }

        internal static void SetDefaultXenSession(PSCmdlet cmdlet, Session session)
        {
            cmdlet.SessionState.PSVariable.Set(DefaultSessionVariable, session);
        }

        internal static string GetUrl(string hostname, int port)
        {
            return $"{(port == 80 ? "http" : "https")}://{hostname}:{port}";
        }

        internal static string FingerprintPrettyString(string fingerprint)
        {
            List<string> pairs = new List<string>();
            while (fingerprint.Length > 1)
            {
                pairs.Add(fingerprint.Substring(0, 2));
                fingerprint = fingerprint.Substring(2);
            }

            if (fingerprint.Length > 0)
                pairs.Add(fingerprint);
            return string.Join(":", pairs.ToArray());
        }

        internal static Dictionary<T, S> ConvertHashTableToDictionary<T, S>(Hashtable tbl)
        {
            if (tbl == null)
                return null;

            var dict = new Dictionary<T, S>();
            foreach (DictionaryEntry entry in tbl)
                dict.Add((T)entry.Key, (S)entry.Value);

            return dict;
        }

        internal static Hashtable ConvertDictionaryToHashtable<T, S>(Dictionary<T, S> dict)
        {
            if (dict == null)
                return null;

            var tbl = new Hashtable();
            foreach (KeyValuePair<T, S> pair in dict)
                tbl.Add(pair.Key, pair.Value);

            return tbl;
        }

        internal static object EnumParseDefault(Type t, string s)
        {
            try
            {
                if (s == null)
                    s = string.Empty;

                return Enum.Parse(t, s.Replace('-', '_'));
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

        internal static bool VerifyInAllStores(X509Certificate2 certificate2)
        {
            try
            {
                X509Chain chain = new X509Chain(true);
                return chain.Build(certificate2) || certificate2.Verify();
            }
            catch (CryptographicException)
            {
                return false;
            }
        }

        internal static string GetCertificatesPath(PSCmdlet cmdlet)
        {
            var certPathObject = cmdlet.SessionState.PSVariable.GetValue(CertificatesPathVariable);

            return certPathObject is PSObject psObject
                ? psObject.BaseObject as string
                : certPathObject?.ToString() ?? string.Empty;
        }

        internal static Dictionary<string, string> LoadCertificates(string certPath)
        {
            var certificates = new Dictionary<string, string>();

            if (File.Exists(certPath))
            {
                var doc = new XmlDocument();
                doc.Load(certPath);

                foreach (XmlNode node in doc.GetElementsByTagName("certificate"))
                {
                    var hostAtt = node.Attributes?["hostname"];
                    var fngprtAtt = node.Attributes?["fingerprint"];

                    if (hostAtt != null && fngprtAtt != null)
                        certificates[hostAtt.Value] = fngprtAtt.Value;
                }
            }

            return certificates;
        }

        internal static void SaveCertificates(string certPath, Dictionary<string, string> certificates)
        {
            string dirName = Path.GetDirectoryName(certPath);

            if (!Directory.Exists(dirName))
                Directory.CreateDirectory(dirName);

            XmlDocument doc = new XmlDocument();
            XmlDeclaration decl = doc.CreateXmlDeclaration("1.0", "utf-8", null);
            doc.AppendChild(decl);
            XmlNode node = doc.CreateElement("certificates");

            foreach (KeyValuePair<string, string> cert in certificates)
            {
                XmlNode certNode = doc.CreateElement("certificate");
                XmlAttribute hostname = doc.CreateAttribute("hostname");
                XmlAttribute fingerprint = doc.CreateAttribute("fingerprint");
                hostname.Value = cert.Key;
                fingerprint.Value = cert.Value;
                certNode.Attributes?.Append(hostname);
                certNode.Attributes?.Append(fingerprint);
                node.AppendChild(certNode);
            }

            doc.AppendChild(node);
            doc.Save(certPath);
        }
    }

    internal abstract class CertificateValidationException : Exception
    {
        protected const string CERT_TRUSTED = "The certificate on this server is trusted. It is recommended you re-issue this server's certificate.";
        protected const string CERT_NOT_TRUSTED = "The certificate on this server is not trusted.";

        protected CertificateValidationException(string fingerprint, bool trusted, string hostname)
        {
            Fingerprint = fingerprint;
            Trusted = trusted;
            Hostname = hostname;
        }

        protected bool Trusted { get; }
        public string Fingerprint { get; }
        public string Hostname { get; }
        public abstract string Caption { get; }
    }

    internal class CertificateChangedException : CertificateValidationException
    {
        public CertificateChangedException(string fingerprint, bool trusted, string hostname)
            : base(fingerprint, trusted, hostname)
        {
        }

        public override string Caption => "Security Certificate Changed";

        public override string Message =>
            $"The certificate thumbprint of server {Hostname} has changed since the last time you connected.\n" +
            $"The certificate thumbprint of the server is:\n{Fingerprint}\n" +
            (Trusted ? CERT_TRUSTED : CERT_NOT_TRUSTED) +
            "\nDo you wish to continue?";
    }

    internal class CertificateNotFoundException : CertificateValidationException
    {
        public CertificateNotFoundException(string fingerprint, bool trusted, string hostname)
            : base(fingerprint, trusted, hostname)
        {
        }

        public override string Caption => "New Security Certificate";

        public override string Message => $"The certificate thumbprint of the server you have connected to is :\n{Fingerprint}\n" +
                                          (Trusted ? CERT_TRUSTED : CERT_NOT_TRUSTED) +
                                          "\nDo you wish to continue?";
    }
}
