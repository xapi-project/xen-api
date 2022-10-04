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
using System.Management.Automation;
using System.Net;
using System.Net.Security;
using System.Runtime.InteropServices;
using System.Security;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet("Connect", "XenServer")]
    public class ConnectXenServerCommand : PSCmdlet
    {
        private readonly object _certificateValidationLock = new object();

        public ConnectXenServerCommand()
        {
            Port = 443;
        }

        #region Cmdlet Parameters

        [Parameter(ParameterSetName = "Url", Mandatory = true, Position = 0)]
        public string[] Url { get; set; }

        [Parameter(ParameterSetName = "ServerPort", Mandatory = true, Position = 0)]
        [Alias("svr")]
        public string[] Server { get; set; }

        [Parameter(ParameterSetName = "ServerPort")]
        public int Port { get; set; }

        [Parameter(ValueFromPipeline = true, ValueFromPipelineByPropertyName = true)]
        [Alias("cred")]
        public PSCredential Creds { get; set; }

        [Parameter(Position = 1)]
        [Alias("user")]
        public string UserName { get; set; }

        [Parameter(Position = 2)]
        [Alias("pwd")]
        public string Password { get; set; }

        [Parameter]
        public string[] OpaqueRef { get; set; }

        [Parameter]
        public string Originator { get; set; } = "XenServerPSModule/" + Helper.APIVersionString(API_Version.LATEST);

        [Parameter]
        public SwitchParameter PassThru { get; set; }

        [Parameter]
        public SwitchParameter NoWarnNewCertificates { get; set; }

        [Parameter]
        public SwitchParameter NoWarnCertificates { get; set; }

        [Parameter]
        public SwitchParameter SetDefaultSession { get; set; }

        [Parameter]
        public SwitchParameter Force { get; set; }

        #endregion

        protected override void ProcessRecord()
        {
            if ((Url == null || Url.Length == 0) && (Server == null || Server.Length == 0))
            {
                ThrowTerminatingError(new ErrorRecord(
                      new Exception("You must provide a URL, Name or IP Address for the XenServer."),
                      "",
                      ErrorCategory.InvalidArgument,
                      null));
            }

            if (Creds == null &&
                (string.IsNullOrEmpty(UserName) || string.IsNullOrEmpty(Password)) &&
                 (OpaqueRef == null || OpaqueRef.Length == 0))
            {
                Creds = Host.UI.PromptForCredential("XenServer Credential Request",
                    "",
                    string.IsNullOrEmpty(UserName) ? "root" : UserName,
                    "");

                if (Creds == null)
                {
                    // Just bail out at this point, they've clicked cancel on the credentials pop up dialog
                    ThrowTerminatingError(new ErrorRecord(
                          new Exception("Credentials must be supplied when connecting to the XenServer."),
                          "",
                          ErrorCategory.InvalidArgument,
                          null));
                }
            }

            string connUser = "";
            string connPassword = "";

            if (OpaqueRef == null || OpaqueRef.Length == 0)
            {
                if (Creds == null)
                {
                    connUser = UserName;
                    connPassword = Password;

                    SecureString secPwd = new SecureString();
                    foreach (char ch in connPassword)
                        secPwd.AppendChar(ch);

                    Creds = new PSCredential(UserName, secPwd);
                }
                else
                {
                    connUser = Creds.UserName.StartsWith("\\") ? Creds.GetNetworkCredential().UserName : Creds.UserName;

                    IntPtr ptrPassword = Marshal.SecureStringToBSTR(Creds.Password);
                    connPassword = Marshal.PtrToStringBSTR(ptrPassword);
                    Marshal.FreeBSTR(ptrPassword);
                }
            }

            ServicePointManager.ServerCertificateValidationCallback = ValidateServerCertificate;
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;

            if (Url == null || Url.Length == 0)
            {
                Url = new string[Server.Length];
                for (int i = 0; i < Server.Length; i++)
                    Url[i] = CommonCmdletFunctions.GetUrl(Server[i], Port);
            }

            if (OpaqueRef == null)
            {
                OpaqueRef = new string[Url.Length];
            }
            else
            {
                if (OpaqueRef.Length != Url.Length)
                    ThrowTerminatingError(new ErrorRecord(
                        new Exception("The number of opaque references provided should be the same as the number of servers."),
                        "",
                        ErrorCategory.InvalidArgument,
                        null));
            }

            Dictionary<string, Session> sessions = CommonCmdletFunctions.GetAllSessions(this);
            Dictionary<string, Session> newSessions = new Dictionary<string, Session>();

            for (int i = 0; i < Url.Length; i++)
            {
                Session session;
                if (string.IsNullOrEmpty(OpaqueRef[i]))
                {
                    session = new Session(Url[i]);
                    try
                    {
                        session.login_with_password(connUser, connPassword, Helper.APIVersionString(API_Version.LATEST), Originator);
                    }
                    catch (Failure f)
                    {
                        if (f.ErrorDescription != null && f.ErrorDescription.Count > 1 && f.ErrorDescription[0] == "HOST_IS_SLAVE")
                        {
                            ThrowTerminatingError(new ErrorRecord(f, "", ErrorCategory.InvalidArgument, Url[i])
                            {
                                ErrorDetails = new ErrorDetails($"The host you are trying to connect to is a supporter. To make regular API calls, please connect to the pool coordinator (IP address: {f.ErrorDescription[1]}).")
                            });
                        }
                        else
                        {
                            throw;
                        }
                    }
                    catch (WebException e)
                    {
                        if (e.InnerException?.InnerException is CertificateValidationException ex)
                        {
                            if (ShouldContinue(ex.Message, ex.Caption))
                            {
                                AddCertificate(ex.Hostname, ex.Fingerprint);
                                i--;
                                continue;
                            }

                            ThrowTerminatingError(new ErrorRecord(ex, "", ErrorCategory.AuthenticationError, Url[i])
                            {
                                ErrorDetails = new ErrorDetails($"Certificate fingerprint rejected. ({ex.Fingerprint} - {ex.Hostname}).")
                            });
                        }

                        throw;
                    }
                }
                else
                {
                    session = new Session(Url[i], OpaqueRef[i]);
                }

                session.Tag = Creds;
                sessions[session.opaque_ref] = session;
                newSessions[session.opaque_ref] = session;

                if (i > 0)
                    continue;

                //set the first of the specified connections as default
                if (SetDefaultSession)
                {
                    WriteVerbose(string.Format("Setting connection {0} ({1}) as default.", session.Url, session.opaque_ref));
                    CommonCmdletFunctions.SetDefaultXenSession(this, session);
                }
            }

            CommonCmdletFunctions.SetAllSessions(this, sessions);

            if (PassThru)
                WriteObject(newSessions.Values, true);
        }

        private void AddCertificate(string hostname, string fingerprint)
        {
            var certificates = CommonCmdletFunctions.LoadCertificates();
            certificates[hostname] = fingerprint;
            CommonCmdletFunctions.SaveCertificates(certificates);
        }

        private bool ValidateServerCertificate(object sender, X509Certificate certificate, X509Chain chain, SslPolicyErrors sslPolicyErrors)
        {
            if (sslPolicyErrors == SslPolicyErrors.None)
                return true;

            lock (_certificateValidationLock)
            {
                bool ignoreChanged = Force || NoWarnCertificates || (bool)GetVariableValue("NoWarnCertificates", false);
                bool ignoreNew = Force || NoWarnNewCertificates || (bool)GetVariableValue("NoWarnNewCertificates", false);

                HttpWebRequest webreq = (HttpWebRequest)sender;
                string hostname = webreq.Address.Host;
                string fingerprint = CommonCmdletFunctions.FingerprintPrettyString(certificate.GetCertHashString());

                bool trusted = VerifyInAllStores(new X509Certificate2(certificate));

                var certificates = CommonCmdletFunctions.LoadCertificates();

                if (certificates.ContainsKey(hostname))
                {
                    string fingerprintOld = certificates[hostname];
                    if (fingerprintOld == fingerprint)
                        return true;

                    if (!ignoreChanged)
                        throw new CertificateChangedException(fingerprint, fingerprintOld, trusted, hostname);
                }
                else
                {
                    if (!ignoreNew)
                        throw new CertificateNotFoundException(fingerprint, trusted, hostname);
                }

                certificates[hostname] = fingerprint;
                CommonCmdletFunctions.SaveCertificates(certificates);
                return true;
            }
        }

        private bool VerifyInAllStores(X509Certificate2 certificate2)
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
    }

    internal abstract class CertificateValidationException : Exception
    {
        protected const string CERT_TRUSTED = "The certificate on this server is trusted. It is recommended you re-issue this server's certificate.";
        protected const string CERT_NOT_TRUSTED = "The certificate on this server is not trusted.";

        protected readonly bool Trusted;
        public readonly string Fingerprint;
        public readonly string Hostname;

        protected CertificateValidationException(string fingerprint, bool trusted, string hostname)
        {
            Fingerprint = fingerprint;
            Trusted = trusted;
            Hostname = hostname;
        }

        public abstract string Caption { get; }
    }

    internal class CertificateChangedException : CertificateValidationException
    {
        private readonly string _oldFingerprint;

        public CertificateChangedException(string fingerprint, string oldFingerprint, bool trusted, string hostname)
            : base(fingerprint, trusted, hostname)
        {
            _oldFingerprint = oldFingerprint;
        }

        public override string Caption => "Security Certificate Changed";

        public override string Message => $"The certificate fingerprint of the server you have connected to is:\n{Fingerprint}\n" +
                                          $"But was expected to be:\n{_oldFingerprint}\n" +
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

        public override string Message => $"The certificate fingerprint of the server you have connected to is :\n{Fingerprint}\n" +
                                          (Trusted ? CERT_TRUSTED : CERT_NOT_TRUSTED) +
                                          "\nDo you wish to continue?";
    }
}
