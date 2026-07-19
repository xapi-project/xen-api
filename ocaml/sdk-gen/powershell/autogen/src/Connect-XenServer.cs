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
using System.Management.Automation;
using System.Net;
#if NET8_0_OR_GREATER
using System.Net.Http;
#endif
using System.Net.Security;
using System.Runtime.InteropServices;
using System.Security;
using System.Security.Cryptography.X509Certificates;
using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet("Connect", "XenServer")]
    public class ConnectXenServerCommand : PSCmdlet
    {
        private static readonly string DefaultUserAgent = "XenServerPSModule/@SDK_VERSION@";
        private static readonly object CertificateValidationLock = new object();

        public ConnectXenServerCommand()
        {
            Port = 443;
            Originator = DefaultUserAgent;
            UserAgent = DefaultUserAgent;
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
        public string Originator { get; set; }

        [Parameter(HelpMessage = "The UserAgent to use for the requests to the server")]
        public string UserAgent { get; set; }

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
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12;

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
                    session = new Session(Url[i]) { UserAgent = UserAgent };
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
                    catch (Exception e)
                    {
                        var inner = e.InnerException?.InnerException ?? //.NET case
                                    e.InnerException; //.NET Framework case

                        if (inner is CertificateValidationException ex)
                        {
                            if (ShouldContinue(ex.Message, ex.Caption))
                            {
                                var certPath = CommonCmdletFunctions.GetCertificatesPath(this);
                                var certificates = CommonCmdletFunctions.LoadCertificates(certPath);
                                certificates[ex.Hostname] = ex.Fingerprint;
                                CommonCmdletFunctions.SaveCertificates(certPath, certificates);
                                i--;
                                continue;
                            }

                            ThrowTerminatingError(new ErrorRecord(ex, "", ErrorCategory.AuthenticationError, Url[i])
                            {
                                ErrorDetails = new ErrorDetails($"Certificate fingerprint rejected. ({ex.Fingerprint} - {ex.Hostname}).")
                            });
                        }

                        if (inner != null)
                            throw inner;

                        throw;
                    }
                }
                else
                {
                    session = new Session(Url[i], OpaqueRef[i]){ UserAgent = UserAgent };
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

        private bool ValidateServerCertificate(object sender, X509Certificate certificate, X509Chain chain,
            SslPolicyErrors sslPolicyErrors)
        {
            if (sslPolicyErrors == SslPolicyErrors.None)
                return true;

#if NET8_0_OR_GREATER
            var requestMessage = sender as HttpRequestMessage;
            string hostname = requestMessage?.RequestUri?.Host ?? string.Empty;
#else
            var webreq = sender as HttpWebRequest;
            string hostname = webreq?.Address?.Host ?? string.Empty;
#endif

            string fingerprint = CommonCmdletFunctions.FingerprintPrettyString(certificate.GetCertHashString());

            lock (CertificateValidationLock)
            {
                var certPath = CommonCmdletFunctions.GetCertificatesPath(this);
                var certificates = CommonCmdletFunctions.LoadCertificates(certPath);

                if (certificates.TryGetValue(hostname, out var fingerprintOld))
                {
                    if (fingerprintOld == fingerprint)
                        return true;

                    bool ignoreChanged = Force || NoWarnCertificates || (bool)GetVariableValue("NoWarnCertificates", false);
                    if (!ignoreChanged)
                    {
                        var trusted = CommonCmdletFunctions.VerifyInAllStores(new X509Certificate2(certificate));
                        throw new CertificateChangedException(fingerprint, trusted, hostname);
                    }
                }
                else
                {
                    bool ignoreNew = Force || NoWarnNewCertificates || (bool)GetVariableValue("NoWarnNewCertificates", false);
                    if (!ignoreNew)
                    {
                        var trusted = CommonCmdletFunctions.VerifyInAllStores(new X509Certificate2(certificate));
                        throw new CertificateNotFoundException(fingerprint, trusted, hostname);
                    }
                }

                certificates[hostname] = fingerprint;
                CommonCmdletFunctions.SaveCertificates(certPath, certificates);
                return true;
            }
        }
    }
}
