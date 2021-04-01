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
        private readonly string _apiVersionString = Helper.APIVersionString(API_Version.LATEST);
        private string _originator = "XenServerPSModule/" + Helper.APIVersionString(API_Version.LATEST);

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
        public string Originator { get { return _originator; } set { _originator = value; } }

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
                    connUser = Creds.UserName.StartsWith("\\")
                                   ? Creds.GetNetworkCredential().UserName
                                   : Creds.UserName;

                    IntPtr ptrPassword = Marshal.SecureStringToBSTR(Creds.Password);
                    connPassword = Marshal.PtrToStringBSTR(ptrPassword);
                    Marshal.FreeBSTR(ptrPassword);
                }
            }

            ServicePointManager.ServerCertificateValidationCallback = new RemoteCertificateValidationCallback(ValidateServerCertificate);
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
                        new Exception("The number of opaque references provided should be the same as the number of xenservers."),
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
                        session.login_with_password(connUser, connPassword, _apiVersionString, Originator);
                    }
                    catch (Failure f)
                    {
                        if (f.ErrorDescription != null && f.ErrorDescription.Count > 1 && f.ErrorDescription[0] == "HOST_IS_SLAVE")
                        {
                            ThrowTerminatingError(new ErrorRecord(f, "", ErrorCategory.InvalidArgument, Url[i])
                            {
                                ErrorDetails = new ErrorDetails(string.Format("The host you are trying to connect to is a slave. To make regular API calls, please connect to the master host (IP address: {0}).",
                                    f.ErrorDescription[1]))
                            });
                        }
                        else
                        {
                            throw;
                        }
                    }
                }
                else
                {
                    session = new Session(Url[i], OpaqueRef[i]);
                }

                session.Tag = Creds;
                session.opaque_ref = session.opaque_ref;
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

        #region Messages

        const string CERT_HAS_CHANGED_CAPTION = "Security Certificate Changed";

        const string CERT_CHANGED = "The certificate fingerprint of the server you have connected to is:\n{0}\nBut was expected to be:\n{1}\n{2}\nDo you wish to continue?";

        const string CERT_FOUND_CAPTION = "New Security Certificate";

        const string CERT_FOUND = "The certificate fingerprint of the server you have connected to is :\n{0}\n{1}\nDo you wish to continue?";

        const string CERT_TRUSTED = "The certificate on this server is trusted. It is recommended you re-issue this server's certificate.";

        const string CERT_NOT_TRUSTED = "The certificate on this server is not trusted.";

        #endregion

        private readonly object certificateValidationLock = new object();

        private bool ValidateServerCertificate(
            object sender,
            X509Certificate certificate,
            X509Chain chain,
            SslPolicyErrors sslPolicyErrors)
        {
            if (sslPolicyErrors == SslPolicyErrors.None)
                return true;

            lock (certificateValidationLock)
            {
                bool ignoreChanged = NoWarnCertificates || (bool)GetVariableValue("NoWarnCertificates", false);
                bool ignoreNew = ignoreChanged || NoWarnNewCertificates || (bool)GetVariableValue("NoWarnNewCertificates", false);

                HttpWebRequest webreq = (HttpWebRequest)sender;
                string hostname = webreq.Address.Host;
                string fingerprint = CommonCmdletFunctions.FingerprintPrettyString(certificate.GetCertHashString());

                string trusted = VerifyInAllStores(new X509Certificate2(certificate))
                                     ? CERT_TRUSTED : CERT_NOT_TRUSTED;

                var certificates = CommonCmdletFunctions.LoadCertificates();
                bool ok;

                if (certificates.ContainsKey(hostname))
                {
                    string fingerprint_old = certificates[hostname];
                    if (fingerprint_old == fingerprint)
                        return true;

                    ok = Force || ignoreChanged || ShouldContinue(string.Format(CERT_CHANGED, fingerprint, fingerprint_old, trusted), CERT_HAS_CHANGED_CAPTION);
                }
                else
                {
                    ok = Force || ignoreNew || ShouldContinue(string.Format(CERT_FOUND, fingerprint, trusted), CERT_FOUND_CAPTION);
                }

                if (ok)
                {
                    certificates[hostname] = fingerprint;
                    CommonCmdletFunctions.SaveCertificates(certificates);
                }
                return ok;
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
}
