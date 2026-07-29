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
using System.Management.Automation;
using System.Net;
using System.Net.Security;
using System.Security.Cryptography.X509Certificates;
using XenAPI;

namespace Citrix.XenServer.Commands
{
    public class XenServerHttpCmdlet : XenServerCmdlet
    {
        private bool _hitonce;
        private static readonly object CertificateValidationLock = new object();

        protected XenServerHttpCmdlet()
        {
            CertificateValidationCallback = ValidateServerCertificate;
        }

        #region Cmdlet Parameters

        [Parameter]
        public HTTP.FuncBool CancellingDelegate { get; set; }

        [Parameter]
        public int TimeoutMs { get; set; }

        [Parameter(Mandatory = true)]
        public string XenHost { get; set; }

        [Parameter]
        public IWebProxy Proxy { get; set; }

        [Parameter]
        public RemoteCertificateValidationCallback CertificateValidationCallback { get; set; }

        [Parameter]
        public SwitchParameter NoWarnNewCertificates { get; set; }

        [Parameter]
        public SwitchParameter NoWarnCertificates { get; set; }

        [Parameter(Mandatory = true)]
        public string Path { get; set; }

        [Parameter]
        public string TaskRef { get; set; }

        #endregion

        private bool ValidateServerCertificate(object sender, X509Certificate certificate, X509Chain chain, SslPolicyErrors sslPolicyErrors)
        {
            try
            {
                if (sslPolicyErrors == SslPolicyErrors.None)
                    return true;

                string fingerprintToCheck = CommonCmdletFunctions.FingerprintPrettyString(certificate.GetCertHashString());

                lock (CertificateValidationLock)
                {
                    var certPath = CommonCmdletFunctions.GetCertificatesPath(this);
                    var certificates = CommonCmdletFunctions.LoadCertificates(certPath);

                    if (certificates.TryGetValue(XenHost, out var hostFingerprint))
                    {
                        if (fingerprintToCheck == hostFingerprint)
                            return true;

                        bool ignoreChanged = NoWarnCertificates || (bool)GetVariableValue("NoWarnCertificates", false);
                        if (!ignoreChanged)
                        {
                            if (_hitonce)
                                throw new Exception($"{XenHost} is not the correct server for this operation. Please specify a different server in the pool.");

                            var trusted = CommonCmdletFunctions.VerifyInAllStores(new X509Certificate2(certificate));
                            throw new CertificateChangedException(fingerprintToCheck, trusted, XenHost);
                        }
                    }
                    else
                    {
                        bool ignoreNew = NoWarnNewCertificates || (bool)GetVariableValue("NoWarnNewCertificates", false);
                        if (!ignoreNew)
                        {
                            var trusted = CommonCmdletFunctions.VerifyInAllStores(new X509Certificate2(certificate));
                            throw new CertificateNotFoundException(fingerprintToCheck, trusted, XenHost);
                        }
                    }

                    certificates[XenHost] = fingerprintToCheck;
                    CommonCmdletFunctions.SaveCertificates(certPath, certificates);
                    return true;
                }
            }
            finally
            {
                _hitonce = true;
            }
        }
    }
}
