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
using System.Text;

using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet("Get", "XenSession", DefaultParameterSetName = "Ref")]
    [OutputType(typeof(XenAPI.Session[]))]
    public class GetXenSessionCommand : PSCmdlet
    {
        public GetXenSessionCommand()
        {
            Port = 443;
        }

        #region Cmdlet Parameters

        [Parameter(ParameterSetName = "Ref", ValueFromPipelineByPropertyName = true)]
        [Alias("opaque_ref")]
        public XenRef<XenAPI.Session> Ref { get; set; }

        [Parameter(ParameterSetName = "Url", Mandatory = true)]
        public string Url { get; set; }

        [Parameter(ParameterSetName = "ServerPort", Mandatory = true)]
        [Alias("svr")]
        public string Server { get; set; }

        [Parameter(ParameterSetName = "ServerPort")]
        public int Port { get; set; }

        [Parameter(ParameterSetName = "UserName", Mandatory = true)]
        public string UserName { get; set; }

        #endregion

        #region Cmdlet Methods

        protected override void ProcessRecord()
        {
            Dictionary<string, Session> sessions = CommonCmdletFunctions.GetAllSessions(this);

            if (string.IsNullOrEmpty(Url) && !string.IsNullOrEmpty(Server))
                Url = CommonCmdletFunctions.GetUrl(Server, Port);

            if (!string.IsNullOrEmpty(Ref))
            {
                if (sessions.ContainsKey(Ref.opaque_ref))
                    WriteObject(sessions[Ref.opaque_ref]);
            }
            else if (!string.IsNullOrEmpty(Url))
            {
                List<Session> results = new List<Session>();
                foreach (KeyValuePair<string, Session> kvp in sessions)
                {
                    if (kvp.Value.Url == Url)
                        results.Add(kvp.Value);
                }
                WriteObject(results, true);
            }
            else if (!string.IsNullOrEmpty(UserName))
            {
                List<Session> results = new List<Session>();
                foreach (KeyValuePair<string, Session> kvp in sessions)
                {
                    PSCredential cred = kvp.Value.Tag as PSCredential;
                    if (cred != null && cred.UserName == UserName)
                        results.Add(kvp.Value);
                }
                WriteObject(results, true);
            }
            else
            {
                WriteObject(sessions.Values, true);
            }
        }
        #endregion
    }
}

