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
using System.Collections;
using System.Collections.Generic;
using System.Management.Automation;
using System.Text;

using XenAPI;


namespace Citrix.XenServer.Commands
{
    public class XenServerCmdlet : PSCmdlet, IDynamicParameters
    {
        protected XenRef<Task> taskRef;
        protected Session session;
        protected Dictionary<string, Session> sessions;
        protected delegate void XenApiCall();

        #region Cmdlet Parameters

        [Parameter]
        public SwitchParameter BestEffort { get; set; }

        [Parameter]
        public string SessionOpaqueRef { get; set; }

        #endregion

        #region Cmdlet Methods

        protected void GetSession()
        {
            sessions = CommonCmdletFunctions.GetAllSessions(this);

            if (sessions.Count == 0)
            {
                ThrowTerminatingError(new ErrorRecord(
                                          new Exception("Could not find open sessions to any XenServers."),
                                          "",
                                          ErrorCategory.InvalidArgument, null));
            }

            session = null;

            if (string.IsNullOrEmpty(SessionOpaqueRef))
            {
                if (sessions.Count == 1)
                {
                    foreach (KeyValuePair<string, Session> kvp in sessions)
                        session = kvp.Value;
                }
                else
                {
                    Session defaultSession = CommonCmdletFunctions.GetDefaultXenSession(this);

                    if (sessions.ContainsValue(defaultSession))
                        session = defaultSession;

                    if (session == null)
                        ThrowTerminatingError(new ErrorRecord(
                                                  new Exception("A default XenServer session has not beeen set."),
                                                  "",
                                                  ErrorCategory.InvalidArgument, null));
                }
            }
            else
            {
                if (sessions.ContainsKey(SessionOpaqueRef))
                    session = sessions[SessionOpaqueRef];

                if (session == null)
                    ThrowTerminatingError(new ErrorRecord(
                                              new Exception("Could not locate the specified session in the open XenServer sessions."),
                                              "",
                                              ErrorCategory.InvalidArgument, SessionOpaqueRef));
            }
        }

        protected void UpdateSessions()
        {
            //save session dictionary back in the session variable (in case it was modified)
            CommonCmdletFunctions.SetAllSessions(this, sessions);
        }

        protected void RunApiCall(XenApiCall call)
        {
            try
            {
                call.Invoke();
            }
            catch (Exception e)
            {
                // if you want to trap errors either set command-line switch "-BestEffort"
                // or session-state variable "$BestEffort" to "$true"

                bool bestEffort = (bool)GetVariableValue("BestEffort", false) || BestEffort;
                if (!bestEffort)
                    throw;

                // catch exception and write it to the terminal then return
                // don't throw it because this will break piping a list into the cmd (won't run rest of list)

                ThrowTerminatingError(new ErrorRecord(e, string.Empty, ErrorCategory.InvalidOperation, null));
            }
        }

        #endregion

        #region Implementation of IDynamicParameters

        protected IXenServerDynamicParameter _context;

        protected virtual bool GenerateAsyncParam
        {
            get { return false; }
        }

        public virtual object GetDynamicParameters()
        {
            if (GenerateAsyncParam)
            {
                _context = new XenServerCmdletDynamicParameters();
                return _context;
            }
            return null;
        }

        #endregion
    }

    public class XenServerCmdletDynamicParameters : IXenServerDynamicParameter
    {
        [Parameter]
        public SwitchParameter Async { get; set; }
    }

    public interface IXenServerDynamicParameter
    { }
}
