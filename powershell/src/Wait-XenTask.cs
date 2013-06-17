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
using System.Threading;
using System.Windows.Forms;

using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet("Wait", "XenTask")]
    public class WaitXenServerTaskCommand : XenServerCmdlet
    {
        public WaitXenServerTaskCommand()
        {
            Min = 0;
            Max = 100;
        }
        
        #region Cmdlet Parameters

        [Parameter]
        public SwitchParameter PassThru { get; set; }

        [Parameter(ParameterSetName = "XenObject", Mandatory = true, ValueFromPipeline = true, Position = 0)]
        public XenAPI.Task Task { get; set; }

        [Parameter(ParameterSetName = "Ref", Mandatory = true, ValueFromPipelineByPropertyName = true, Position = 0)]
        [Alias("opaque_ref")]
        public XenRef<XenAPI.Task> Ref { get; set; }

        [Parameter(ParameterSetName = "Uuid", Mandatory = true, ValueFromPipelineByPropertyName = true, Position = 0)]
        public Guid Uuid { get; set; }

        [Parameter(ParameterSetName = "Name", Mandatory = true, ValueFromPipelineByPropertyName = true, Position = 0)]
        [Alias("name_label")]
        public string Name { get; set; }

        [Parameter]
        public SwitchParameter ShowProgress { get; set; }

        [Parameter]
        public ProgressBar Progressbar { get; set; }

        [Parameter]
        public int Min { get; set; }

        [Parameter]
        public int Max { get; set; }

        #endregion

        #region Cmdlet Methods

        protected override void ProcessRecord()
        {
            GetSession();
            
            string task = ParseTask();

            RunApiCall(delegate()
            {
                string name = XenAPI.Task.get_name_label(session, task);
                string uuid = XenAPI.Task.get_uuid(session, task);
                DateTime created = XenAPI.Task.get_created(session, task);
                ProgressRecord prog = new ProgressRecord(Math.Abs(uuid.GetHashCode()), name, "0% complete");
                prog.RecordType = ProgressRecordType.Processing;

                while (true)
                {
                    double progress = XenAPI.Task.get_progress(session, task);
                    prog.PercentComplete = Min + (int)(progress * (Max - Min));
                    prog.StatusDescription = string.Format("{0}% complete", prog.PercentComplete);
                    TimeSpan elapsed = TimeSpan.FromTicks(DateTime.UtcNow.Ticks - created.Ticks);
                    prog.CurrentOperation = string.Format("{0}:{1}:{2}",
                        elapsed.Hours.ToString("00"),
                        elapsed.Minutes.ToString("00"),
                        elapsed.Seconds.ToString("00"));

                    if (XenAPI.Task.get_status(session, task) != XenAPI.task_status_type.pending)
                    {
                        if (prog.PercentComplete == 100)
                            prog.RecordType = ProgressRecordType.Completed;

                        if (Progressbar != null)
                            Progressbar.Value = (int)(Progressbar.Minimum + (prog.PercentComplete * ((Progressbar.Maximum - Progressbar.Minimum) / 100d)));
                        else if (ShowProgress)
                            WriteProgress(prog);

                        break;
                    }
                    else
                    {
                        if (Progressbar != null)
                            Progressbar.Value = (int)(Progressbar.Minimum + (prog.PercentComplete * ((Progressbar.Maximum - Progressbar.Minimum) / 100d)));
                        else if (ShowProgress)
                            WriteProgress(prog);
                    }
                    Thread.Sleep(500);
                }

                if (XenAPI.Task.get_status(session, task) == XenAPI.task_status_type.failure)
                {
                    throw new Failure(XenAPI.Task.get_error_info(session, task));
                }
                
                if (XenAPI.Task.get_status(session, task) == XenAPI.task_status_type.cancelled)
                {
                    throw new Exception("User Cancelled");
                }

                if (PassThru)
                    WriteObject(XenAPI.Task.get_result(session, task).Replace("<value>", "").Replace("</value>", ""), true);
            });
            
            UpdateSessions();
        }

        #endregion

        #region Private Methods

        private string ParseTask()
        {
            string task = null;

            if (Task != null)
                task = (new XenRef<XenAPI.Task>(Task)).opaque_ref;
            else if (Uuid != Guid.Empty)
            {
                XenRef<XenAPI.Task> xenRef = XenAPI.Task.get_by_uuid(session, Uuid.ToString());
                if (xenRef != null)
                    task = xenRef.opaque_ref;
            }
            else if (Name != null)
            {
                List<XenRef<XenAPI.Task>> xenRefs = XenAPI.Task.get_by_name_label(session, Name);
                if (xenRefs.Count == 1)
                    task = xenRefs[0].opaque_ref;
                else if (xenRefs.Count > 1)
                    ThrowTerminatingError(new ErrorRecord(
                        new ArgumentException(string.Format("More than one XenAPI.Task with name label {0} exist", Name)),
                        string.Empty,
                        ErrorCategory.InvalidArgument,
                        Name));
            }
            else if (Ref != null)
                task = Ref.opaque_ref;
            else
            {
                ThrowTerminatingError(new ErrorRecord(
                    new ArgumentException("At least one of the parameters 'Task', 'Ref', 'Uuid' must be set"),
                    string.Empty,
                    ErrorCategory.InvalidArgument,
                    Task));
            }

            return task;
        }

        #endregion
    }
}
