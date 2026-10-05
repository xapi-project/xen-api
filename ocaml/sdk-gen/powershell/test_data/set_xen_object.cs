// Copyright (c) Cloud Software Group, Inc.

using System;
using System.Collections;
using System.Collections.Generic;
using System.Management.Automation;
using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet(VerbsCommon.Set, "XenVM", SupportsShouldProcess = true)]
    [OutputType(typeof(XenAPI.VM))]
    [OutputType(typeof(void))]
    public class SetXenVM : XenServerCmdlet
    {
        #region Cmdlet Parameters

        [Parameter]
        public SwitchParameter PassThru { get; set; }

        [Parameter(ParameterSetName = "Ref", Mandatory = true, ValueFromPipelineByPropertyName = true, Position = 0)]
        public XenRef<XenAPI.VM> Ref { get; set; }

        [Parameter]
        public string NameLabel { get; set; }

        #endregion

        #region Cmdlet Methods

        protected override void ProcessRecord()
        {
            GetSession();

            string vm = ParseVM();

            ProcessRecordNameLabel(vm);

            if (PassThru)
                WriteObject(obj, true);

            UpdateSessions();
        }

        #endregion

        #region Private Methods

        private string ParseVM()
        {
            return Ref.opaque_ref;
        }

        private void ProcessRecordNameLabel(string vm)
        {
            RunApiCall(() => XenAPI.VM.set_name_label(session, vm, NameLabel));
        }

        #endregion
    }
}
