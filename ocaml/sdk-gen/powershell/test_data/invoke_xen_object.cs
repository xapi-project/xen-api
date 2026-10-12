// Copyright (c) Cloud Software Group, Inc.

using System;
using System.Collections;
using System.Collections.Generic;
using System.Management.Automation;
using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet(VerbsLifecycle.Invoke, "XenVM", SupportsShouldProcess = true)]
    public class InvokeXenVM : XenServerCmdlet
    {
        #region Cmdlet Parameters

        [Parameter]
        public SwitchParameter PassThru { get; set; }

        [Parameter(ParameterSetName = "Ref", Mandatory = true, ValueFromPipelineByPropertyName = true, Position = 0)]
        public XenRef<XenAPI.VM> Ref { get; set; }

        [Parameter(Mandatory = true)]
        public XenVMAction XenAction { get; set; }

        #endregion

        public override object GetDynamicParameters()
        {
            switch (XenAction)
            {
                case XenVMAction.Clone:
                    _context = new XenVMActionCloneDynamicParameters();
                    return _context;
                default:
                    return null;
            }
        }

        #region Cmdlet Methods

        protected override void ProcessRecord()
        {
            GetSession();

            string vm = ParseVM();

            switch (XenAction)
            {
                case XenVMAction.Clone:
                    ProcessRecordClone(vm);
                    break;

            }

            UpdateSessions();
        }

        #endregion

        #region Private Methods

        private string ParseVM()
        {
            return Ref.opaque_ref;
        }

        private void ProcessRecordClone(string vm)
        {
            RunApiCall(() => XenAPI.VM.clone(session, vm, NewName));
        }

        #endregion
    }

    public enum XenVMAction
    {
        Clone,
        Destroy
    }

    public class XenVMActionCloneDynamicParameters : IXenServerDynamicParameter
    {
        [Parameter]
        public string NewName { get; set; }
    }

}
