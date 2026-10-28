// Copyright (c) Cloud Software Group, Inc.

using System;
using System.Collections;
using System.Collections.Generic;
using System.Management.Automation;
using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet(VerbsCommon.Get, "XenBlob", DefaultParameterSetName = "Ref", SupportsShouldProcess = false)]
    [OutputType(typeof(XenAPI.Blob[]))]
    public class GetXenBlobCommand : XenServerCmdlet
    {
        #region Cmdlet Parameters

        [Parameter(ParameterSetName = "Ref", Mandatory = true, ValueFromPipelineByPropertyName = true, Position = 0)]
        public XenRef<XenAPI.VM> Ref { get; set; }

        #endregion

        #region Cmdlet Methods

        protected override void ProcessRecord()
        {
            GetSession();

            var records = XenAPI.Blob.get_all_records(session);

            foreach (var record in records)
                record.Value.opaque_ref = record.Key;

            var results = new List<XenAPI.Blob>();

            if (Ref != null)
            {
                foreach (var record in records)
                    if (Ref.opaque_ref == record.Key.opaque_ref)
                    {
                        results.Add(record.Value);
                        break;
                    }
            }
            else
            {
                results.AddRange(records.Values);
            }

            WriteObject(results, true);

            UpdateSessions();
        }

        #endregion
    }
}
