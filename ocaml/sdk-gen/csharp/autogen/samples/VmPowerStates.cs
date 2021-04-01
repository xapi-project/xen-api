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
using System.Linq;
using System.Threading;
using XenAPI;


namespace XenSdkSample
{
    class VmPowerStates : TestBase
    {
        public VmPowerStates(OutputLogger logger, Session session)
            : base(logger, session)
        {
        }

        public override string Name
        {
            get { return "VmPowerStates"; }
        }

        public override string Description
        {
            get { return "Powercycle a VM"; }
        }

        protected override void TestCore()
        {
            // Choose a linux VM at random which is not a template or control domain and which is currently switched off.

            var vmRecords = VM.get_all_records(_session);

            var vmRef = (from KeyValuePair<XenRef<VM>, VM> kvp in vmRecords
                let theVm = kvp.Value
                where !theVm.is_a_template && !theVm.is_control_domain
                      && !theVm.name_label.ToLower().Contains("windows")
                      && theVm.power_state == vm_power_state.Halted
                select kvp.Key).FirstOrDefault();

            if (vmRef == null)
            {
                var msg = "Cannot find a halted linux VM. Please create one.";
                _logger.Log(msg);
                throw new Exception(msg);
            }

            //to avoid playing with existing data, clone the VM and powercycle its clone

            VM vm = VM.get_record(_session, vmRef);

            _logger.Log("Cloning VM '{0}'...", vm.name_label);
            string cloneVmRef = VM.clone(_session, vmRef, string.Format("Cloned VM (from '{0}')", vm.name_label));
            _logger.Log("Cloned VM; new VM's ref is {0}", cloneVmRef);

            VM.set_name_description(_session, cloneVmRef, "Another cloned VM");
            VM cloneVm = VM.get_record(_session, cloneVmRef);
            _logger.Log("Clone VM's Name: {0}, Description: {1}, Power State: {2}", cloneVm.name_label,
                cloneVm.name_description, cloneVm.power_state);

            _logger.Log("Starting VM in paused state...");
            VM.start(_session, cloneVmRef, true, true);
            _logger.Log("VM Power State: {0}", VM.get_power_state(_session, cloneVmRef));

            _logger.Log("Unpausing VM...");
            VM.unpause(_session, cloneVmRef);
            _logger.Log("VM Power State: {0}", VM.get_power_state(_session, cloneVmRef));

            // here we need to delay for a bit until the suspend feauture is written
            // in the guest metrics; this check should be enough for most guests;
            // let's try a certain number of times with sleeps of a few seconds inbetween
            int max = 20;
            int delay = 10;
            for (int i = 0; i < max; i++)
            {
                cloneVm = VM.get_record(_session, cloneVmRef);
                var metrics = VM_guest_metrics.get_record(_session, cloneVm.guest_metrics);
                if (metrics.other.ContainsKey("feature-suspend") && metrics.other["feature-suspend"] == "1")
                    break;
                _logger.Log("Checked for feature-suspend count {0} out of {1}; will re-try in {2}sec.", i + 1, max, delay);
                Thread.Sleep(delay * 1000);
            }

            _logger.Log("Suspending VM...");
            VM.suspend(_session, cloneVmRef);
            _logger.Log("VM Power State: {0}", VM.get_power_state(_session, cloneVmRef));

            _logger.Log("Resuming VM...");
            VM.resume(_session, cloneVmRef, false, true);
            _logger.Log("VM Power State: {0}", VM.get_power_state(_session, cloneVmRef));

            _logger.Log("Forcing shutdown VM...");
            VM.hard_shutdown(_session, cloneVmRef);
            _logger.Log("VM Power State: {0}", VM.get_power_state(_session, cloneVmRef));

            cloneVm = VM.get_record(_session, cloneVmRef);
            var vdis = (from vbd in cloneVm.VBDs
                let vdi = VBD.get_VDI(_session, vbd)
                where vdi.opaque_ref != "OpaqueRef:NULL"
                select vdi).ToList();

            _logger.Log("Destroying VM...");
            VM.destroy(_session, cloneVmRef);

            _logger.Log("Destroying VM's disks...");
            foreach (var vdi in vdis)
                VDI.destroy(_session, vdi);

            _logger.Log("VM destroyed.");
        }
    }
}
