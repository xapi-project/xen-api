using System;
using System.Collections.Generic;
using System.Linq;
using XenAPI;


namespace GetVmRecords
{
    class VmPowerStates
    {
        public static void Run(Session session)
        {
            System.Console.WriteLine("*** Powercycle a VM ***");

            // Choose a VM at random which is not a template or control domain and which is currently switched off.

            var vmRecords = VM.get_all_records(session);

            var vmRef = (from KeyValuePair<XenRef<VM>, VM> kvp in vmRecords
                let theVm = kvp.Value
                where !theVm.is_a_template && !theVm.is_control_domain && theVm.power_state == vm_power_state.Halted
                select kvp.Key).FirstOrDefault();

            if (vmRef == null)
            {
                System.Console.WriteLine("Cannot find a halted VM. Please create one");
                return;
            }

            // clone the vm
            VM vm = VM.get_record(session, vmRef);
            System.Console.WriteLine("Cloning VM '{0}'...", vm.name_label);
            string cloneVm = VM.clone(session, vmRef, string.Format("Cloned VM (from '{0}')", vm.name_label));
            System.Console.WriteLine("Cloned VM; new VM is {0}", cloneVm);

            // set its description
            VM.set_name_description(session, cloneVm, "Another cloned VM");
            System.Console.WriteLine("VM name: {0} Description: {1} Power State: {2}",
                VM.get_name_label(session, cloneVm),
                VM.get_name_description(session, cloneVm),
                VM.get_power_state(session, cloneVm));

            // start the clone in a paused state
            System.Console.WriteLine("Starting VM paused...");
            VM.start(session, cloneVm, true, true);
            System.Console.WriteLine("VM Power State: {0}", VM.get_power_state(session, cloneVm));

            // unpause it
            System.Console.WriteLine("Unpausing VM...");
            VM.unpause(session, cloneVm);
            System.Console.WriteLine("VM Power State: {0}", VM.get_power_state(session, cloneVm));

            // now suspend it
            System.Console.WriteLine("Suspending VM...");
            VM.suspend(session, cloneVm);
            System.Console.WriteLine("VM Power State: {0}", VM.get_power_state(session, cloneVm));

            // and then resume
            System.Console.WriteLine("Resuming VM...");
            VM.resume(session, cloneVm, false, true);
            System.Console.WriteLine("VM Power State: {0}", VM.get_power_state(session, cloneVm));

            // and then shutdown
            System.Console.WriteLine("Forcing shutdown VM...");
            VM.hard_shutdown(session, cloneVm);
            System.Console.WriteLine("VM Power State: {0}", VM.get_power_state(session, cloneVm));

            // now destroy it
            System.Console.WriteLine("Destroying VM...");
            VM.destroy(session, cloneVm);
            System.Console.WriteLine("VM destroyed.");

        }
    }
}
