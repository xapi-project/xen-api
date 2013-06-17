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
using System.Text;
using XenAPI; // the API namespace

namespace VmPowerStates
{
    /// <summary>
    /// Connects to a XenServer, finds a VM, and takes it through the various power states.
    /// Requires a shut-down VM to be already installed.
    /// </summary>
    public class Program
    {
        public static void Main(string[] args)
        {
            if (args.Length < 3)
            {
                System.Console.WriteLine("Required arguments: host-ip username password\n");
                return;
            }
            
            // Host information necessary to get started
            string hostname = args[0];
            int port = 80; // default
            string username = args[1];
            string password = args[2];

            // Establish a session
            Session session = new Session(hostname, port);

            // Authenticate with username and password. The third parameter tells the server which API version we support.
            session.login_with_password(username, password, API_Version.API_1_3);

            // Choose a VM at random which is not a template or control domain and which is currently switched off.
            XenRef<VM> chosenRef = null;
            foreach (KeyValuePair<XenRef<VM>, VM> kvp in VM.get_all_records(session))
            {
                VM vm = kvp.Value;
                if (!vm.is_a_template && !vm.is_control_domain && vm.power_state == vm_power_state.Halted)
                {
                    chosenRef = kvp.Key;
                    break;
                }
            }

            string cloneVM = null;
            if (chosenRef != null)
            {
                // clone the vm
                VM chosenVM = VM.get_record(session, chosenRef);
                System.Console.WriteLine("Cloning VM '{0}'...", chosenVM.name_label);
                cloneVM = VM.clone(session, chosenRef, string.Format("Cloned VM (from '{0}')", chosenVM.name_label));
                System.Console.WriteLine("Cloned VM; new VM is {0}", cloneVM);

                // set its description
                VM.set_name_description(session, cloneVM, "Another cloned VM");
                System.Console.WriteLine("VM name: {0} Description: {1} Power State: {2}",
                                            VM.get_name_label(session, cloneVM),
                                            VM.get_name_description(session, cloneVM),
                                            VM.get_power_state(session, cloneVM));

                // start the clone in a paused state
                System.Console.WriteLine("Starting VM paused...");
                VM.start(session, cloneVM, true, true);
                System.Console.WriteLine("VM Power State: {0}", VM.get_power_state(session, cloneVM));

                // unpause it
                System.Console.WriteLine("Unpausing VM...");
                VM.unpause(session, cloneVM);
                System.Console.WriteLine("VM Power State: {0}", VM.get_power_state(session, cloneVM));

                // now suspend it
                System.Console.WriteLine("Suspending VM...");
                VM.suspend(session, cloneVM);
                System.Console.WriteLine("VM Power State: {0}", VM.get_power_state(session, cloneVM));

                // and then resume
                System.Console.WriteLine("Resuming VM...");
                VM.resume(session, cloneVM, false, true);
                System.Console.WriteLine("VM Power State: {0}", VM.get_power_state(session, cloneVM));

                // and then shutdown
                System.Console.WriteLine("Forcing shutdown VM...");
                VM.hard_shutdown(session, cloneVM);
                System.Console.WriteLine("VM Power State: {0}", VM.get_power_state(session, cloneVM));

                // now destroy it
                System.Console.WriteLine("Destroying VM...");
                VM.destroy(session, cloneVM);
                System.Console.WriteLine("VM destroyed.");
            }
            else
            {
                System.Console.WriteLine("No suitable VMs found (please install one)");
            }

            // watch it all before it vanishes
            System.Console.WriteLine("\nHit any key to exit\n");
            System.Console.ReadKey();
        }
    }
}
