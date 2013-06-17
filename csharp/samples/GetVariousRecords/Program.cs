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

namespace GetVariousRecords
{
    /// <summary>
    /// Connects to a XenServer and displays some interesting information about hosts, storage and virtual machines.
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

            // Information about the hosts
            PrintHostInformation(session);

            // Information about physical network interfaces
            PrintPhysicalInterfaces(session);

            // All the storage repositories available
            PrintStorageRepositories(session);

            // Print per VM information
            PrintVMInformation(session);

            // watch it all before it vanishes
            System.Console.WriteLine("Hit any key to exit\n");
            System.Console.ReadKey();
        }

        // In this release a session is established with a host
        // hence the session represents a single host. In future, sessions 
        // could extend to a group of computers
        private static void PrintHostInformation(Session session)
        {
            System.Console.WriteLine("*** Hosts ***\n");

            // Get the list of all hosts associated with this session
            // For this version of the product there will be just the one
            // that we established a session with
            List<XenRef<Host>> hostRefs = Host.get_all(session);

            foreach (XenRef<Host> hostRef in hostRefs)
            {
                // obtain the full host record from the server
                Host host = Host.get_record(session, hostRef);

                // display name and description
                System.Console.WriteLine("Name: {0}", host.name_label);
                System.Console.WriteLine("Hostname: {0}", host.hostname);
                System.Console.WriteLine("Description: {0}", host.name_description);
                System.Console.WriteLine("-");
            }
        }

        /// <summary>
        /// Storage Repositories are an abstraction to represent various types of storage aggregations.
        /// </summary>
        /// <param name="session"></param>
        private static void PrintStorageRepositories(Session session)
        {
            System.Console.WriteLine("\n*** Storage Repositories ***\n");

            // Get all the SRs from this session/host
            List<XenRef<SR>> srRefs = SR.get_all(session);

            // enumerate all the SRs available for storage operations 
            foreach (XenRef<SR> srRef in srRefs)
            {
                SR sr = SR.get_record(session, srRef);

                System.Console.WriteLine("Name: {0}", sr.name_label);
                System.Console.WriteLine("Description: {0}", sr.name_description);
                System.Console.WriteLine("Usage: {0:0.0}GB / {1:0.0}GB", sr.physical_utilisation / 1e9, sr.physical_size / 1e9);
                System.Console.WriteLine("-");
            }
        }

        // Obtain all the VMs available
        private static void PrintVMInformation(Session session)
        {
            System.Console.WriteLine("\n*** Virtual Machines ***\n");

            // Get the list of VMs
            List<XenRef<VM>> vmRefs = VM.get_all(session);

            foreach (XenRef<VM> vmRef in vmRefs)
            {
                // get the entire record for each VM.
                VM vm = VM.get_record(session, vmRef);

                // print out information of interest to the console
                System.Console.WriteLine("{0} name: {1}", vm.is_a_template ? "Template" : "VM", vm.name_label);
                System.Console.WriteLine("Power state: {0}", vm.power_state);
                System.Console.WriteLine("Allowed operations: {0}", GetAllowedOperations(vm));
                System.Console.WriteLine("-");
            }
        }

        /// <summary>
        /// Returns a comma-separated list of the allowed operations for the given VM.
        /// </summary>
        /// <param name="vm"></param>
        private static string GetAllowedOperations(VM vm)
        {
            return string.Join(",", Array.ConvertAll<vm_operations, string>(vm.allowed_operations.ToArray(), delegate(vm_operations op)
            {
                return op.ToString();
            }));
        }

        private static void PrintPhysicalInterfaces(Session session)
        {
            System.Console.WriteLine("\n*** Physical network interfaces: ***\n");

            foreach (PIF pif in PIF.get_all_records(session).Values)
            {
                // resolve the host reference in the pif
                Host host = Host.get_record(session, pif.host);

                System.Console.WriteLine("Host: {0}", host.name_label);
                System.Console.WriteLine("IP: {0}", pif.IP);
                System.Console.WriteLine("MAC address: {0}", pif.MAC);
                System.Console.WriteLine("-");
            }
        }
    }
}
