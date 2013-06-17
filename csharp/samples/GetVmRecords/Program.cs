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
using XenAPI;

namespace GetVmRecords
{
    /// <summary>
    /// Connects to a XenServer and downloads all the VM records.
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

            List<XenRef<VM>> vmRefs = VM.get_all(session);
            foreach (XenRef<VM> vmRef in vmRefs)
            {
                VM vm = VM.get_record(session, vmRef);
                System.Console.WriteLine("Name: {0}\nvCPUs: {1}\nDescription: {2}\n-", vm.name_label, vm.VCPUs_at_startup, vm.name_description);
            }
        }
    }
}
