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
using System.Net;
using XenAPI;
using XenSdkSample;

namespace GetVmRecords
{
    public class Program
    {
        public static void Main(string[] args)
        {
            using (var outputLogger = new OutputLogger())
            {
                if (args.Length < 3)
                {
                    outputLogger.Log("Required arguments: host-ip username password");
                    return;
                }

                //Trust all certificates. This is a test workaround. DO NOT USE IN PRODUCTION CODE!
                ServicePointManager.ServerCertificateValidationCallback = delegate { return true; };
                ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;

                Session session = null;
                try
                {
                    string hostname = args[0], username = args[1], password = args[2];
                    session = new Session(hostname, 443); //using the default port
                    session.login_with_password(username, password, "", "XenSdkSample");

                    int pass = 0, fail = 0;

                    var testList = new List<TestBase>
                    {
                        new GetVariousRecords(outputLogger, session),
                        new VmPowerStates(outputLogger, session)
                    };

                    using (var resultLogger = new ResultLogger())
                    {
                        resultLogger.Initialise();
                        foreach (var test in testList)
                        {
                            try
                            {
                                test.Run();
                                pass++;
                                resultLogger.LogTest(test.Name, true, "");
                            }
                            catch (Exception e)
                            {
                                fail++;
                                resultLogger.LogTest(test.Name, false, e.Message);
                            }
                        }

                        resultLogger.Finalise(testList.Count, pass, fail);
                    }
                }
                finally
                {
                    if (session != null)
                        session.logout();
                }
            }
        }
    }
}
