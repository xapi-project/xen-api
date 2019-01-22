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

import java.util.*;

/**
 * Runs each of the tests except EventMonitor and Https, with plain text debug output, and XML summary of test results.
 */
public class RunTests
{
    private static final boolean stopOnFailure = false;

    /**
     * Expects the first three parameters to be server {address, username, password}.
     * 
     * The fourth and fifth parameters are optional and should be respectively the address of an NFS filer, and the path
     * on that filer to use for creating a new SR.
     * 
     * e.g.
     * 
     * java RunTests myhost root mypassword nfsserver /nfsshare/sr/path
     */
    public static void main(String[] args)
    {
        FileLogger textLogger = new FileLoggerText("JavaTestOutput.txt");
        FileLogger xmlLogger = new FileLoggerXml("JavaTestOutput.xml");

        if (args.length != 3 && args.length != 5)
        {
            textLogger.log("Expected arguments: <host> <username> <password> [nfs server] [nfs path]");
            return;
        }

        TargetServer server = new TargetServer(args[0], args[1], args[2]);

        String nfsServer = null;
        String nfsPath = null;
        if (args.length == 5)
        {
            nfsServer = args[3];
            nfsPath = args[4];
        }

        xmlLogger.log("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
                "<results>\n  <group>\n    <name>Java</name>");

        textLogger.logf("RunTests.java: test run started at %s", new Date().toString());

        List<TestBase> tests = new ArrayList<TestBase>();
        tests.add(new EventMonitor());
        tests.add(new AddNetwork());
        tests.add(new SessionReuse());
        tests.add(new AsyncVMCreate());
        tests.add(new VdiAndSrOps());
        tests.add(new CreateVM());
        tests.add(new DeprecatedMethod());
        tests.add(new GetAllRecordsOfAllTypes());
        tests.add(new Https());
        tests.add(new SharedStorage(nfsServer, nfsPath));
        tests.add(new StartAllVMs());

        int succeeded = 0, failed = 0, skipped = 0;

        for (TestBase test : tests) {
            try {
                textLogger.logTestStart(test);
                test.RunTest(textLogger, server);

                succeeded++;
                textLogger.logTestResult(test, Result.Pass);
                xmlLogger.logTestResult(test, Result.Pass);
            }
            catch (TestBase.SkippingException e) {
                skipped++;
                textLogger.logTestResult(test, Result.Skip);
                xmlLogger.logTestResult(test, Result.Skip);
            }
            catch (Exception e) {
                failed++;
                textLogger.logException(e);
                textLogger.logTestResult(test, Result.Fail);
                xmlLogger.logTestResult(test, Result.Fail);

                if (stopOnFailure)
                    System.exit(1);
            }
        }

        xmlLogger.log("  </group>\n</results>");

        textLogger.logf("%d succeeded, %d skipped, %d failed, %d total",
                succeeded, skipped, failed, succeeded + skipped + failed);

        textLogger.logf("RunTests.java: test run finished at %s", new Date().toString());
    }


    public enum Result
    {
        Pass, Fail, Skip;

        @Override
        public String toString() {
            switch (this){

                case Pass:
                    return "Passed";
                case Fail:
                    return "Failed";
                case Skip:
                    return "Skipped";
                default:
                    return "Unknown";
            }
        }
    }
}
