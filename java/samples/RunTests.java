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

import java.io.ByteArrayOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Date;

/**
 * Runs each of the tests except EventMonitor and Https, with plain text debug output, and XML summary of test results.
 */
public class RunTests
{
    private static ILog textLogger, xmlLogger;
    private static String testName;
    private static int succeeded = 0, failed = 0, skipped = 0;
    private static final boolean stopOnFailure = false;

    private static class FileLogger implements ILog
    {
        private FileWriter w;

        public FileLogger(String path)
        {
            try
            {
                w = new FileWriter(path);
            } catch (IOException e)
            {
                System.err.print("Couldn't open " + path + " for log output.");
                e.printStackTrace();
            }
        }

        public void log(String s)
        {
            if (w != null)
            {
                try
                {
                    System.out.print(s);
                    w.write(s);
                    w.flush();
                } catch (IOException e)
                {
                    System.err.print("Couldn't write to log file!");
                    e.printStackTrace();
                }
            }
        }

        public void logln(String s)
        {
            log(s + "\n");
        }
    }

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
        textLogger = new FileLogger("JavaTestOutput.txt");
        xmlLogger = new FileLogger("JavaTestOutput.xml");

        if (args.length != 3 && args.length != 5)
        {
            logln("Expected arguments: <host> <username> <password> [nfs server] [nfs path]");
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

        xml("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
        xml("<results>");
        xml("  <group>");
        xml("    <name>Java</name>");

        logln("RunTests.java: test run started at " + new Date().toString());

        testName = "AddNetwork";
        try
        {
            testStart();
            AddNetwork.RunTest(textLogger, server);
            testSuccess();
        } catch (Exception e)
        {
            testFailure(e);
        }

        testName = "SessionReuse";
        try
        {
            testStart();
            SessionReuse.RunTest(textLogger, server);
            testSuccess();
        } catch (Exception e)
        {
            testFailure(e);
        }

        testName = "AsyncVMCreate";
        try
        {
            testStart();
            AsyncVMCreate.RunTest(textLogger, server);
            testSuccess();
        } catch (Exception e)
        {
            testFailure(e);
        }

        testName = "VdiAndSrOps";
        try
        {
            testStart();
            VdiAndSrOps.RunTest(textLogger, server);
            testSuccess();
        } catch (Exception e)
        {
            testFailure(e);
        }

        testName = "CreateVM";
        try
        {
            testStart();
            CreateVM.RunTest(textLogger, server);
            testSuccess();
        } catch (Exception e)
        {
            testFailure(e);
        }

        testName = "DeprecatedMethod";
        try
        {
            testStart();
            DeprecatedMethod.RunTest(textLogger, server);
            testSuccess();
        } catch (Exception e)
        {
            testFailure(e);
        }

        testName = "GetAllRecordsOfAllTypes";
        try
        {
            testStart();
            GetAllRecordsOfAllTypes.RunTest(textLogger, server);
            testSuccess();
        } catch (Exception e)
        {
            testFailure(e);
        }

        testName = "SharedStorage";
        if (nfsServer != null && nfsPath != null)
        {
            try
            {
                testStart();
                SharedStorage.RunTest(textLogger, server, nfsServer, nfsPath);
                testSuccess();
            } catch (Exception e)
            {
                testFailure(e);
            }
        } else
        {
            logln("nfsServer and nfsPath were not both provided. Skipping SharedStorage test");
            testSkipped();
        }

        testName = "StartAllVMs";
        try
        {
            testStart();
            StartAllVMs.RunTest(textLogger, server);
            testSuccess();
        } catch (Exception e)
        {
            testFailure(e);
        }

        xml("  </group>");
        xml("</results>");

        logf("%d succeeded, %d skipped, %d failed, %d total\n", succeeded, skipped, failed, succeeded + skipped
                + failed);

        logln("RunTests.java: test run finished at " + new Date().toString());
    }

    private static void testStart()
    {
        logln("\n^^^ " + testName + " starting ^^^");
    }

    private static void testSuccess()
    {
        succeeded++;
        logln("^^^ " + testName + " success ^^^\n");
        xmlTest(testName, Result.Pass);
    }

    private static void testSkipped()
    {
        skipped++;
        logln("^^^ " + testName + " skipped ^^^\n");
        xmlTest(testName, Result.Skip);
    }

    private static void testFailure(Exception e)
    {
        failed++;
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        e.printStackTrace(new PrintStream(bytes));
        logln(bytes.toString());
        logln("^^^ " + testName + " failure ^^^\n");
        xmlTest(testName, Result.Fail);

        if (stopOnFailure)
        {
            System.exit(1);
        }
    }

    private static void xml(String s)
    {
        xmlLogger.logln(s);
    }

    private static void logln(String s)
    {
        textLogger.logln(s);
    }

    private static void logf(String s, Object... args)
    {
        textLogger.log(String.format(s, args));
    }

    public enum Result
    {
        Pass, Fail, Skip
    };

    private static void xmlTest(String name, Result result)
    {
        xml("    <test>");
        xml("        <name>" + name + "</name>");
        xml("        <state>" + result.toString() + "</state>");
        xml("    </test>");
    }
}
