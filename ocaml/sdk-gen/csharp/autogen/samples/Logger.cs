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
using System.IO;


namespace XenSdkSample
{
    class Logger : IDisposable
    {
        protected readonly StreamWriter _writer;

        protected Logger(string file)
        {
            _writer = new StreamWriter(file);
        }
        
        public void Dispose()
        {
            if (_writer != null)
            {
                _writer.Flush();
                _writer.Close();
                _writer.Dispose();
            }
        }
    }

    class OutputLogger : Logger
    {
        public OutputLogger()
            : base("TestConsoleOutput.txt")
        {
        }

        public void Log(string format, params object[] args)
        {
            Console.WriteLine(format, args);
            _writer.WriteLine(format, args);
        }

        public void WriteLine()
        {
            Log("");
        }

        public void WriteHRule()
        {
            Log("---------------------------------------");
        }
    }

    class ResultLogger : Logger
    {
        public ResultLogger()
            : base("TestResults.xml")
        {
        }

        public void LogTest(string testName, bool pass, string message)
        {
            _writer.Write("<test><name>{0}</name><pass>{1}</pass><message>{2}</message></test>", testName, pass, message);
        }

        public void Initialise()
        {
            _writer.Write("<?xml version=\"1.0\" encoding=\"utf-8\"?><results><tests>");
        }

        public void Finalise(int total, int pass, int fail)
        {
            _writer.Write("</tests><total>{0}</total><pass>{1}</pass><fail>{2}</fail></results>", total, pass, fail);
        }
    }
}
