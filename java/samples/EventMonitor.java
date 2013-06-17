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

import java.util.HashSet;
import java.util.Set;

import com.xensource.xenapi.Event;

/**
 * Listens for events on a connection and prints each event out as it is received.
 */
public class EventMonitor extends TestBase
{
    private static final int MAX_EVENTS = 100;
    private static final int TIMEOUT = 30 * 1000;

    public static void RunTest(ILog logger, TargetServer server) throws Exception
    {
        TestBase.logger = logger;
        try
        {
            connect(server);
            Set<String> everything = new HashSet<String>();
            everything.add("*");
            Event.register(connection, everything);

            int eventsReceived = 0;
            long started = System.currentTimeMillis();

            while (eventsReceived < MAX_EVENTS && System.currentTimeMillis() - started < TIMEOUT)
            {
                Set<Event.Record> events = Event.next(connection);
                announce(events.size() + " event(s) received");

                // print the events out in a nice format
                String format = "%10s %5s %3s %10s %50s";
                logf(format + " date       time%n", "class", "id", "uuid", "operation", "reference");
                for (Event.Record e : events)
                {
                    logf(format, e.clazz, e.id, e.objUuid, e.operation, e.ref);
                    logf(" %te/%<tm/%<tY %<tH.%<tM.%<tS %n", e.timestamp);
                    logln("associated snapshot: " + e.snapshot);
                }
                eventsReceived += events.size();
            }
        } finally
        {
            disconnect();
        }
    }
}
