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
import com.xensource.xenapi.EventBatch;

/**
 * Listens for events on a connection and prints each event out as it is received.
 */
public class EventMonitor extends TestBase
{
    private static final int MAX_TRIES = 10;
    private static final double TIMEOUT_SEC = 30;
    private static final int INTERVAL = 10;

    public String getTestName() {
        return "EventMonitor";
    }

    protected void TestCore() throws Exception
    {
        Set<String> eventTypes = new HashSet<String>();
        eventTypes.add("*");

        int tries = 0;
        String token = "";

        while (tries <= MAX_TRIES)
        {
            tries++;
            EventBatch eventBatch = Event.from(connection, eventTypes, token, TIMEOUT_SEC);
            token = eventBatch.token;
            announce("Poll %d out of %d: %d event(s) received", tries, MAX_TRIES, eventBatch.events.size());

            // print the events out in a nice format
            String format = "%-10s %-10s %-10s %-50s%n";
            logf(format, "class", "id", "operation", "reference");
            for (Event.Record e : eventBatch.events)
            {
                logf(format, e.clazz, e.id, e.operation, e.ref);
                log("associated snapshot:\n" + e.snapshot);
            }

            logf("Waiting %d seconds before next poll...", INTERVAL);
            Thread.sleep(INTERVAL * 1000);
        }
    }
}
