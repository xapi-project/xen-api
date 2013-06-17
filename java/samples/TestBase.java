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

import java.net.URL;
import java.util.Map;
import java.util.Set;

import com.xensource.xenapi.APIVersion;
import com.xensource.xenapi.Connection;
import com.xensource.xenapi.Network;
import com.xensource.xenapi.Pool;
import com.xensource.xenapi.SR;
import com.xensource.xenapi.Session;
import com.xensource.xenapi.Task;
import com.xensource.xenapi.Types;
import com.xensource.xenapi.VM;

public abstract class TestBase
{
    protected static ILog logger;
    protected static Connection connection;
    private static String connectionName;

    protected static void connect(TargetServer target) throws Exception
    {
        /*
         * Old style: Connection constructor performs login_with_password for you. Deprecated.
         *
         * connection = new Connection(target.Hostname, target.Username, target.Password);
         */

        /*
         * New style: we are responsible for Session login/logout.
         */
        connection = new Connection(new URL("http://" + target.Hostname));
        logln(String.format("logging in to '%s' as '%s' with password '%s'...", target.Hostname, target.Username,
                target.Password));
        logln("Success");
        Session.loginWithPassword(connection, target.Username, target.Password, APIVersion.latest().toString());
        logln(String.format("Session API version is %s", connection.getAPIVersion().toString()));

        connectionName = target.Hostname;
    }

    protected static void disconnect() throws Exception
    {
        logln("disposing connection for " + connectionName);
        Session.logout(connection);
    }

    protected static void hRule()
    {
        logln("----------------------------------------------------------------------");
    }

    protected static void announce(String s)
    {
        hRule();
        log(s);
        hRule();
    }

    protected static void log(String s)
    {
        logger.log(s);
    }

    protected static void logf(String s, Object... args)
    {
        logger.log(String.format(s, args));
    }

    protected static void logln(String s)
    {
        logger.logln(s);
    }

    protected static void logln(Object o)
    {
        logln(o.toString());
    }

    protected static TargetServer ParseTarget(String[] args)
    {
        return new TargetServer(args[0], args[1], args[2]);
    }

    /**
     * Given a task in progress, sleeps until it completes, waking to print status reports periodically.
     */
    protected static void waitForTask(Connection c, Task task, int delay) throws Exception
    {
        while (task.getStatus(c) == Types.TaskStatusType.PENDING)
        {
            logf("%.2f;", task.getProgress(c));
            Thread.sleep(delay);
        }
        logln("");
    }

    protected static SR getDefaultSR() throws Exception
    {
        Set<Pool> pools = Pool.getAll(connection);
        Pool pool = (pools.toArray(new Pool[0]))[0];
        return pool.getDefaultSR(connection);
    }

    protected static VM getFirstWindowsTemplate() throws Exception
    {
        Map<VM, VM.Record> all_recs = VM.getAllRecords(connection);
        for (Map.Entry<VM, VM.Record> e : all_recs.entrySet())
        {
            if (e.getValue().isATemplate == true && e.getValue().nameLabel.contains("Windows"))
            {
                return e.getKey();
            }
        }

        throw new Exception("No Windows templates found!");
    }

    /**
     * Finds the first network (probably the one created by AddNetwork.java).
     */
    protected static Network getFirstNetwork() throws Exception
    {
        Set<Network> networks = Network.getAll(connection);
        for (Network i : networks)
        {
            return i;
        }

        throw new Exception("No networks found!");
    }
}
