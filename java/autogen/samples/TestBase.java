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

import com.xensource.xenapi.*;

public abstract class TestBase
{
    /**
     * Exception thrown when we want to skip a test
     */
    public class SkippingException extends Exception{

    }

    protected FileLogger logger;
    protected Connection connection;
    private String connectionName;

    protected abstract void TestCore() throws Exception;
    public abstract String getTestName();

    public void RunTest(FileLogger logger, TargetServer server) throws Exception {
        this.logger = logger;
        connect(server);
        try {
            TestCore();
        }
        finally {
            disconnect();
        }
    }

    private void connect(TargetServer target) throws Exception
    {
        connection = new Connection(new URL("https://" + target.Hostname));
        log(String.format("logging in to '%s'...", target.Hostname));
        Session.loginWithPassword(connection, target.Username, target.Password, APIVersion.latest().toString());
        logf("Success! Session API version is %s", connection.getAPIVersion().toString());

        connectionName = target.Hostname;
    }

    private void disconnect() throws Exception
    {
        logf("disposing connection for %s", connectionName);
        Session.logout(connection);
    }

    protected void hRule()
    {
        log("----------------------------------------------------------------------");
    }

    protected void announce(String s, Object... args)
    {
        hRule();
        logf(s, args);
        hRule();
    }

    protected void logf(String s, Object... args)
    {
        logger.logf(String.format(s, args));
    }

    protected void log(String s)
    {
        logger.log(s);
    }

    /**
     * Given a task in progress, sleeps until it completes, waking to print status reports periodically.
     */
    protected void waitForTask(Connection c, Task task, int delay) throws Exception
    {
        while (task.getStatus(c) == Types.TaskStatusType.PENDING)
        {
            logf("%.2f;", task.getProgress(c));
            Thread.sleep(delay);
        }
    }

    /**
     * Get the pool's default storage; if null, get a local storage.
     * @return
     * @throws Exception
     */
    protected SR getStorage() throws Exception {
        Set<Pool> pools = Pool.getAll(connection);
        Pool pool = (Pool)pools.toArray()[0];
        SR storage = pool.getDefaultSR(connection);

        if (storage != null && !storage.isNull())
            return storage;

        Map<SR, SR.Record> srs = SR.getAllRecords(connection);
        Map<Host, Host.Record> hosts = Host.getAllRecords(connection);

        for (Map.Entry<SR, SR.Record> pair : srs.entrySet()) {
            SR.Record sr = pair.getValue();

            if (sr.shared || "iso".equals(sr.contentType) || !canCreateVdi(sr))
                continue;

            Set<PBD> pbds = sr.PBDs;
            for (PBD pbd : pbds) {
                if (!pbd.getCurrentlyAttached(connection))
                    continue;

                for (Map.Entry<Host, Host.Record> host : hosts.entrySet()) {
                    if (pbd.getHost(connection).getUuid(connection).equals(host.getValue().uuid)) {
                        return pair.getKey();
                    }
                }
            }
        }

        return null;
    }

    private boolean canCreateVdi(SR.Record rec) throws Exception {
        Set<SM> sms = SM.getAll(connection);
        for (SM sm : sms) {
            if (sm.getType(connection).equals(rec.type)) {
                if (sm.getFeatures(connection).containsKey("VDI_CREATE")) {
                    return true;
                }
            }
        }
        return false;
    }

    protected VM getFirstWindowsTemplate() throws Exception
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
    protected Network getFirstNetwork() throws Exception
    {
        Set<Network> networks = Network.getAll(connection);
        for (Network i : networks)
        {
            return i;
        }

        throw new Exception("No networks found!");
    }

    /**
     * Checks whether the master has hvm capabilities.
     */
    protected void checkMasterHvmCapable() throws Exception
    {
        log("checking master has hvm capabilities...");
        Pool pool = (Pool) Pool.getAll(connection).toArray()[0];
        Host master = pool.getMaster(connection);
        Set<String> capabilities = master.getCapabilities(connection);

        Boolean hvmCapable = false;
        for (String s: capabilities)
            if (s.contains("hvm")) {
                hvmCapable = true;
                break;
            }

        if (!hvmCapable)
            throw new Exception("Master has no hvm capabilities!");
    }
}
