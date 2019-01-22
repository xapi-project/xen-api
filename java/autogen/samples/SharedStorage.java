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

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.xensource.xenapi.Host;
import com.xensource.xenapi.PBD;
import com.xensource.xenapi.SR;
import com.xensource.xenapi.Types.XenAPIException;

/**
 * Creates a shared NFS SR.
 * 
 * java equivalent to the cli command: xe sr-create type=nfs name-label=<name> device-config-server=<nfsServer>
 * device-config-serverpath=<serverPath> shared=true
 */
public class SharedStorage extends TestBase
{
    private String nfsServer;
    private String serverPath;

    public String getTestName() {
        return "SharedStorage";
    }

    public SharedStorage(String nfsServer, String serverPath) {
        this.nfsServer = nfsServer;
        this.serverPath = serverPath;
    }

    protected void TestCore() throws Exception
    {
        if (nfsServer == null || serverPath == null) {
            log("nfsServer and nfsPath were not provided. Skipping SharedStorage test");
            throw new SkippingException();
        }

        log("getting list of hosts and choosing the first one...");
        Host host = (Host) Host.getAll(connection).toArray()[0];
        logf("Got host %s", host.getNameLabel(connection));

        // create config parameter for shared storage on nfs server
        Map<String, String> deviceConfig = new HashMap<String, String>();
        deviceConfig.put("server", nfsServer);
        deviceConfig.put("serverpath", serverPath);

        log("creating a shared storage SR ...");

        SR newSr = SR.create(connection, host, deviceConfig, 100000L,
                "NFS SR created by SharedStorage.java",
                String.format("[%s:%s] Created at %s", nfsServer, serverPath, new Date().toString()),
                "nfs", "unused", true, new HashMap<String, String>());

        log("Now unplugging any PBDs");
        // First unplug any PBDs associated with the SR
        Set<PBD> pbds = PBD.getAll(connection);
        for (PBD pbd : pbds)
        {
            if (pbd.getSR(connection).equals(newSr))
            {
                pbd.unplug(connection);
            }
        }

        log("Now destroying the newly-created SR");
        newSr.destroy(connection);

        // try a couple of erroneous calls to generate exceptions

        log("now trying to create one with bad device_config - should throw exception");
        try {
            SR.create(connection, host, new HashMap<String, String>(), 100000L, "bad_device_config", "description", "nfs",
                    "contenttype", true, new HashMap<String, String>());
        }
        catch (XenAPIException ex) {
            logf("Received expected exception: %s", ex.toString());
        }

        /*
        comment this out until CA-182929 is fixed
        log("now trying to create one with a bad 'type' field - should throw a different exception");
        try {
            SR.create(connection, host, new HashMap<String, String>(), 100000L, "bad_sr_type", "description", "made_up",
                    "", true, new HashMap<String, String>());
        }
        catch (XenAPIException ex) {
            logf("Received expected exception: %s", ex.toString());
        }
        */
    }
}
