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

import com.xensource.xenapi.Network;
import com.xensource.xenapi.SR;
import com.xensource.xenapi.Task;
import com.xensource.xenapi.Types;
import com.xensource.xenapi.VIF;
import com.xensource.xenapi.VM;

/**
 * Makes a new VM from a built-in template, starts and stops it.
 */
public class AsyncVMCreate extends TestBase
{
    public static void RunTest(ILog logger, TargetServer target) throws Exception
    {
        TestBase.logger = logger;
        connect(target);
        try
        {
            createVM(new Date().toString() + " (made by AsyncVMCreate.java)");
        } finally
        {
            disconnect();
        }
    }

    private static void createVM(String newvmname) throws Exception
    {
        /*First check we can start an HVM on the master*/
        checkMasterHvmCapable();

        VM template = getFirstWindowsTemplate();
        logln("Template found: " + template.getNameLabel(connection));

        /* Clone the template */
        logln("Cloning the template...");
        Task cloning = template.createCloneAsync(connection, newvmname);
        waitForTask(connection, cloning, 500);
        checkForSuccess(cloning);
        VM newVm = Types.toVM(cloning, connection);
        logln("New VM clone: " + newVm.getNameLabel(connection));

        /* Find a storage repository */
        SR defaultSR = getDefaultSR();
        logln("Default SR: " + defaultSR.getNameLabel(connection));

        /* Find a network */
        Network network = getFirstNetwork();
        logln("Network chosen: " + network.getNameLabel(connection));

        /*
         * We have our clone and our network, attach them to each other with a
         * VIF
         */
        makeVIF(newVm, network, "0");

        /* Put the SR uuid into the provision XML */
        Map<String, String> otherConfig = newVm.getOtherConfig(connection);
        String disks = otherConfig.get("disks");
        disks = disks.replace("sr=\"\"", "sr=\"" + defaultSR.getUuid(connection) + "\"");
        otherConfig.put("disks", disks);
        newVm.setOtherConfig(connection, otherConfig);

        /* Now provision the disks */
        logln("provisioning... ");
        Task provisioning = newVm.provisionAsync(connection);
        waitForTask(connection, provisioning, 5000);
        checkForSuccess(provisioning);
        logln("provisioned");

        /* Should have done the trick. Let's see if it starts. */
        logln("Starting new VM.....");
        Task t = newVm.startAsync(connection, false, false);
        waitForTask(connection, t, 250);
        checkForSuccess(t);
        logln("started");

        /* and shut it down */
        logln("Shutting it down.....");
        t = newVm.cleanShutdownAsync(connection);
        waitForTask(connection, t, 500);
        logln("Shut down.");
    }

    /* Assert that a task has succeeded. Throw an exception if not */
    private static void checkForSuccess(Task task) throws Exception
    {
        if (task.getStatus(connection) == Types.TaskStatusType.SUCCESS)
        {
            logln("task succeeded");
        } else
        {
            throw new Exception("Task failed! Task record:\n" + task.getRecord(connection));
        }
    }

    /*
     * Create a VIF by making a VIF.record and then filling in the necessary
     * fields
     */
    private static VIF makeVIF(VM newVm, Network defaultNetwork, String device) throws Exception
    {
        VIF.Record newVifRecord = new VIF.Record();

        // These three parameters are used in the command line VIF creation
        newVifRecord.VM = newVm;
        newVifRecord.network = defaultNetwork;
        newVifRecord.device = device;

        // These appear to be necessary
        newVifRecord.MTU = 1500L;
        newVifRecord.lockingMode = Types.VifLockingMode.NETWORK_DEFAULT;
        newVifRecord.qosAlgorithmType = "";
        newVifRecord.qosAlgorithmParams = new HashMap<String, String>();
        newVifRecord.otherConfig = new HashMap<String, String>();

        /* Create the VIF by asynchronous means */
        logln("Creating a VIF");
        Task task1 = VIF.createAsync(connection, newVifRecord);
        waitForTask(connection, task1, 0);

        /*
         * Now deliberately cause an error by creating a second VIF with the
         * same parameters.
         */
        Task task2;
        log("Deliberately causing an error by trying to create the same VIF twice: ");
        task2 = VIF.createAsync(connection, newVifRecord);
        waitForTask(connection, task2, 0);
        /* This should all go through, but the task shouldn't have succeeded */
        try
        {
            checkForSuccess(task2);
        } catch (Exception e)
        {
            logln("Exception duly thrown");
        }

        /*
         * However, the first call should have worked, so we can get its result
         * and use that
         */
        checkForSuccess(task1);
        return Types.toVIF(task1, connection);
    }
}
