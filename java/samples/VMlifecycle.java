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
import java.util.Set;

import com.xensource.xenapi.Types;
import com.xensource.xenapi.VM;

/**
 * Takes a VM through the various lifecycle states. Requires a shutdown VM with tools installed.
 */
public class VMlifecycle extends TestBase
{
    public String getTestName() {
        return "VMlifecycle";
    }

    protected void TestCore() throws Exception
    {
        // find a halted real virtual machine.
        Set<VM> refVMs = VM.getAll(connection);
        VM chosen = null;
        for (VM vm : refVMs)
        {
            VM.Record record = vm.getRecord(connection);
            if (!record.isATemplate && !record.isControlDomain && record.powerState == Types.VmPowerState.HALTED)
            {
                chosen = vm;
                break;
            }
        }

        if (chosen == null)
        {
            throw new Exception("We need a non-template, halted VM to clone. Can't find one, so aborting.");
        } else
        {
            // clone the vm we found, name it and set its description
            String cloneName = "Cloned by VMlifecycle.java";

            log("We're cloning: " + chosen.getNameLabel(connection) + " to " + cloneName);

            VM cloneVM = chosen.createClone(connection, cloneName);
            cloneVM.setNameDescription(connection, "Created at " + new Date().toString());

            logf("VM Name: %s Description: %s\n", cloneVM.getNameLabel(connection), cloneVM
                    .getNameDescription(connection));
            printPowerState(cloneVM);

            // power-cycle it
            cloneVM.start(connection, true, false);
            printPowerState(cloneVM);
            cloneVM.unpause(connection);
            printPowerState(cloneVM);
            cloneVM.suspend(connection);
            printPowerState(cloneVM);
            cloneVM.resume(connection, false, false);
            printPowerState(cloneVM);
            cloneVM.cleanReboot(connection);
            printPowerState(cloneVM);
            cloneVM.cleanShutdown(connection);
            printPowerState(cloneVM);
        }
    }

    private void printPowerState(VM vm) throws Exception
    {
        log("VM powerstate: " + vm.getPowerState(connection));
    }
}
