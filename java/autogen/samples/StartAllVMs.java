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

import java.util.Map;

import com.xensource.xenapi.Types;
import com.xensource.xenapi.VM;

/**
 * Connects to a host and tries to start each VM on it.
 */
public class StartAllVMs extends TestBase
{
    public String getTestName() {
        return "StartAllVMs";
    }

    public void TestCore() throws Exception
    {
        log("We'll try to start all the available VMs. Since most of them will be templates");
        log("This should cause a fair number of exceptions");

        announce("Getting all VM records");
        Map<VM, VM.Record> vms = VM.getAllRecords(connection);
        log("got: " + vms.size() + " records");

        announce("Start all the available VMs");
        for (VM.Record record : vms.values())
        {
            log("Trying to start: " + record.nameLabel);
            if (record.isATemplate)
                log("(template) ");
            try
            {
                VM vm = VM.getByUuid(connection, record.uuid);
                vm.start(connection, false, false);
                log(" -- success!");
            } catch (Types.VmIsTemplate ex)
            {
                if (record.isATemplate)
                    log(" -- expected failure: can't start a template.");
                else
                    throw ex;
            } catch (Types.NoHostsAvailable ex)
            {
                log(" -- predictable failure: insufficient host capacity to start the VM");
            } catch (Types.OperationNotAllowed ex)
            {
                if (record.isControlDomain)
                    log(" -- expected failure: can't start the control domain");
                else
                    throw ex;
            } catch (Types.VmBadPowerState ex)
            {
                if (record.powerState != Types.VmPowerState.HALTED)
                    log(" -- expected failure: bad power state (actual: " + ex.actual + " expected: " + ex.expected
                            + ")");
                else
                    throw ex;
            } catch (Types.LicenceRestriction ex)
            {
                log(" -- predictable failure: licence restriction");
            } catch (Types.BootloaderFailed ex)
            {
                log(" -- predictable failure: the vm would not boot (" + ex + ")");
            }
        }
    }
}
