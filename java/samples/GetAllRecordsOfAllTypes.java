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

import com.xensource.xenapi.*;

/**
 * Does what it says on the tin.
 */
public class GetAllRecordsOfAllTypes extends TestBase
{
    public String getTestName() {
        return "GetAllRecordsOfAllTypes";
    }

    protected void TestCore() throws Exception
    {
        log("We'll try to retrieve all the records for all types of objects");
        log("This should exercise most of the marshalling code");
        callAutoCode();
    }

    /*
     * Here is a way to do it with common lisp in emacs. Not sure about the
     * sanity aspect. lisp code ----------- load into emacs, start slime with
     * M-x slime, edit this file as lisp with M-x slime-mode, and then we can
     * use C-1 M-C-x to evaluate arbitrary lisp expressions and paste them into
     * the buffer. M-x lisp-mode and M-x java-mode can be used to switch between
     * the two views of the file
     *
     * (format nil "~A" "test")
     *
     * ;;list of all database objects culled from javadoc ;;missing Event,
     * Session, User, VTPM (because they don't have get_all_records methods.)
     * (defvar objects '( "Console" "Crashdump" "Host" "HostCpu" "HostCrashdump"
     * "HostMetrics" "HostPatch" "Network" "PBD" "PIF" "PIFMetrics" "Pool" "SM"
     * "SR" "Task" "VBD" "VBDMetrics" "VDI" "VIF" "VIFMetrics" "VM"
     * "VMGuestMetrics" "VMMetrics")) (defvar miami-only-objects '( "Bond"
     * "PoolPatch" "VLAN" ))
     *
     *
     * (defun java-test-getAllRecords-proc(string) (format nil " //automatically
     * generated. Do not modify public static void test~As() throws Exception {
     * announce( \"Get all the ~:* ~A Records\" ); Map<~:* ~A,~:* ~A.Record>
     * allrecords = ~:* ~A.getAllRecords(connection); log( \"got:
     * \"+ allrecords.size() + \" records\" ); if (allrecords.size()>0){
     * announce( \"Print out a ~:* ~A record \" ); log(
     * allrecords.values().toArray()[0]); } hRule(); } " string))
     *
     * (defun string-concat-map (fn lst) (format nil "~{ ~A ~}" (mapcar fn
     * lst)))
     *
     * (defun many-test-getAllRecords-procedures (stringlist) (string-concat-map
     * #'java-test-getAllRecords-proc stringlist))
     *
     * (defun calltestproc(String) (format nil "test~As();" String))
     *
     * (defun callmiamionlytestproc(String) (format nil "if (!rio) test~As();"
     * String))
     *
     * (defun callAutoCode(stringlist miami-only-stringlist) (format nil
     * "~{~A~}" (list (format nil " public static void callAutoCode(Boolean rio)
     * throws Exception~%") (format nil " {~%") (format nil "~{ ~A~%~}" (mapcar
     * #'calltestproc stringlist)) (format nil "~{ ~A~%~}" (mapcar
     * #'callmiamionlytestproc miami-only-stringlist)) (format nil " }~%"))))
     *
     * (progn (format t "//********** Automatically generated code **********")
     * (format t "~A~%" (many-test-getAllRecords-procedures objects)) (format t
     * "~A~%" (many-test-getAllRecords-procedures miami-only-objects)) (format t
     * "~A~%" (callautocode objects miami-only-objects)) (format t "//**********
     * End of automatically generated code **********")) ;;to create the
     * auto-generated code, edit the file in lisp-mode. ;;use C-M-x to evaluate
     * the above expressions, checking that the output of the last one looks
     * right. ;;then place the cursor below this line and type C-1 M-C-x to
     * paste that output into this file. If wrong use C-_ to undo the paste
     */

    // ********** Automatically generated code **********
    // automatically generated. Do not modify
    private void testConsoles() throws Exception
    {
        announce("Get all the  Console Records");
        Map<Console, Console.Record> allrecords = Console.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  Console record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }


    // automatically generated. Do not modify
    private void testCrashdumps() throws Exception
    {
        announce("Get all the  Crashdump Records");
        Map<Crashdump, Crashdump.Record> allrecords = Crashdump.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  Crashdump record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testHosts() throws Exception
    {
        announce("Get all the  Host Records");
        Map<Host, Host.Record> allrecords = Host.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  Host record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testHostCpus() throws Exception
    {
        announce("Get all the  HostCpu Records");
        Map<HostCpu, HostCpu.Record> allrecords = HostCpu.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  HostCpu record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testHostCrashdumps() throws Exception
    {
        announce("Get all the  HostCrashdump Records");
        Map<HostCrashdump, HostCrashdump.Record> allrecords = HostCrashdump.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  HostCrashdump record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testHostMetricss() throws Exception
    {
        announce("Get all the  HostMetrics Records");
        Map<HostMetrics, HostMetrics.Record> allrecords = HostMetrics.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  HostMetrics record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testHostPatchs() throws Exception
    {
        announce("Get all the  HostPatch Records");
        Map<HostPatch, HostPatch.Record> allrecords = HostPatch.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  HostPatch record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testNetworks() throws Exception
    {
        announce("Get all the  Network Records");
        Map<Network, Network.Record> allrecords = Network.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  Network record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testPBDs() throws Exception
    {
        announce("Get all the  PBD Records");
        Map<PBD, PBD.Record> allrecords = PBD.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  PBD record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testPIFs() throws Exception
    {
        announce("Get all the  PIF Records");
        Map<PIF, PIF.Record> allrecords = PIF.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  PIF record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testPIFMetricss() throws Exception
    {
        announce("Get all the  PIFMetrics Records");
        Map<PIFMetrics, PIFMetrics.Record> allrecords = PIFMetrics.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  PIFMetrics record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testPools() throws Exception
    {
        announce("Get all the  Pool Records");
        Map<Pool, Pool.Record> allrecords = Pool.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        for (Pool key : allrecords.keySet())
        {
            announce("Print out a  Pool record ");
            log(allrecords.get(key).toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testSMs() throws Exception
    {
        announce("Get all the  SM Records");
        Map<SM, SM.Record> allrecords = SM.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  SM record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testSRs() throws Exception
    {
        announce("Get all the  SR Records");
        Map<SR, SR.Record> allrecords = SR.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  SR record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testTasks() throws Exception
    {
        announce("Get all the  Task Records");
        Map<Task, Task.Record> allrecords = Task.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  Task record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testVBDs() throws Exception
    {
        announce("Get all the  VBD Records");
        Map<VBD, VBD.Record> allrecords = VBD.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  VBD record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testVBDMetricss() throws Exception
    {
        announce("Get all the  VBDMetrics Records");
        Map<VBDMetrics, VBDMetrics.Record> allrecords = VBDMetrics.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  VBDMetrics record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testVDIs() throws Exception
    {
        announce("Get all the  VDI Records");
        Map<VDI, VDI.Record> allrecords = VDI.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  VDI record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testVIFs() throws Exception
    {
        announce("Get all the  VIF Records");
        Map<VIF, VIF.Record> allrecords = VIF.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  VIF record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testVIFMetricss() throws Exception
    {
        announce("Get all the  VIFMetrics Records");
        Map<VIFMetrics, VIFMetrics.Record> allrecords = VIFMetrics.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  VIFMetrics record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testVMs() throws Exception
    {
        announce("Get all the  VM Records");
        Map<VM, VM.Record> allrecords = VM.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  VM record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testVMGuestMetricss() throws Exception
    {
        announce("Get all the  VMGuestMetrics Records");
        Map<VMGuestMetrics, VMGuestMetrics.Record> allrecords = VMGuestMetrics.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  VMGuestMetrics record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testVMMetricss() throws Exception
    {
        announce("Get all the  VMMetrics Records");
        Map<VMMetrics, VMMetrics.Record> allrecords = VMMetrics.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  VMMetrics record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testBonds() throws Exception
    {
        announce("Get all the  Bond Records");
        Map<Bond, Bond.Record> allrecords = Bond.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  Bond record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testPoolPatchs() throws Exception
    {
        announce("Get all the  PoolPatch Records");
        Map<PoolPatch, PoolPatch.Record> allrecords = PoolPatch.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  PoolPatch record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    // automatically generated. Do not modify
    private void testVLANs() throws Exception
    {
        announce("Get all the  VLAN Records");
        Map<VLAN, VLAN.Record> allrecords = VLAN.getAllRecords(connection);
        log("got: " + allrecords.size() + " records");
        if (allrecords.size() > 0)
        {
            announce("Print out a  VLAN record ");
            log(allrecords.values().toArray()[0].toString());
        }
        hRule();
    }

    private void callAutoCode() throws Exception
    {
        testConsoles();
        testCrashdumps();
        testHosts();
        testHostCpus();
        testHostCrashdumps();
        testHostMetricss();
        testHostPatchs();
        testNetworks();
        testPBDs();
        testPIFs();
        testPIFMetricss();
        testPools();
        testSMs();
        testSRs();
        testTasks();
        testVBDs();
        testVBDMetricss();
        testVDIs();
        testVIFs();
        testVIFMetricss();
        testVMs();
        testVMGuestMetricss();
        testVMMetricss();
        testBonds();
        testPoolPatchs();
        testVLANs();
    }

    // ********** End of automatically generated code **********
}
