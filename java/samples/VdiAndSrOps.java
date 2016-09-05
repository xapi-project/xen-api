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

import java.util.HashMap;
import java.util.Set;

import com.xensource.xenapi.APIVersion;
import com.xensource.xenapi.Connection;
import com.xensource.xenapi.Host;
import com.xensource.xenapi.PBD;
import com.xensource.xenapi.SR;
import com.xensource.xenapi.Types;
import com.xensource.xenapi.VDI;

/**
 * Performs various SR and VDI tests, including creating a dummy SR.
 */
public class VdiAndSrOps extends TestBase
{
    public String getTestName() {
        return "VdiAndSrOps";
    }

    protected void TestCore() throws Exception
    {
        APIVersion version = connection.getAPIVersion();

        for (boolean nullSmConfig : new boolean[] { true, false })
        {
            testSrOp("SR.create", false, nullSmConfig, version);
            testSrOp("SR.forget", false, nullSmConfig, version);
            testSrOp("SR.createAsync", false, nullSmConfig, version);
            testSrOp("SR.forget", false, nullSmConfig, version);
            testSrOp("SR.forget", false, nullSmConfig, version);
            testSrOp("SR.introduce", false, nullSmConfig, version);
            testSrOp("SR.forget", false, nullSmConfig, version);
            testSrOp("SR.introduceAsync", false, nullSmConfig, version);
            testSrOp("SR.forget", false, nullSmConfig, version);
        }
        testVdiOp("VDI.snapshot", version);
        testVdiOp("VDI.createClone", version);
        testVdiOp("VDI.snapshotAsync", version);
        testVdiOp("VDI.createCloneAsync", version);
    }

    private void testVdiOp(String op, APIVersion version) throws Exception
    {
        log("--attempting " + op + " with null driverParams");
        vdiOpWithNullDriverParams(connection, op);

        log("--attempting " + op + " with non-null driverParams");
        vdiOpWithNonNullDriverParams(connection, op);
    }

    private void testSrOp(String op, boolean deprecated_response_ok, boolean nullSmConfig, APIVersion version)
            throws Exception
    {
        try
        {
            if (nullSmConfig)
            {
                log("--attempting " + op + " with null smConfig... ");
                srOpWithNullSmConfig(connection, op);
            } else
            {
                log("--attempting " + op + " with non-null smConfig... ");
                srOpWithNonNullSmConfig(connection, op);                
            }
            log("success");
        }
        catch (Types.XenAPIException ex)
        {
            // Bug (CA-23404) workaround: sometimes the server doesn't send a proper MESSAGE_DEPRECATED,
            // so the bindings can't parse it properly. Perform manual check...
            if (errorDescriptionIs(ex.errorDescription, "MESSAGE_DEPRECATED"))
            {
                if (deprecated_response_ok)
                {
                    log(op + " threw MessageDeprecated (but this is OK)");
                }
                else
                {
                    throw ex;
                }
            }
        }
    }

    private void srOpWithNonNullSmConfig(Connection c, String op) throws Exception
    {
        HashMap<String, String> smConfig = new HashMap<String, String>();
        smConfig.put("testKey", "testValue");
        srOpLong(c, smConfig, op);
    }

    private void srOpWithNullSmConfig(Connection c, String op) throws Exception
    {
        HashMap<String, String> smConfig = new HashMap<String, String>();
        srOpLong(c, smConfig, op);
    }

    private void vdiOpWithNonNullDriverParams(Connection c, String op) throws Exception
    {
        HashMap<String, String> smConfig = new HashMap<String, String>();
        smConfig.put("testKey", "testValue");
        vdiOpLong(c, smConfig, op);
    }

    private void vdiOpWithNullDriverParams(Connection c, String op) throws Exception
    {
        HashMap<String, String> smConfig = new HashMap<String, String>();
        vdiOpLong(c, smConfig, op);
    }

    private static Boolean errorDescriptionIs(String[] errDesc, String firstElement)
    {
        return errDesc != null && errDesc.length > 0 && errDesc[0].compareTo(firstElement) == 0;
    }

    private static final String FAKE_VDI_NAME = "madeupvdi";

    private void vdiOpLong(Connection c, HashMap<String, String> driverParams, String op) throws Exception
    {
        try
        {
            VDI dummy = Types.toVDI(FAKE_VDI_NAME);
            if (op.equals("VDI.snapshot"))
            {
                dummy.snapshot(c, driverParams);
            } else if (op.equals("VDI.createClone"))
            {
                dummy.createClone(c, driverParams);
            } else if (op.equals("VDI.snapshotAsync"))
            {
                dummy.snapshotAsync(c, driverParams);
            } else if (op.equals("VDI.createCloneAsync"))
            {
                dummy.createCloneAsync(c, driverParams);
            } else
            {
                throw new Exception("bad op");
            }
        } catch (Types.HandleInvalid ex)
        {
            log("Expected error: HANDLE_INVALID. that's ok.");
            /* We're happy with this, since it means that the call made it through to xen
             * and an attempt was made to execute it. */
        }
    }

    private static final String TEST_SR_NAME = "TestSR: DO NOT USE (created by VdiAndSrOps.java)";
    private static final String TEST_SR_DESC = "Should be automatically deleted";
    private static final String TEST_SR_TYPE = "dummy";
    private static final String TEST_SR_CONTENT = "contenttype";
    private static final long TEST_SR_SIZE = 100000L;

    private void srOpLong(Connection c, HashMap<String, String> smConfig, String op) throws Exception
    {
        try
        {
            Host our_host = (Host) Host.getAll(c).toArray()[0];
            if (op.equals("SR.create"))
            {
                SR.create(c, our_host, new HashMap<String, String>(), TEST_SR_SIZE, TEST_SR_NAME, TEST_SR_DESC,
                        TEST_SR_TYPE, TEST_SR_CONTENT, true, smConfig);
            } else if (op.equals("SR.createAsync"))
            {
                SR.createAsync(c, our_host, new HashMap<String, String>(), TEST_SR_SIZE, TEST_SR_NAME, TEST_SR_DESC,
                        TEST_SR_TYPE, TEST_SR_CONTENT, true, smConfig);
            } else if (op.equals("SR.forget"))
            {
                Set<SR> srs = SR.getByNameLabel(c, TEST_SR_NAME);
                for (SR sr : srs)
                {
                    // First destroy any PBDs associated with the SR
                    Set<PBD> pbds = PBD.getAll(c);
                    for (PBD pbd : pbds)
                    {
                        if (pbd.getSR(c).equals(sr))
                        {
                            pbd.unplug(c);
                            pbd.destroy(c);
                        }
                    }
                    sr.forget(c);
                    break;
                }
            } else if (op.equals("SR.introduce"))
            {
                SR.introduce(c, "uuid", TEST_SR_NAME, TEST_SR_DESC, TEST_SR_TYPE, TEST_SR_CONTENT, true, smConfig);
            } else if (op.equals("SR.introduceAsync"))
            {
                SR.introduceAsync(c, "uuid", TEST_SR_NAME, TEST_SR_DESC, TEST_SR_TYPE, TEST_SR_CONTENT, true, smConfig);
            } else
            {
                throw new Exception("bad op");
            }
        } catch (Types.SrUnknownDriver ex)
        {
            log("Expected error: SR unknown driver. that's ok");
        } catch (Types.XenAPIException ex)
        {
            // Our call parameters are not good and should cause a particular error
            if (errorDescriptionIs(ex.errorDescription, "SR_BACKEND_FAILURE_102"))
            {
                log("Expected error: SR backend failure 102. that's ok.");
                /* 'The request is missing the server parameter'
                 * 
                 * We're happy with this, since it means that the call made it through to xen
                 * and an attempt was made to execute it. */
                return;
            } else if (errorDescriptionIs(ex.errorDescription, "SR_BACKEND_FAILURE_101"))
            {
                /* 'The request is missing the serverpath parameter' */
                log("Expected error: SR backend failure 101. that's ok.");
                return;
            } else
            {
                // otherwise, there was a more serious error, which should be
                // passed upwards
                throw ex;
            }
        }
    }
}
