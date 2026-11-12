/*
 * Copyright (c) Cloud Software Group, Inc.
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

import com.xensource.xenapi.JsonRpcResponseError;
import com.xensource.xenapi.Types;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * A JSON-RPC error carries the error code in message and only the error's parameters in data,
 * unlike XML-RPC, where the code is the first element of the description.
 */
public class TypesCheckErrorTest {

    private static JsonRpcResponseError error(String message, String... data) {
        var error = new JsonRpcResponseError();
        error.code = 1;
        error.message = message;
        error.data = data;
        return error;
    }

    @Test
    public void shouldReadTheOnlyParameterOfAnError() {
        var e = assertThrows(Types.HostIsSlave.class,
                () -> Types.checkError(error("HOST_IS_SLAVE", "192.0.2.1")));
        assertEquals("192.0.2.1", e.masterIPAddress);
    }

    @Test
    public void shouldReadEachParameterInOrder() {
        var e = assertThrows(Types.VmBadPowerState.class,
                () -> Types.checkError(error("VM_BAD_POWER_STATE", "OpaqueRef:vm", "halted", "running")));
        assertEquals("OpaqueRef:vm", e.vm);
        assertEquals("halted", e.expected);
        assertEquals("running", e.actual);
    }

    @Test
    public void shouldThrowTheTypedExceptionForAnErrorWithNoParameters() {
        assertThrows(Types.HaNotEnabled.class, () -> Types.checkError(error("HA_NOT_ENABLED")));
    }

    @Test
    public void shouldKeepTheCodeOfAnUnknownError() {
        var e = assertThrows(Types.XenAPIException.class,
                () -> Types.checkError(error("NOT_A_KNOWN_ERROR", "first", "second")));
        assertEquals("NOT_A_KNOWN_ERROR", e.shortDescription);
        assertArrayEquals(new String[] {"NOT_A_KNOWN_ERROR", "first", "second"}, e.errorDescription);
    }

    @Test
    public void shouldRejectAnErrorWithNoCode() {
        assertThrows(Types.BadServerResponse.class, () -> Types.checkError(error(null)));
    }
}
