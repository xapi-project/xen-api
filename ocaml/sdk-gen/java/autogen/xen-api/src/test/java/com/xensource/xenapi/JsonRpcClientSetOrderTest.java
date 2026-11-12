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

package com.xensource.xenapi;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * xapi sends sets as JSON arrays, and some results depend on their order,
 * such as the rows of Caller.query_all_usage.
 */
public class JsonRpcClientSetOrderTest {

    private static ObjectMapper configuredMapper() {
        var mapper = new ObjectMapper();
        JsonRpcClient.configureObjectMapper(mapper);
        return mapper;
    }

    @Test
    public void shouldKeepTheOrderOfNestedSets() throws Exception {
        // Rows of [uuid; name_label; tokens; calls], highest tokens first. A HashSet
        // would reorder the columns of both rows.
        var json = "[[\"zz-uuid\",\"zz-name\",\"900.\",\"30.\"],[\"aa-uuid\",\"aa-name\",\"10.\",\"2.\"]]";

        Set<Set<String>> rows = configuredMapper().readValue(json, new TypeReference<Set<Set<String>>>() {});

        var actual = new ArrayList<List<String>>();
        for (var row : rows) {
            actual.add(new ArrayList<>(row));
        }
        assertEquals(List.of(
                List.of("zz-uuid", "zz-name", "900.", "30."),
                List.of("aa-uuid", "aa-name", "10.", "2.")), actual);
    }

    @Test
    public void shouldReadSetsAsLinkedHashSets() throws Exception {
        Set<String> set = configuredMapper().readValue("[\"b\",\"a\"]", new TypeReference<Set<String>>() {});

        assertInstanceOf(LinkedHashSet.class, set);
        assertEquals(List.of("b", "a"), new ArrayList<>(set));
    }
}
