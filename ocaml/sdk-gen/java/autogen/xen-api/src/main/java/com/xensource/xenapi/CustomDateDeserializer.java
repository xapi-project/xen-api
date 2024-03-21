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

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * {@link CustomDateDeserializer} is a Jackson JSON deserializer for parsing {@link Date} objects
 * from custom date formats used in Xen-API responses.
 */
public class CustomDateDeserializer extends StdDeserializer<Date> {

    /**
     * Array of {@link SimpleDateFormat} objects representing the custom date formats
     * used in XenServer API responses.
     */
    private final SimpleDateFormat[] dateFormatters
            = new SimpleDateFormat[]{
            new SimpleDateFormat("yyyyMMdd'T'HH:mm:ss'Z'"),
            new SimpleDateFormat("ss.SSS")
    };

    /**
     * Constructs a {@link CustomDateDeserializer} instance.
     */
    public CustomDateDeserializer() {
        this(null);
    }

    /**
     * Constructs a {@link CustomDateDeserializer} instance with the specified value type.
     *
     * @param t The value type to handle (can be null, handled by superclass)
     */
    public CustomDateDeserializer(Class t) {
        super(t);
    }

    /**
     * Deserializes a {@link Date} object from the given JSON parser.
     *
     * @param jsonParser             The JSON parser containing the date value to deserialize
     * @param deserializationContext The deserialization context
     * @return The deserialized {@link Date} object
     * @throws IOException if an I/O error occurs during deserialization
     */
    @Override
    public Date deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {

        for (SimpleDateFormat formatter : dateFormatters) {
            try {
                return formatter.parse(jsonParser.getText());
            } catch (ParseException e) {
                // ignore
            }
        }

        throw new IOException("Failed to deserialize a Date value.");
    }
}
