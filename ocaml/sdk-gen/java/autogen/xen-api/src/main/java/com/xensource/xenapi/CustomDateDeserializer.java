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
import java.util.TimeZone;

/**
 * {@link CustomDateDeserializer} is a Jackson JSON deserializer for parsing
 * {@link Date} objects
 * from custom date formats used in Xen-API responses.
 */
public class CustomDateDeserializer extends StdDeserializer<Date> {

    /**
     * Array of {@link SimpleDateFormat} objects representing the date formats
     * used in xen-api responses.
     * <br>
     * RFC-3339 date formats can be returned in either Zulu or time zone agnostic.
     * This list is not an exhaustive list of formats supported by RFC-3339, rather
     * a set of formats that will enable the deserialization of xen-api dates.
     * Formats are listed in order of decreasing precision. When adding
     * to this list, please ensure the order is kept.
     */
    private static final SimpleDateFormat[] dateFormatsUtc = {
        // Most commonly returned formats
        new SimpleDateFormat("yyyyMMdd'T'HHmmss'Z'"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'"),
        new SimpleDateFormat("ss.SSS"),

        // Other
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"),
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ss'Z'"),
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ss.SSS'Z'"),
        new SimpleDateFormat("yyyyMMdd'T'HHmmss.SSS'Z'"),

        // Formats without timezone info default to UTC in xapi
        new SimpleDateFormat("yyyyMMdd'T'HHmmss.SSS"),
        new SimpleDateFormat("yyyyMMdd'T'HHmmss"),
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ss.SSS"),
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ss"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss"),
    };

    /**
     * Array of {@link SimpleDateFormat} objects representing the date formats for
     * local time.
     * These formats are used to parse dates in local time zones.
     * Formats are listed in order of decreasing precision. When adding
     * to this list, please ensure the order is kept.
     */
    private static final SimpleDateFormat[] dateFormatsLocal = {
        // no dashes, no colons
        new SimpleDateFormat("yyyyMMdd'T'HHmmss.SSSZZZ"),
        new SimpleDateFormat("yyyyMMdd'T'HHmmss.SSSZZ"),
        new SimpleDateFormat("yyyyMMdd'T'HHmmss.SSSZ"),
        new SimpleDateFormat("yyyyMMdd'T'HHmmss.SSSXXX"),
        new SimpleDateFormat("yyyyMMdd'T'HHmmss.SSSXX"),
        new SimpleDateFormat("yyyyMMdd'T'HHmmss.SSSX"),

        new SimpleDateFormat("yyyyMMdd'T'HHmmssZZZ"),
        new SimpleDateFormat("yyyyMMdd'T'HHmmssZZ"),
        new SimpleDateFormat("yyyyMMdd'T'HHmmssZ"),
        new SimpleDateFormat("yyyyMMdd'T'HHmmssXXX"),
        new SimpleDateFormat("yyyyMMdd'T'HHmmssXX"),
        new SimpleDateFormat("yyyyMMdd'T'HHmmssX"),

        // no dashes, with colons
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ss.SSSZZZ"),
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ss.SSSZZ"),
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ss.SSSZ"),
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ss.SSSXXX"),
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ss.SSSXX"),
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ss.SSSX"),

        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ssZZZ"),
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ssZZ"),
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ssZ"),
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ssXXX"),
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ssXX"),
        new SimpleDateFormat("yyyyMMdd'T'HH:mm:ssX"),

        // dashes and colons
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZZZ"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZZ"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXX"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSX"),

        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZZZ"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZZ"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXX"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssX"),
    };

    /**
     * Constructs a {@link CustomDateDeserializer} instance.
    */
    public CustomDateDeserializer() {
        this(null);
    }

    /**
     * Constructs a {@link CustomDateDeserializer} instance with the specified value
     * type.
     *
     * @param t The value type to handle (can be null, handled by superclass)
     */
    public CustomDateDeserializer(Class t) {
        super(t);
        var utcTimeZone = TimeZone.getTimeZone("UTC");
        for (var utcFormatter : dateFormatsUtc) {
            utcFormatter.setTimeZone(utcTimeZone);
        }
    }

    /**
     * Deserializes a {@link Date} object from the given JSON parser.
     *
     * @param jsonParser             The JSON parser containing the date value to
     *                               deserialize
     * @param deserializationContext The deserialization context
     * @return The deserialized {@link Date} object
     * @throws IOException if an I/O error occurs during deserialization
     */
    @Override
    public Date deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
        var text = jsonParser.getText();
        Date localDate = null;
        Date utcDate = null;

        for (SimpleDateFormat formatter : dateFormatsUtc) {
            try {
                utcDate = formatter.parse(text);
                break;
            } catch (ParseException e) {
                // ignore
            }
        }

        for (SimpleDateFormat formatter : dateFormatsLocal) {
            try {
                localDate = formatter.parse(text);
                break;
            } catch (ParseException e) {
                // ignore
            }
        }

        // Some dates such as 20220101T12:30:45.123+03:00 will match both with a UTC
        // and local date format. In that case, we pick the date returned by the
        // local formatter, as it's more precise.
        // This allows us to match strings with no timezone information (such as 20220101T12:30:45.123)
        // as UTC, while correctly parsing more precise date representations
        if (localDate != null && utcDate != null) {
            return localDate; // Prioritize local format if both match
        } else if (localDate != null) {
            return localDate;
        } else if (utcDate != null) {
            return utcDate;
        } else {
            throw new IOException("Failed to deserialize a Date value.");
        }
    }
}
