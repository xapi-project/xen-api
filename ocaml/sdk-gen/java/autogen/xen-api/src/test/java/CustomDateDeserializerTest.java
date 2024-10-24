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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.xensource.xenapi.CustomDateDeserializer;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class CustomDateDeserializerTest {

    private static Stream<Arguments> provideDateStringsAndExpectedDates() {
        Hashtable<String, Date> dates = new Hashtable<>();

        // no dashes, no colons
        dates.put("20220101T123045", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("UTC")));
        dates.put("20220101T123045Z", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("UTC")));
        dates.put("20220101T123045+03", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("GMT+03")));
        dates.put("20220101T123045+0300", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("GMT+03")));
        dates.put("20220101T123045+03:00", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("GMT+03")));

        dates.put("20220101T123045.123", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("UTC")));
        dates.put("20220101T123045.123Z", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("UTC")));
        dates.put("20220101T123045.123+03", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("GMT+03")));
        dates.put("20220101T123045.123+0300", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("GMT+03")));
        dates.put("20220101T123045.123+03:00", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("GMT+03")));

        // no dashes, with colons
        dates.put("20220101T12:30:45", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("UTC")));
        dates.put("20220101T12:30:45Z", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("UTC")));
        dates.put("20220101T12:30:45+03", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("GMT+03")));
        dates.put("20220101T12:30:45+0300", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("GMT+03")));
        dates.put("20220101T12:30:45+03:00", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("GMT+03")));

        dates.put("20220101T12:30:45.123", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("UTC")));
        dates.put("20220101T12:30:45.123Z", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("UTC")));
        dates.put("20220101T12:30:45.123+03", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("GMT+03")));
        dates.put("20220101T12:30:45.123+0300", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("GMT+03")));
        dates.put("20220101T12:30:45.123+03:00", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("GMT+03")));

        // dashes and colons
        dates.put("2022-01-01T12:30:45", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("UTC")));
        dates.put("2022-01-01T12:30:45Z", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("UTC")));
        dates.put("2022-01-01T12:30:45+03", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("GMT+03")));
        dates.put("2022-01-01T12:30:45+0300", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("GMT+03")));
        dates.put("2022-01-01T12:30:45+03:00", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 0, TimeZone.getTimeZone("GMT+03")));

        dates.put("2022-01-01T12:30:45.123", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("UTC")));
        dates.put("2022-01-01T12:30:45.123Z", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("UTC")));
        dates.put("2022-01-01T12:30:45.123+03", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("GMT+03")));
        dates.put("2022-01-01T12:30:45.123+0300", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("GMT+03")));
        dates.put("2022-01-01T12:30:45.123+03:00", createDate(2022, Calendar.JANUARY, 1, 12, 30, 45, 123, TimeZone.getTimeZone("GMT+03")));


        return dates.entrySet().stream()
                .map(entry -> Arguments.of(entry.getKey(), entry.getValue()));
    }

    private static Date createDate(int year, int month, int day, int hour, int minute, int seconds, int milliseconds, TimeZone timeZone) {
        Calendar calendar = new GregorianCalendar(timeZone);
        calendar.set(year, month, day, hour, minute, seconds);
        calendar.set(Calendar.MILLISECOND, milliseconds);
        return calendar.getTime();
    }

    private static ObjectMapper createObjectMapperWithCustomDeserializer() {
        ObjectMapper mapper = new ObjectMapper();
        SimpleModule module = new SimpleModule();
        module.addDeserializer(Date.class, new CustomDateDeserializer());
        mapper.registerModule(module);
        return mapper;
    }

    @ParameterizedTest
    @MethodSource("provideDateStringsAndExpectedDates")
    public void shouldParseDateStringsCorrectlyWithCustomDeserializer(String dateString, Date expectedDate) throws Exception {
        ObjectMapper mapper = createObjectMapperWithCustomDeserializer();

        Date parsedDate = mapper.readValue("\"" + dateString + "\"", Date.class);

        SimpleDateFormat outputFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS Z");
        String parsedDateString = outputFormat.format(parsedDate);
        String expectedDateString = outputFormat.format(expectedDate);

        assertEquals(expectedDate, parsedDate,
                () -> "Failed to parse datetime value: " + dateString +
                        ". Parsed date: " + parsedDateString +
                        ", expected: " + expectedDateString);
    }
}
