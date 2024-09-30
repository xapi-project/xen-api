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

using System.Reflection;
using Newtonsoft.Json;
using XenAPI;
using Console = System.Console;

namespace XenServerTest;

internal class DateTimeObject
{
    [JsonConverter(typeof(XenDateTimeConverter))]
    public DateTime Date { get; set; }
}

[TestClass]
public class DateTimeTests
{
    private readonly JsonSerializerSettings _settings = new()
    {
        Converters = new List<JsonConverter> { new XenDateTimeConverter() }
    };

    [TestMethod]
    [DynamicData(nameof(GetTestData), DynamicDataSourceType.Method,
        DynamicDataDisplayName = nameof(GetCustomDynamicDataDisplayName))]
    public void TestXenDateTimeConverter(string dateString, DateTime expectedDateTime)
    {
        try
        {
            var jsonDateString = "{ \"Date\" : \"" + dateString + "\" }";
            var actualDateTime = JsonConvert.DeserializeObject<DateTimeObject>(jsonDateString, _settings);

            Assert.IsNotNull(actualDateTime, $"Failed to convert '{dateString}'");
            Assert.IsTrue(expectedDateTime.Equals(actualDateTime.Date),
                $"Conversion of '{dateString}' resulted in an incorrect DateTime value");
        }
        catch (Exception ex)
        {
            // Log the error or mark this specific data entry as failed
            Console.WriteLine($@"Error processing dateString '{dateString}': {ex.Message}");
            Assert.Fail($"An error occurred while processing '{dateString}'");
        }
    }

    public static string GetCustomDynamicDataDisplayName(MethodInfo methodInfo, object[] data)
    {
        return $"{methodInfo.Name}: '{data[0] as string}'";
    }

    public static IEnumerable<object[]> GetTestData()
    {
        // no dashes, no colons
        yield return new object[] { "20220101T123045", new DateTime(2022, 1, 1, 12, 30, 45, DateTimeKind.Unspecified) };
        yield return new object[] { "20220101T123045Z", new DateTime(2022, 1, 1, 12, 30, 45, DateTimeKind.Utc) };
        yield return new object[] { "20220101T123045+03", new DateTime(2022, 1, 1, 9, 30, 45, DateTimeKind.Local) };
        yield return new object[] { "20220101T123045+0300", new DateTime(2022, 1, 1, 9, 30, 45, DateTimeKind.Local) };
        yield return new object[] { "20220101T123045+03:00", new DateTime(2022, 1, 1, 9, 30, 45, DateTimeKind.Local) };

        yield return new object[]
            { "20220101T123045.123", new DateTime(2022, 1, 1, 12, 30, 45, 123, DateTimeKind.Unspecified) };
        yield return new object[]
            { "20220101T123045.123Z", new DateTime(2022, 1, 1, 12, 30, 45, 123, DateTimeKind.Utc) };
        yield return new object[]
            { "20220101T123045.123+03", new DateTime(2022, 1, 1, 9, 30, 45, 123, DateTimeKind.Local) };
        yield return new object[]
            { "20220101T123045.123+0300", new DateTime(2022, 1, 1, 9, 30, 45, 123, DateTimeKind.Local) };
        yield return new object[]
            { "20220101T123045.123+03:00", new DateTime(2022, 1, 1, 9, 30, 45, 123, DateTimeKind.Local) };

        // no dashes, with colons
        yield return new object[]
            { "20220101T12:30:45", new DateTime(2022, 1, 1, 12, 30, 45, DateTimeKind.Unspecified) };
        yield return new object[] { "20220101T12:30:45Z", new DateTime(2022, 1, 1, 12, 30, 45, DateTimeKind.Utc) };
        yield return new object[] { "20220101T12:30:45+03", new DateTime(2022, 1, 1, 9, 30, 45, DateTimeKind.Local) };
        yield return new object[] { "20220101T12:30:45+0300", new DateTime(2022, 1, 1, 9, 30, 45, DateTimeKind.Local) };
        yield return new object[]
            { "20220101T12:30:45+03:00", new DateTime(2022, 1, 1, 9, 30, 45, DateTimeKind.Local) };

        yield return new object[]
            { "20220101T12:30:45.123", new DateTime(2022, 1, 1, 12, 30, 45, 123, DateTimeKind.Unspecified) };
        yield return new object[]
            { "20220101T12:30:45.123Z", new DateTime(2022, 1, 1, 12, 30, 45, 123, DateTimeKind.Utc) };
        yield return new object[]
            { "20220101T12:30:45.123+03", new DateTime(2022, 1, 1, 9, 30, 45, 123, DateTimeKind.Local) };
        yield return new object[]
            { "20220101T12:30:45.123+0300", new DateTime(2022, 1, 1, 9, 30, 45, 123, DateTimeKind.Local) };
        yield return new object[]
            { "20220101T12:30:45.123+03:00", new DateTime(2022, 1, 1, 9, 30, 45, 123, DateTimeKind.Local) };

        // dashes and colons
        yield return new object[]
            { "2022-01-01T12:30:45", new DateTime(2022, 1, 1, 12, 30, 45, DateTimeKind.Unspecified) };
        yield return new object[] { "2022-01-01T12:30:45Z", new DateTime(2022, 1, 1, 12, 30, 45, DateTimeKind.Utc) };
        yield return new object[] { "2022-01-01T12:30:45+03", new DateTime(2022, 1, 1, 9, 30, 45, DateTimeKind.Local) };
        yield return new object[]
            { "2022-01-01T12:30:45+0300", new DateTime(2022, 1, 1, 9, 30, 45, DateTimeKind.Local) };
        yield return new object[]
            { "2022-01-01T12:30:45+03:00", new DateTime(2022, 1, 1, 9, 30, 45, DateTimeKind.Local) };

        yield return new object[]
            { "2022-01-01T12:30:45.123", new DateTime(2022, 1, 1, 12, 30, 45, 123, DateTimeKind.Unspecified) };
        yield return new object[]
            { "2022-01-01T12:30:45.123Z", new DateTime(2022, 1, 1, 12, 30, 45, 123, DateTimeKind.Utc) };
        yield return new object[]
            { "2022-01-01T12:30:45.123+03", new DateTime(2022, 1, 1, 9, 30, 45, 123, DateTimeKind.Local) };
        yield return new object[]
            { "2022-01-01T12:30:45.123+0300", new DateTime(2022, 1, 1, 9, 30, 45, 123, DateTimeKind.Local) };
        yield return new object[]
            { "2022-01-01T12:30:45.123+03:00", new DateTime(2022, 1, 1, 9, 30, 45, 123, DateTimeKind.Local) };
    }
}
