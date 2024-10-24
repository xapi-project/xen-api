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

package xenapi_test

import (
	"testing"
	"time"

	"go/xenapi"
)

func TestDateDeseralization(t *testing.T) {
	dates := map[string]time.Time{
		// no dashes, no colons
		"20220101T123045":       time.Date(2022, 1, 1, 12, 30, 45, 0, time.Local),
		"20220101T123045Z":      time.Date(2022, 1, 1, 12, 30, 45, 0, time.UTC),
		"20220101T123045+03":    time.Date(2022, 1, 1, 12, 30, 45, 0, time.FixedZone("", 3*60*60)), // +03 timezone
		"20220101T123045+0300":  time.Date(2022, 1, 1, 12, 30, 45, 0, time.FixedZone("", 3*60*60)),
		"20220101T123045+03:00": time.Date(2022, 1, 1, 12, 30, 45, 0, time.FixedZone("", 3*60*60)),

		"20220101T123045.123":       time.Date(2022, 1, 1, 12, 30, 45, 123000000, time.Local),
		"20220101T123045.123Z":      time.Date(2022, 1, 1, 12, 30, 45, 123000000, time.UTC),
		"20220101T123045.123+03":    time.Date(2022, 1, 1, 12, 30, 45, 123000000, time.FixedZone("", 3*60*60)),
		"20220101T123045.123+0300":  time.Date(2022, 1, 1, 12, 30, 45, 123000000, time.FixedZone("", 3*60*60)),
		"20220101T123045.123+03:00": time.Date(2022, 1, 1, 12, 30, 45, 123000000, time.FixedZone("", 3*60*60)),

		// no dashes, with colons
		"20220101T12:30:45":       time.Date(2022, 1, 1, 12, 30, 45, 0, time.Local),
		"20220101T12:30:45Z":      time.Date(2022, 1, 1, 12, 30, 45, 0, time.UTC),
		"20220101T12:30:45+03":    time.Date(2022, 1, 1, 12, 30, 45, 0, time.FixedZone("", 3*60*60)),
		"20220101T12:30:45+0300":  time.Date(2022, 1, 1, 12, 30, 45, 0, time.FixedZone("", 3*60*60)),
		"20220101T12:30:45+03:00": time.Date(2022, 1, 1, 12, 30, 45, 0, time.FixedZone("", 3*60*60)),

		"20220101T12:30:45.123":       time.Date(2022, 1, 1, 12, 30, 45, 123000000, time.Local),
		"20220101T12:30:45.123Z":      time.Date(2022, 1, 1, 12, 30, 45, 123000000, time.UTC),
		"20220101T12:30:45.123+03":    time.Date(2022, 1, 1, 12, 30, 45, 123000000, time.FixedZone("", 3*60*60)),
		"20220101T12:30:45.123+0300":  time.Date(2022, 1, 1, 12, 30, 45, 123000000, time.FixedZone("", 3*60*60)),
		"20220101T12:30:45.123+03:00": time.Date(2022, 1, 1, 12, 30, 45, 123000000, time.FixedZone("", 3*60*60)),

		// dashes and colons
		"2022-01-01T12:30:45":       time.Date(2022, 1, 1, 12, 30, 45, 0, time.Local),
		"2022-01-01T12:30:45Z":      time.Date(2022, 1, 1, 12, 30, 45, 0, time.UTC),
		"2022-01-01T12:30:45+03":    time.Date(2022, 1, 1, 12, 30, 45, 0, time.FixedZone("", 3*60*60)),
		"2022-01-01T12:30:45+0300":  time.Date(2022, 1, 1, 12, 30, 45, 0, time.FixedZone("", 3*60*60)),
		"2022-01-01T12:30:45+03:00": time.Date(2022, 1, 1, 12, 30, 45, 0, time.FixedZone("", 3*60*60)),

		"2022-01-01T12:30:45.123":    time.Date(2022, 1, 1, 12, 30, 45, 123000000, time.Local),
		"2022-01-01T12:30:45.123Z":   time.Date(2022, 1, 1, 12, 30, 45, 123000000, time.UTC),
		"2022-01-01T12:30:45.123+03": time.Date(2022, 1, 1, 12, 30, 45, 123000000, time.FixedZone("", 3*60*60)),
	}
	for input, expected := range dates {
		t.Run("Input:"+input, func(t *testing.T) {
			result, err := xenapi.DeserializeTime("", input)
			if err == nil {
				matching := expected.Equal(result)
				if !matching {
					t.Fatalf(`Failed to find match for '%s'`, input)
				}
			} else {
				t.Fatalf(`Failed to find match for '%s'`, input)
			}
		})
	}
}
