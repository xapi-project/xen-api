package componenttest

import (
	"testing"
	"time"

	"xenapi"
)

func TestDateDeserialization(t *testing.T) {
	utc := time.UTC
	plus3 := time.FixedZone("", 3*60*60)
	vmRef := xenapi.VMRef("OpaqueRef:datetime")

	cases := []struct {
		testID string
		want   time.Time
	}{
		// no dashes, no colons
		{"xapi-24/datetime_01", time.Date(2022, 1, 1, 12, 30, 45, 0, utc)},
		{"xapi-24/datetime_02", time.Date(2022, 1, 1, 12, 30, 45, 0, utc)},
		{"xapi-24/datetime_03", time.Date(2022, 1, 1, 12, 30, 45, 0, plus3)},
		{"xapi-24/datetime_04", time.Date(2022, 1, 1, 12, 30, 45, 0, plus3)},
		{"xapi-24/datetime_05", time.Date(2022, 1, 1, 12, 30, 45, 0, plus3)},
		{"xapi-24/datetime_06", time.Date(2022, 1, 1, 12, 30, 45, 123000000, utc)},
		{"xapi-24/datetime_07", time.Date(2022, 1, 1, 12, 30, 45, 123000000, utc)},
		{"xapi-24/datetime_08", time.Date(2022, 1, 1, 12, 30, 45, 123000000, plus3)},
		{"xapi-24/datetime_09", time.Date(2022, 1, 1, 12, 30, 45, 123000000, plus3)},
		{"xapi-24/datetime_10", time.Date(2022, 1, 1, 12, 30, 45, 123000000, plus3)},
		// no dashes, with colons
		{"xapi-24/datetime_11", time.Date(2022, 1, 1, 12, 30, 45, 0, utc)},
		{"xapi-24/datetime_12", time.Date(2022, 1, 1, 12, 30, 45, 0, utc)},
		{"xapi-24/datetime_13", time.Date(2022, 1, 1, 12, 30, 45, 0, plus3)},
		{"xapi-24/datetime_14", time.Date(2022, 1, 1, 12, 30, 45, 0, plus3)},
		{"xapi-24/datetime_15", time.Date(2022, 1, 1, 12, 30, 45, 0, plus3)},
		{"xapi-24/datetime_16", time.Date(2022, 1, 1, 12, 30, 45, 123000000, utc)},
		{"xapi-24/datetime_17", time.Date(2022, 1, 1, 12, 30, 45, 123000000, utc)},
		{"xapi-24/datetime_18", time.Date(2022, 1, 1, 12, 30, 45, 123000000, plus3)},
		{"xapi-24/datetime_19", time.Date(2022, 1, 1, 12, 30, 45, 123000000, plus3)},
		{"xapi-24/datetime_20", time.Date(2022, 1, 1, 12, 30, 45, 123000000, plus3)},
		// dashes and colons
		{"xapi-24/datetime_21", time.Date(2022, 1, 1, 12, 30, 45, 0, utc)},
		{"xapi-24/datetime_22", time.Date(2022, 1, 1, 12, 30, 45, 0, utc)},
		{"xapi-24/datetime_23", time.Date(2022, 1, 1, 12, 30, 45, 0, plus3)},
		{"xapi-24/datetime_24", time.Date(2022, 1, 1, 12, 30, 45, 0, plus3)},
		{"xapi-24/datetime_25", time.Date(2022, 1, 1, 12, 30, 45, 0, plus3)},
		{"xapi-24/datetime_26", time.Date(2022, 1, 1, 12, 30, 45, 123000000, utc)},
		{"xapi-24/datetime_27", time.Date(2022, 1, 1, 12, 30, 45, 123000000, utc)},
		{"xapi-24/datetime_28", time.Date(2022, 1, 1, 12, 30, 45, 123000000, plus3)},
	}

	for _, c := range cases {
		t.Run(c.testID, func(t *testing.T) {
			session, err := GetSession(c.testID)
			if err != nil {
				t.Fatalf("GetSession(%s): %v", c.testID, err)
			}
			got, err := xenapi.VM.GetSnapshotTime(session, vmRef)
			if err != nil {
				t.Fatalf("GetSnapshotTime(%s): %v", c.testID, err)
			}
			if !c.want.Equal(got) {
				t.Fatalf("%s: expected %v, got %v", c.testID, c.want, got)
			}
		})
	}
}
