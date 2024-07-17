var timeFormats = []string{
	time.RFC3339,
	"2006-01-02T15:04:05",

	// no dashes, no colons
	"20060102T15:04:05Z",
	"20060102T15:04:05",
	"20060102T150405.999999999Z0700",
	"20060102T150405",
	"20060102T150405Z07",
	"20060102T150405Z07:00",

	// no dashes, with colons
	"20060102T15:04:05Z07",
	"20060102T15:04:05Z0700",
	"20060102T15:04:05Z07:00",
	"20060102T15:04:05.999999999Z07",
	"20060102T15:04:05.999999999Z07:00",
	"20060102T15:04:05.999999999Z07",

	// dashes and colon patterns not covered by `time.RFC3339`
	"2006-01-02T15:04:05Z07",
	"2006-01-02T15:04:05Z0700",
	"2006-01-02T15:04:05Z07:00",
	"2006-01-02T15:04:05.999999999Z07",
	"2006-01-02T15:04:05.999999999Z07:00",
	"2006-01-02T15:04:05.999999999Z07",
}

//nolint:unparam
func serializeTime(context string, value time.Time) (string, error) {
	_ = context
	return value.Format(time.RFC3339), nil
}

func deserializeTime(context string, input interface{}) (value time.Time, err error) {
	_ = context
	if input == nil {
		return
	}
	strValue := fmt.Sprintf("%v", input)
	floatValue, err := strconv.ParseFloat(strValue, 64)
	if err != nil {
		for _, timeFormat := range timeFormats {
			value, err = time.Parse(timeFormat, strValue)
			if err == nil {
				return value, nil
			}
		}
		return
	}
	unixTimestamp, err := strconv.ParseInt(strconv.Itoa(int(floatValue)), 10, 64)
	value = time.Unix(unixTimestamp, 0).UTC()

	return
}
