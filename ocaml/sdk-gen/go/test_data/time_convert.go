var timeFormats = []string{time.RFC3339, "20060102T15:04:05Z", "20060102T15:04:05"}

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
	value = time.Unix(unixTimestamp, 0)

	return
}