func serializeInt(context string, value int64) (int64, error) {
	_ = context
	return value, nil
}

func deserializeInt(context string, input interface{}) (value int64, err error) {
	_ = context
	if input == nil {
		return
	}
	strValue := fmt.Sprintf("%v", input)
	value, err = strconv.ParseInt(strValue, 10, 64)
	if err != nil {
		floatValue, err1 := strconv.ParseFloat(strValue, 64)
		if err1 == nil {
			return int64(floatValue), nil
		}
	}
	return
}