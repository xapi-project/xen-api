func serializeInt(context string, value int) (int, error) {
	_ = context
	return value, nil
}

func deserializeInt(context string, input interface{}) (value int, err error) {
	_ = context
	if input == nil {
		return
	}
	strValue := fmt.Sprintf("%v", input)
	value, err = strconv.Atoi(strValue)
	if err != nil {
		floatValue, err1 := strconv.ParseFloat(strValue, 64)
		if err1 == nil {
			return int(floatValue), nil
		}
	}
	return
}