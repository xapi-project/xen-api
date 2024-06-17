//nolint:unparam
func serializeFloat(context string, value float64) (interface{}, error) {
	_ = context
	if math.IsInf(value, 0) {
		if math.IsInf(value, 1) {
			return "+Inf", nil
		}
		return "-Inf", nil
	} else if math.IsNaN(value) {
		return "NaN", nil
	}
	return value, nil
}

func deserializeFloat(context string, input interface{}) (value float64, err error) {
	_ = context
	if input == nil {
		return
	}
	strValue := fmt.Sprintf("%v", input)
	value, err = strconv.ParseFloat(strValue, 64)
	if err != nil {
		switch strValue {
		case "+Inf":
			return math.Inf(1), nil
		case "-Inf":
			return math.Inf(-1), nil
		case "NaN":
			return math.NaN(), nil
		}
	}
	return
}