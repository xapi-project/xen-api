func serializeString(context string, value string) (string, error) {
	_ = context
	return value, nil
}

func serializeBool(context string, value bool) (bool, error) {
	_ = context
	return value, nil
}

func deserializeString(context string, input interface{}) (value string, err error) {
	if input == nil {
		return
	}
	value, ok := input.(string)
	if !ok {
		err = fmt.Errorf("failed to parse XenAPI response: expected Go type %s at %s but got Go type %s with value %v", "string", context, reflect.TypeOf(input), input)
	}
	return
}

func deserializeBool(context string, input interface{}) (value bool, err error) {
	if input == nil {
		return
	}
	value, ok := input.(bool)
	if !ok {
		err = fmt.Errorf("failed to parse XenAPI response: expected Go type %s at %s but got Go type %s with value %v", "bool", context, reflect.TypeOf(input), input)
	}
	return
}