func serializeSRRefSet(context string, slice []SRRef) (set []interface{}, err error) {
	set = make([]interface{}, len(slice))
	for index, item := range slice {
		itemContext := fmt.Sprintf("%s[%d]", context, index)
		itemValue, err := serializeSRRef(itemContext, item)
		if err != nil {
			return set, err
		}
		set[index] = itemValue
	}
	return
}

func deserializeStringSet(context string, input interface{}) (slice []string, err error) {
	set, ok := input.([]interface{})
	if !ok {
		err = fmt.Errorf("failed to parse XenAPI response: expected Go type %s at %s but got Go type %s with value %v", "[]interface{}", context, reflect.TypeOf(input), input)
		return
	}
	slice = make([]string, len(set))
	for index, item := range set {
		itemContext := fmt.Sprintf("%s[%d]", context, index)
		itemValue, err := deserializeString(itemContext, item)
		if err != nil {
			return slice, err
		}
		slice[index] = itemValue
	}
	return
}