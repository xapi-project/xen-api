func serializeVMRef(context string, ref VMRef) (string, error) {
	_ = context
	return string(ref), nil
}

func deserializeVMRef(context string, input interface{}) (VMRef, error) {
	var ref VMRef
	value, ok := input.(string)
	if !ok {
		return ref, fmt.Errorf("failed to parse XenAPI response: expected Go type %s at %s but got Go type %s with value %v", "string", context, reflect.TypeOf(input), input)
	}
	return VMRef(value), nil
}