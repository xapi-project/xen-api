func serializeEnumTaskStatusType(context string, value TaskStatusType) (string, error) {
	_ = context
	return string(value), nil
}

func deserializeEnumTaskStatusType(context string, input interface{}) (value TaskStatusType, err error) {
	strValue, err := deserializeString(context, input)
	if err != nil {
		return
	}
	switch strValue {
	case "pending":
		value = TaskStatusTypePending
	case "success":
		value = TaskStatusTypeSuccess
	default:
		err = fmt.Errorf("unable to parse XenAPI response: got value %q for enum %s at %s, but this is not any of the known values", strValue, "TaskStatusType", context)
	}
	return
}