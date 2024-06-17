func serializeVBDRecord(context string, record VBDRecord) (rpcStruct map[string]interface{}, err error) {
	rpcStruct = map[string]interface{}{}
	rpcStruct["uuid"], err = serializeString(fmt.Sprintf("%s.%s", context, "uuid"), record.UUID)
	if err != nil {
		return
	}
	rpcStruct["allowed_operations"], err = serializeEnumVbdOperationsSet(fmt.Sprintf("%s.%s", context, "allowed_operations"), record.AllowedOperations)
	if err != nil {
		return
	}
	return
}

func deserializeVBDRecord(context string, input interface{}) (record VBDRecord, err error) {
	rpcStruct, ok := input.(map[string]interface{})
	if !ok {
		err = fmt.Errorf("failed to parse XenAPI response: expected Go type %s at %s but got Go type %s with value %v", "map[string]interface{}", context, reflect.TypeOf(input), input)
		return
	}
	uuidValue, ok := rpcStruct["uuid"]
	if ok && uuidValue != nil {
		record.UUID, err = deserializeString(fmt.Sprintf("%s.%s", context, "uuid"), uuidValue)
		if err != nil {
			return
		}
	}
	allowedOperationsValue, ok := rpcStruct["allowed_operations"]
	if ok && allowedOperationsValue != nil {
		record.AllowedOperations, err = deserializeEnumVbdOperationsSet(fmt.Sprintf("%s.%s", context, "allowed_operations"), allowedOperationsValue)
		if err != nil {
			return
		}
	}
	return
}