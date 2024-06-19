func deserializeEventBatch(context string, input interface{}) (batch EventBatch, err error) {
	rpcStruct, ok := input.(map[string]interface{})
	if !ok {
		err = fmt.Errorf("failed to parse XenAPI response: expected Go type %s at %s but got Go type %s with value %v", "map[string]interface{}", context, reflect.TypeOf(input), input)
		return
	}
	tokenValue, ok := rpcStruct["token"]
	if ok && tokenValue != nil {
		batch.Token, err = deserializeString(fmt.Sprintf("%s.%s", context, "token"), tokenValue)
		if err != nil {
			return
		}
	}
	validRefCountsValue, ok := rpcStruct["validRefCounts"]
	if ok && validRefCountsValue != nil {
		batch.ValidRefCounts, err = deserializeStringToIntMap(fmt.Sprintf("%s.%s", context, "validRefCounts"), validRefCountsValue)
		if err != nil {
			return
		}
	}
	eventsValue, ok := rpcStruct["events"]
	if ok && eventsValue != nil {
		batch.Events, err = deserializeEventRecordSet(fmt.Sprintf("%s.%s", context, "events"), eventsValue)
		if err != nil {
			return
		}
	}
	return
}