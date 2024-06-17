func serializeVIFRefToStringMap(context string, goMap map[VIFRef]string) (xenMap map[string]interface{}, err error) {
	xenMap = make(map[string]interface{})
	for goKey, goValue := range goMap {
		keyContext := fmt.Sprintf("%s[%s]", context, goKey)
		xenKey, err := serializeVIFRef(keyContext, goKey)
		if err != nil {
			return xenMap, err
		}
		xenValue, err := serializeString(keyContext, goValue)
		if err != nil {
			return xenMap, err
		}
		xenMap[xenKey] = xenValue
	}
	return
}

func deserializePBDRefToPBDRecordMap(context string, input interface{}) (goMap map[PBDRef]PBDRecord, err error) {
	xenMap, ok := input.(map[string]interface{})
	if !ok {
		err = fmt.Errorf("failed to parse XenAPI response: expected Go type %s at %s but got Go type %s with value %v", "map[string]interface{}", context, reflect.TypeOf(input), input)
		return
	}
	goMap = make(map[PBDRef]PBDRecord, len(xenMap))
	for xenKey, xenValue := range xenMap {
		keyContext := fmt.Sprintf("%s[%s]", context, xenKey)
		goKey, err := deserializePBDRef(keyContext, xenKey)
		if err != nil {
			return goMap, err
		}
		goValue, err := deserializePBDRecord(keyContext, xenValue)
		if err != nil {
			return goMap, err
		}
		goMap[goKey] = goValue
	}
	return
}