func serializeOptionSrStatRecord(context string, input OptionSrStatRecord) (option interface{}, err error) {
	if input == nil {
		return
	}
	option, err = serializeSrStatRecord(context, *input)
	if err != nil {
		return
	}
	return
}

func deserializeOptionSrStatRecord(context string, input interface{}) (option OptionSrStatRecord, err error) {
	if input == nil {
		return
	}
	value, err := deserializeSrStatRecord(context, input)
	if err != nil {
		return
	}
	option = &value
	return
}