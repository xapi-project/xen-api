package componenttest

import (
	"encoding/json"
	"reflect"
	"testing"
	"xenapi"
)

func TestEventFrom(t *testing.T) {
	data, err := ReadJsonFile("../../spec/xapi-24/event_from.json")
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	testId, err := GetTestId(data)
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}
	session, err := GetSession(testId)
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	type ResultBody struct {
		Result xenapi.EventBatch `json:"result,omitempty"`
	}

	type MethodResult struct {
		MethodName ResultBody `json:"event.from,omitempty"`
	}

	type TestSpecBody struct {
		Method         []string                 `json:"method,omitempty"`
		Params         map[string][]interface{} `json:"params,omitempty"`
		ExpectedResult MethodResult             `json:"expected_result"`
	}

	type TestSpec struct {
		Key TestSpecBody `json:"xapi-24/event_from_11"`
	}

	var spec TestSpec
	if err := json.Unmarshal(data, &spec); err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	expectedResult := spec.Key.ExpectedResult.MethodName.Result

	inputParams := spec.Key.Params["event.from"]
	const (
		IndexClasses = 1
		IndexToken   = 2
		IndexTimeout = 3
	)
	classesInterfaceSlice, ok1 := inputParams[IndexClasses].([]interface{})
	token, ok2 := inputParams[IndexToken].(string)
	timeout, ok3 := inputParams[IndexTimeout].(float64)
	if !ok1 || !ok2 || !ok3 {
		t.Log("Parameter get error from json file")
		t.Fail()
		return
	}
	classes, err := ConvertInterfaceSliceToStringSlice(classesInterfaceSlice)
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	result, err := xenapi.Event.From(session, classes, token, timeout)
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	if !reflect.DeepEqual(result, expectedResult) {
		t.Log("The result returned not the same with expected -> The XAPI outcome diverges from the anticipated value.")
		t.Fail()
		return
	}
}
