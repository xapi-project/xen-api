package componenttest

import (
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"testing"
	"xenapi"
)

func TestRpcError(t *testing.T) {
	//Constuct an rpc client error
	data, err := ReadJsonFile("../../spec/xapi-24/rpc_error.json")
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
		Result map[string]interface{} `json:"result,omitempty"`
		Error  map[string]interface{} `json:"error,omitempty"`
	}

	type MethodResult struct {
		MethodName ResultBody `json:"VM.get_record,omitempty"`
	}

	type TestSpecBody struct {
		Method         []string                 `json:"method,omitempty"`
		Params         map[string][]interface{} `json:"params,omitempty"`
		ExpectedResult MethodResult             `json:"expected_result"`
	}

	type TestSpec struct {
		Key TestSpecBody `json:"xapi-24/rpc_error_09"`
	}

	var spec TestSpec
	if err := json.Unmarshal(data, &spec); err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	testMethod := "VM.get_record"
	vmRef := spec.Key.Params[testMethod][1].(string)
	_, err = xenapi.VM.GetRecord(session, xenapi.VMRef(vmRef))
	if err == nil {
		t.Log(err)
		t.Fail()
		return
	}

	errorString := "call " + testMethod + "() on " + ServerURL + "/jsonrpc status code: 200. Could not decode response body: json: cannot unmarshal string into Go struct field ResponseError.error.code of type int"
	var rpcError = errors.New(errorString)
	if err.Error() != rpcError.Error() {
		t.Log("The expected error is not the same with the returned one!")
		t.Fail()
		return
	}
}

func TestHttpError(t *testing.T) {
	// Construct an error in RPC response that returned for method/parameter check not as expected
	data, err := ReadJsonFile("../../spec/xapi-24/http_error.json")
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

	//Use a VMRef which is inconsist with in json file to trigger server check error then return HTTP Error
	var vmRef xenapi.VMRef = "OpaqueRef:6ef08bce-0bf0-30ff-804f-5f0ee4bbdd13"
	var httpError = errors.New("API error: code 500, message Rpc server failed to handle the client request!")

	_, err = xenapi.VM.GetRecord(session, vmRef)
	if err == nil {
		t.Log("Expected to be error, but no error is detected")
		t.Fail()
		return
	}

	if !strings.Contains(err.Error(), httpError.Error()) {
		t.Log("The expected error is not the same with the returned one!")
		t.Fail()
		return
	}
}

func TestXapiError(t *testing.T) {
	// Construct an error that returned from Xapi
	data, err := ReadJsonFile("../../spec/xapi-24/xapi_error.json")
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
		Result map[string]interface{} `json:"result,omitempty"`
		Error  map[string]interface{} `json:"error,omitempty"`
	}

	type MethodResult struct {
		MethodName ResultBody `json:"VM.get_record,omitempty"`
	}

	type TestSpecBody struct {
		Method         []string                 `json:"method,omitempty"`
		Params         map[string][]interface{} `json:"params,omitempty"`
		ExpectedResult MethodResult             `json:"expected_result"`
	}

	type TestSpec struct {
		Key TestSpecBody `json:"xapi-24/xapi_error_13"`
	}

	var spec TestSpec
	if err := json.Unmarshal(data, &spec); err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	inputParams := spec.Key.Params["VM.get_record"]
	vmRef := inputParams[1].(string)

	expectedError := spec.Key.ExpectedResult.MethodName.Error
	errorCode := expectedError["code"].(float64)
	errorMessage := expectedError["message"].(string)
	errorData := expectedError["data"].(string)
	xapiError := errors.New("API error: code " + fmt.Sprint(errorCode) + ", message " + errorMessage + ", data " + errorData)

	_, err = xenapi.VM.GetRecord(session, xenapi.VMRef(vmRef))
	if err == nil {
		t.Log("Expected to be error, but no error is detected")
		t.Fail()
		return
	}

	if !strings.Contains(err.Error(), xapiError.Error()) {
		t.Log("The expected error is not the same with the returned one!")
		t.Fail()
		return
	}
}
