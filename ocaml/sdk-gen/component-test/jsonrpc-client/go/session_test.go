package componenttest

import (
	"encoding/json"
	"testing"
	"xenapi"
)

func TestLoginSuccess(t *testing.T) {
	data, err := ReadJsonFile("../../spec/xapi-24/session_login.json")
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

	type ResultBody struct {
		Result xenapi.HostRecord `json:"result,omitempty"`
	}

	type MethodResults struct {
		MethodResult1 map[string]interface{} `json:"session.login_with_password,omitempty"`
		MethodResult2 map[string]interface{} `json:"pool.get_all,omitempty"`
		MethodResult3 map[string]interface{} `json:"pool.get_record,omitempty"`
		MethodResult4 ResultBody             `json:"host.get_record,omitempty"`
		MethodResult5 map[string]interface{} `json:"session.logout,omitempty"`
	}

	type TestSpecBody struct {
		Method         []string                 `json:"method,omitempty"`
		Params         map[string][]interface{} `json:"params,omitempty"`
		ExpectedResult MethodResults            `json:"expected_result"`
	}

	type TestSpec struct {
		Key TestSpecBody `json:"xapi-24/session_login_01"`
	}

	var spec TestSpec
	if err := json.Unmarshal(data, &spec); err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	expectedResult := spec.Key.ExpectedResult.MethodResult4.Result

	session = xenapi.NewSession(&xenapi.ClientOpts{
		URL: ServerURL,
		Headers: map[string]string{
			"Test-ID": testId,
		},
	})

	inputParams := spec.Key.Params["session.login_with_password"]
	const (
		IndexVersion    = 2
		IndexOriginator = 3
	)

	version, ok1 := inputParams[IndexVersion].(string)
	originator, ok2 := inputParams[IndexOriginator].(string)
	if !ok1 || !ok2 {
		t.Log("Parameter get error from json file")
		t.Fail()
		return
	}
	_, err = session.LoginWithPassword(*USERNAME_FLAG, *PASSWORD_FLAG, version, originator)
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	expectedXapiVersion := expectedResult.SoftwareVersion["xapi"]
	getXapiVersion := session.XAPIVersion
	if expectedXapiVersion != getXapiVersion {
		t.Errorf("Unexpected result. Expected: %s, Got: %s", expectedXapiVersion, getXapiVersion)
	}
	expectedAPIVersion := xenapi.GetAPIVersion(expectedResult.APIVersionMajor, expectedResult.APIVersionMinor)
	getAPIVersion := session.APIVersion
	if expectedAPIVersion != getAPIVersion {
		t.Errorf("Unexpected result. Expected: %s, Got: %s", expectedAPIVersion, getAPIVersion)
	}

	err = session.Logout()
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}
}
