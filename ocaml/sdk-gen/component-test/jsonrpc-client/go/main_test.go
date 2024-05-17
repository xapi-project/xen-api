package main

import (
	"flag"
	"fmt"
	"testing"
	"xenapi"
)

const ServerURL = "http://localhost:5000"

var session *xenapi.Session

var USERNAME_FLAG = flag.String("root", "", "the username of the host (e.g. root)")
var PASSWORD_FLAG = flag.String("secret", "", "the password of the host")

func TestLoginSuccess(t *testing.T) {
	session = xenapi.NewSession(&xenapi.ClientOpts{
		URL: ServerURL,
		Headers: map[string]string{
			"Test-ID": "test_id1",
		},
	})
	if session == nil {
		fmt.Printf("Failed to get the session")
		return
	}
	_, err := session.LoginWithPassword(*USERNAME_FLAG, *PASSWORD_FLAG, "1.0", "Go sdk component test")
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	expectedXapiVersion := "1.20"
	getXapiVersion := session.XAPIVersion
	if expectedXapiVersion != getXapiVersion {
		t.Errorf("Unexpected result. Expected: %s, Got: %s", expectedXapiVersion, getXapiVersion)
	}
	var expectedAPIVersion xenapi.APIVersion = xenapi.APIVersion2_21
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
