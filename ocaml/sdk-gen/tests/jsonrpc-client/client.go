package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
)

const ServerURL = "http://jsonrpc-server:5000"

type Request struct {
	Jsonrpc string      `json:"jsonrpc"`
	Method  string      `json:"method"`
	Params  interface{} `json:"params"`
	ID      int         `json:"id"`
}

func sendJsonRpcRequest(method string, params ...map[string]string) {
    var p map[string]string
    if len(params) > 0 {
        p = params[0]
    } else {
        p = map[string]string{}
    }

	requestBody, _ := json.Marshal(Request{
		Jsonrpc: "2.0",
		Method:  method,
		Params:  p,
		ID:      1,
	})

	resp, err := http.Post(ServerURL, "application/json", bytes.NewBuffer(requestBody))
	if err != nil {
		fmt.Printf("Failed to send JSON-RPC request: %v\n", err)
		return
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Printf("Failed to read response body: %v\n", err)
		return
	}

	fmt.Println(string(body))
}

func testSessions() {
	sendJsonRpcRequest("login_session", map[string]string{"username": "admin", "password": "secret"})
	sendJsonRpcRequest("logout_session")
}

func main() {
	testSessions()
}