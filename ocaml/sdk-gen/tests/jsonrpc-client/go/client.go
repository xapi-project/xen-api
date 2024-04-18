package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"strings"
	"strconv"
)

const ServerURL = "http://jsonrpc-server:5000"

type Request struct {
	Jsonrpc string      `json:"jsonrpc"`
	Method  string      `json:"method"`
	Params  interface{} `json:"params"`
	ID      int         `json:"id"`
}

func sendJsonRpcRequest(test_id string, method string, params ...map[string]string) {
    var p map[string]string
    if len(params) > 0 {
        p = params[0]
    } else {
        p = map[string]string{}
    }

	idStr := strings.TrimPrefix(test_id, "test_id")
	id, err := strconv.Atoi(idStr)
	if err != nil {
		fmt.Printf("Failed to parse id: %v\n", err)
		return
	}

	requestBody, _ := json.Marshal(Request{
		Jsonrpc: "2.0",
		Method:  method,
		Params:  p,
		ID:      id,
	})

    req, err := http.NewRequest("POST", ServerURL, bytes.NewBuffer(requestBody))
    if err != nil {
        fmt.Printf("Failed to create request: %v\n", err)
        return
    }

    req.Header.Set("Content-Type", "application/json")
    req.Header.Set("User-Agent", test_id)

    client := &http.Client{}
    resp, err := client.Do(req)
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
	sendJsonRpcRequest("test_id1", "login_session", map[string]string{"username": "admin", "password": "secret"})
	sendJsonRpcRequest("test_id2", "login_session", map[string]string{"username": "admin", "password": "invalid"})
	sendJsonRpcRequest("test_id3", "logout_session")
}

func main() {
	testSessions()
}
