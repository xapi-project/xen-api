package xenapi

import (
	"bytes"
	"context"
	"crypto/tls"
	"crypto/x509"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log"
	"net/http"
	"net/url"
	"os"
	"strings"
	"time"
)

type Request struct {
	JSONRPC string      `json:"jsonrpc"`
	Method  string      `json:"method"`
	Params  interface{} `json:"params,omitempty"`
	ID      int         `json:"id"`
}

type ResponseError struct {
	Code    int         `json:"code"`
	Message string      `json:"message"`
	Data    interface{} `json:"data,omitempty"`
}

type Response struct {
	JSONRPC string         `json:"jsonrpc"`
	Result  interface{}    `json:"result,omitempty"`
	Error   *ResponseError `json:"error,omitempty"`
	ID      int            `json:"id"`
}

type rpcClient struct {
	endpoint   string
	httpClient *http.Client
	headers    map[string]string
}

func (client *rpcClient) newRequest(ctx context.Context, req interface{}) (*http.Request, error) {
	dataByte, err := json.Marshal(req)
	if err != nil {
		return nil, fmt.Errorf("error marshaling request: %w", err)
	}

	request, err := http.NewRequestWithContext(ctx, http.MethodPost, client.endpoint, bytes.NewReader(dataByte))
	if err != nil {
		return nil, fmt.Errorf("error creating request: %w", err)
	}

	request.Header.Set("Content-Type", "application/json; charset=utf-8")
	request.Header.Set("Accept", "application/json")
	request.Header.Set("User-Agent", "XenAPI/"+APIVersionLatest.String())
	for k, v := range client.headers {
		request.Header.Set(k, v)
	}

	return request, nil
}

func convertUnhandledJSONData(jsonBytes []byte) []byte {
	jsonString := string(jsonBytes)
	jsonString = strings.ReplaceAll(jsonString, ":Infinity", ":\"+Inf\"")
	jsonString = strings.ReplaceAll(jsonString, ":-Infinity", ":\"-Inf\"")
	jsonString = strings.ReplaceAll(jsonString, ":NaN", ":\"NaN\"")

	return []byte(jsonString)
}

func (client *rpcClient) call(ctx context.Context, methodName string, params ...interface{}) (*Response, error) {
	request := &Request{
		ID:      0,
		Method:  methodName,
		Params:  params,
		JSONRPC: "2.0",
	}

	httpRequest, err := client.newRequest(ctx, request)
	if err != nil {
		return nil, fmt.Errorf("could not create request for %v() : %w", request.Method, err)
	}

	httpResponse, err := client.httpClient.Do(httpRequest)
	if err != nil {
		return nil, fmt.Errorf("call %v() on %v. Error making http request: %w", request.Method, httpRequest.URL.Redacted(), err)
	}
	defer httpResponse.Body.Close()

	var rpcResponse *Response
	body, err := io.ReadAll(httpResponse.Body)
	if err != nil {
		return nil, fmt.Errorf("call %v() on %v status code: %v. Could not read response body: %w", request.Method, httpRequest.URL.Redacted(), httpResponse.StatusCode, err)
	}
	body = convertUnhandledJSONData(body)
	err = json.Unmarshal(body, &rpcResponse)
	if err != nil && !errors.Is(err, io.EOF) {
		return nil, fmt.Errorf("call %v() on %v status code: %v. Could not decode response body: %w", request.Method, httpRequest.URL.Redacted(), httpResponse.StatusCode, err)
	}

	if rpcResponse == nil {
		return nil, fmt.Errorf("call %v() on %v status code: %v. Response missing", request.Method, httpRequest.URL.Redacted(), httpResponse.StatusCode)
	}

	return rpcResponse, nil
}

func (client *rpcClient) sendCall(methodName string, params ...interface{}) (result interface{}, err error) {
	response, err := client.call(context.Background(), methodName, params...)
	if err != nil {
		return
	}

	if response.Error != nil {
		errString := fmt.Sprintf("API error: code %d, message %s", response.Error.Code, response.Error.Message)
		if response.Error.Data != nil {
			errString += fmt.Sprintf(", data %v", response.Error.Data)
		}
		err = errors.New(errString)
		return
	}

	result = response.Result
	return
}

type SecureOpts struct {
	ServerCert string
	ClientCert string
	ClientKey  string
}

type ClientOpts struct {
	URL        string
	SecureOpts *SecureOpts
	Timeout    int
	Headers    map[string]string
}

func newJSONRPCClient(opts *ClientOpts) *rpcClient {
	client := &rpcClient{
		endpoint:   fmt.Sprintf("%s%s", opts.URL, "/jsonrpc"),
		httpClient: &http.Client{},
		headers:    make(map[string]string),
	}

	u, err := url.Parse(opts.URL)
	if err != nil {
		log.Fatal(err)
	}
	if strings.Compare(u.Scheme, "https") == 0 {
		skipVerify := true
		caCertPool := x509.NewCertPool()
		certs := []tls.Certificate{}
		if opts.SecureOpts != nil {
			if opts.SecureOpts.ServerCert != "" {
				skipVerify = false
				caCert, err := os.ReadFile(opts.SecureOpts.ServerCert)
				if err != nil {
					log.Fatal(err)
				}
				ok := caCertPool.AppendCertsFromPEM(caCert)
				if !ok {
					log.Fatal("failed to parse CA certificate")
				}
			}
			if opts.SecureOpts.ClientCert != "" || opts.SecureOpts.ClientKey != "" {
				if opts.SecureOpts.ClientCert == "" {
					log.Fatal("missing client certificate")
				}
				if opts.SecureOpts.ClientKey == "" {
					log.Fatal("missing client private key")
				}
				cert, err := tls.LoadX509KeyPair(opts.SecureOpts.ClientCert, opts.SecureOpts.ClientKey)
				if err != nil {
					log.Fatal(err)
				}
				certs = []tls.Certificate{cert}
			}
		}
		tlsConfig := &tls.Config{
			RootCAs:            caCertPool,
			Certificates:       certs,
			InsecureSkipVerify: skipVerify, // #nosec
			CipherSuites: []uint16{
				tls.TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384,
				tls.TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256,
			},
			MinVersion:               tls.VersionTLS12,
			PreferServerCipherSuites: true,
		}
		transport := &http.Transport{
			TLSClientConfig: tlsConfig,
		}
		client.httpClient.Transport = transport
	}

	if opts.Timeout != 0 {
		client.httpClient.Timeout = time.Duration(opts.Timeout) * time.Second
	}

	if opts.Headers != nil {
		for k, v := range opts.Headers {
			client.headers[k] = v
		}
	}

	return client
}
