## How Component Testing Works

#### jsonrpc-server
The jsonrpc-server is a mock HTTP server which aligns with [jsonrpc 2.0 specification](https://www.jsonrpc.org/specification) and to emulate the xen-api server. It parses the test data from JSON files located in the spec/ directory, executes the method and parameters specified in spec files with incoming requests and then sends back a jsonrpc response that includes the expect_result.

For the purpose of backwards and forwards compatibility, the following structure is recommended:

- Base Directory: All test cases should be housed within a dedicated directory, conventionally named spec/.

- Sub-directories: As the number of test cases grows or when differentiating between API versions, it becomes advantageous to categorize them into sub-directories. These sub-directories can represent different versions or modules of the API.
```
spec/
├── v1/
│   ├── test_id_1.json
│   ├── test_id_2.json
│   └── ...
├── v2/
│   ├── test_id_3.json
│   ├── test_id_4.json
│   └── ...
└── ...
```

- Test Data Specification: Within each JSON file, the test data should be structured to include essential fields such as test_id, method, params, and expect_result. This structure allows for clear definition and expectation of each test case.
```json
{
      "test_id": "test_id_1",
        "method": "methodName",
        "params": {
                // ... parameters required for the method

        },
        "expect_result": {
                // ... expected result of the method execution

        }

}
```

#### jsonrpc-client
jsonrpc-client is a client that imports the SDK and runs the functions, following these important details:

1. Single test case in single test spec file
 
2. Add test_id as a customize request header.

3. Ensure that the function and params are aligned with the data defined in spec/ directory.

4. In order to support test reports, practitioners should use the specific test framework to test SDK, eg: pytest, gotest, junit, xUnit and so on.

5. To support the SDK component test, it recommended to move the SDK generated to a sub directory as a local module for import purposes, eg:
```
cp -r ${{ github.workspace }}/_build/install/default/share/go/src jsonrpc-client/go/goSDK
```
then, import the local module.
```
module github.com/xapi-project/xen-api/sdk-gen/component-test/jsonrpc-client/go

go 1.22.2

replace xenapi => ./goSDK
```

#### github actions
For a CI step in the generate sdk sources job, it should involve performing lint and component testing after sdk generation.

## Run test locally
Install python 3.11+ with requirements and go 1.22+ and go to ocaml/sdk-gen/component-test and run `bash run-tests.sh`
