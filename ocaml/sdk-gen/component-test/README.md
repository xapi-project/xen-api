## How Component Testing Works

#### jsonrpc-server
The jsonrpc-server is a mock HTTP server created to emulate the XAPI (eXtensible Application Programming Interface) environment. It operates by executing the method specified within incoming requests and subsequently providing a response that includes the outcome of that execution. To facilitate comprehensive testing, the server extracts test data from JSON files located in the spec/ directory. To enhance the organization and scalability of test cases, especially when simulating various API versions for the purpose of ensuring backwards compatibility, the following structure is recommended:

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
- Test Case Files: Each test case is represented by a JSON file. These files should be named uniquely, often corresponding to a test ID, to avoid conflicts and to make it easier to reference specific test cases.

- JSON Object Structure: Within each JSON file, the test data should be structured to include essential fields such as test_id, method, params, and expect_result. This structure allows for clear definition and expectation of each test case.
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
- Test Execution: The jsonrpc-server should be designed to iterate through the spec/ directory and its sub-directories, executing each test case and comparing the actual results against the expect_result as defined in the JSON files.

- Compatiblity: When organizing test cases for multiple API versions, ensure that the sub-directory structure reflects these versions. This allows for running version-specific tests or comprehensive tests that cover multiple versions.

- Maintenance and Evolution: As the API evolves, the test cases should be updated or extended to reflect new methods, parameters, or expected results. The directory structure should facilitate easy updates and additions to the test cases.

#### jsonrpc-client

jsonrpc-client is a client that imports the SDK and runs the functions, following these important details:

1. Add test_id as the user agent in the request header.

2. Ensure that the function and params are aligned with the data defined in spec/ directory.

3. The test results/reports need to be collected from the client side.

#### github actions
For a ci step in the generate sdk sources job, it should involve performing lint, component testing. For instance, in the case of the go sdk, there is a docker compose which launches a jsonrpc server and client and executes the testing procedure in the go-ct step.


## Run test locally
Install python 3.11+ with requirements and go 1.22+ and go to ocaml/sdk-gen/component-test and run `bash run-tests.sh`
