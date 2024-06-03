package componenttest

import (
	"encoding/json"
	"math"
	"reflect"
	"testing"
	"time"
	"xenapi"
)

func TestVMGetAllRecords(t *testing.T) {
	data, err := ReadJsonFile("../../spec/xapi-24/vm_get_all_records.json")
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
		Result map[xenapi.VMRef]xenapi.VMRecord `json:"result,omitempty"`
	}

	type MethodResult struct {
		MethodName ResultBody `json:"VM.get_all_records,omitempty"`
	}

	type TestSpecBody struct {
		Method         []string               `json:"method,omitempty"`
		Params         map[string]interface{} `json:"params,omitempty"`
		ExpectedResult MethodResult           `json:"expected_result"`
	}

	type TestSpec struct {
		Key TestSpecBody `json:"xapi-24/vm_get_all_records_02"`
	}

	var spec TestSpec
	if err := json.Unmarshal(data, &spec); err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	expectedResult := spec.Key.ExpectedResult.MethodName.Result

	result, err := xenapi.VM.GetAllRecords(session)
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

func TestVMImport(t *testing.T) {
	data, err := ReadJsonFile("../../spec/xapi-24/vm_import.json")
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
		Result []xenapi.VMRef `json:"result,omitempty"`
	}

	type MethodResult struct {
		MethodName ResultBody `json:"VM.import,omitempty"`
	}

	type TestSpecBody struct {
		Method         []string                 `json:"method,omitempty"`
		Params         map[string][]interface{} `json:"params,omitempty"`
		ExpectedResult MethodResult             `json:"expected_result"`
	}

	type TestSpec struct {
		Key TestSpecBody `json:"xapi-24/vm_import_03"`
	}

	var spec TestSpec
	if err := json.Unmarshal(data, &spec); err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	expectedResult := spec.Key.ExpectedResult.MethodName.Result
	inputParams := spec.Key.Params["VM.import"]
	const (
		IndexVMImportURL = 1 //string
		IndexSrRef       = 2 //string
		IndexFullRestore = 3 //bool
		IndexForce       = 4 //bool
	)
	VMImportURL, ok1 := inputParams[IndexVMImportURL].(string)
	srRef, ok2 := inputParams[IndexSrRef].(string)
	fullRestore, ok3 := inputParams[IndexFullRestore].(bool)
	force, ok4 := inputParams[IndexForce].(bool)
	if !ok1 || !ok2 || !ok3 || !ok4 {
		t.Log("Parameter get error from json file")
		t.Fail()
		return
	}

	result, err := xenapi.VM.Import(session, VMImportURL, xenapi.SRRef(srRef), fullRestore, force)
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

func TestVMRetrieveWlbRecommendations(t *testing.T) {
	data, err := ReadJsonFile("../../spec/xapi-24/vm_retrieve_wlb_recommendations.json")
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
		Result map[xenapi.HostRef][]string `json:"result,omitempty"`
	}

	type MethodResult struct {
		MethodName ResultBody `json:"VM.retrieve_wlb_recommendations,omitempty"`
	}

	type TestSpecBody struct {
		Method         []string                 `json:"method,omitempty"`
		Params         map[string][]interface{} `json:"params,omitempty"`
		ExpectedResult MethodResult             `json:"expected_result"`
	}

	type TestSpec struct {
		Key TestSpecBody `json:"xapi-24/vm_retrieve_wlb_recommendations_04"`
	}

	var spec TestSpec
	if err := json.Unmarshal(data, &spec); err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	expectedResult := spec.Key.ExpectedResult.MethodName.Result

	inputParams := spec.Key.Params["VM.retrieve_wlb_recommendations"]
	vmRef, ok := inputParams[1].(string)
	if !ok {
		t.Log("Parameter get error from json file")
		t.Fail()
		return
	}

	result, err := xenapi.VM.RetrieveWlbRecommendations(session, xenapi.VMRef(vmRef))
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

func TestVMMigrateSend(t *testing.T) {
	data, err := ReadJsonFile("../../spec/xapi-24/vm_migrate_send.json")
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
		Result xenapi.VMRef `json:"result,omitempty"`
	}

	type MethodResult struct {
		MethodName ResultBody `json:"VM.migrate_send,omitempty"`
	}

	type TestSpecBody struct {
		Method         []string                 `json:"method,omitempty"`
		Params         map[string][]interface{} `json:"params,omitempty"`
		ExpectedResult MethodResult             `json:"expected_result"`
	}

	type TestSpec struct {
		Key TestSpecBody `json:"xapi-24/vm_migrate_send_05"`
	}

	var spec TestSpec
	if err := json.Unmarshal(data, &spec); err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	expectedResult := spec.Key.ExpectedResult.MethodName.Result

	inputParams := spec.Key.Params["VM.migrate_send"]
	const (
		IndexVMRef   = 1
		IndexDest    = 2
		IndexLive    = 3
		IndexVdiMap  = 4
		IndexVifMap  = 5
		IndexOptions = 6
		IndexVgpuMap = 7
	)
	vmRef, ok1 := inputParams[IndexVMRef].(string)
	destOrg, ok2 := inputParams[IndexDest].(map[string]interface{})
	live, ok3 := inputParams[IndexLive].(bool)
	vdiMapOrg, ok4 := inputParams[IndexVdiMap].(map[string]interface{})
	vifMapOrg, ok5 := inputParams[IndexVifMap].(map[string]interface{})
	optionsOrg, ok6 := inputParams[IndexOptions].(map[string]interface{})
	vgpuMapOrg, ok7 := inputParams[IndexVgpuMap].(map[string]interface{})
	if !ok1 || !ok2 || !ok3 || !ok4 || !ok5 || !ok6 || !ok7 {
		t.Log("Parameter get error from json file")
		t.Fail()
		return
	}
	dest, err := ConvertMapToStringMap(destOrg)
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}
	vdiMap, err := ConvertMapToVdiMap(vdiMapOrg)
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}
	vifMap, err := ConvertMapToVifMap(vifMapOrg)
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}
	options, err := ConvertMapToStringMap(optionsOrg)
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}
	vgpuMap, err := ConvertMapToVgpuMap(vgpuMapOrg)
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	result, err := xenapi.VM.MigrateSend(session, xenapi.VMRef(vmRef), dest, live, vdiMap, vifMap, options, vgpuMap)
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	if result != expectedResult {
		t.Log("The result returned not the same with expected -> The XAPI outcome diverges from the anticipated value.")
		t.Fail()
		return
	}
}

func TestVMGetDataSources(t *testing.T) {
	data, err := ReadJsonFile("../../spec/xapi-24/vm_get_data_sources.json")
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
		Result []xenapi.DataSourceRecord `json:"result,omitempty"`
	}

	type MethodResult struct {
		MethodName ResultBody `json:"VM.get_data_sources,omitempty"`
	}

	type TestSpecBody struct {
		Method         []string                 `json:"method,omitempty"`
		Params         map[string][]interface{} `json:"params,omitempty"`
		ExpectedResult MethodResult             `json:"expected_result"`
	}

	type TestSpec struct {
		Key TestSpecBody `json:"xapi-24/vm_get_data_sources_06"`
	}

	var spec TestSpec
	if err := json.Unmarshal(data, &spec); err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	expectedResult := spec.Key.ExpectedResult.MethodName.Result

	inputParams := spec.Key.Params["VM.get_data_sources"]
	vmRef, ok := inputParams[1].(string)
	if !ok {
		t.Log("Parameter get error from json file")
		t.Fail()
		return
	}

	result, err := xenapi.VM.GetDataSources(session, xenapi.VMRef(vmRef))
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

func TestVMGetSnapshotTime(t *testing.T) {
	data, err := ReadJsonFile("../../spec/xapi-24/vm_get_snapshot_time.json")
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
		Result time.Time `json:"result,omitempty"`
	}

	type MethodResult struct {
		MethodName ResultBody `json:"VM.get_snapshot_time,omitempty"`
	}

	type TestSpecBody struct {
		Method         []string                 `json:"method,omitempty"`
		Params         map[string][]interface{} `json:"params,omitempty"`
		ExpectedResult MethodResult             `json:"expected_result"`
	}

	type TestSpec struct {
		Key TestSpecBody `json:"xapi-24/vm_get_snapshot_time_07"`
	}

	var spec TestSpec
	if err := json.Unmarshal(data, &spec); err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	expectedResult := spec.Key.ExpectedResult.MethodName.Result

	inputParams := spec.Key.Params["VM.get_snapshot_time"]
	vmRef, ok := inputParams[1].(string)
	if !ok {
		t.Log("Parameter get error from json file")
		t.Fail()
		return
	}
	result, err := xenapi.VM.GetSnapshotTime(session, xenapi.VMRef(vmRef))

	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	if result != expectedResult {
		t.Log("The result returned not the same with expected -> The XAPI outcome diverges from the anticipated value.")
		t.Fail()
		return
	}
}

func TestVMQueryDataSource(t *testing.T) {
	data, err := ReadJsonFile("../../spec/xapi-24/vm_query_data_source.json")
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

	var vmRef xenapi.VMRef = "OpaqueRef:6ef08bce-0bf0-30ff-804f-5f0ee4bbdd13"

	result, err := xenapi.VM.QueryDataSource(session, vmRef, "CPU0 usage")
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}
	if !math.IsNaN(result) {
		t.Log("The result returned not the same with expected -> The XAPI outcome diverges from the anticipated value.")
		t.Fail()
		return
	}
}

func TestVMAsyncImport(t *testing.T) {
	data, err := ReadJsonFile("../../spec/xapi-24/async_vm_import.json")
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
		Result xenapi.TaskRef `json:"result,omitempty"`
	}

	type MethodResult struct {
		MethodName ResultBody `json:"Async.VM.import"`
	}

	type TestSpecBody struct {
		Method         []string                 `json:"method,omitempty"`
		Params         map[string][]interface{} `json:"params,omitempty"`
		ExpectedResult MethodResult             `json:"expected_result"`
	}

	type TestSpec struct {
		Key TestSpecBody `json:"xapi-24/async_vm_import_12"`
	}

	var spec TestSpec
	if err := json.Unmarshal(data, &spec); err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	expectedResult := spec.Key.ExpectedResult.MethodName.Result

	inputParams := spec.Key.Params["Async.VM.import"]
	const (
		IndexVMImportURL = 1 //string
		IndexSrRef       = 2 //string
		IndexFullRestore = 3 //bool
		IndexForce       = 4 //bool
	)
	VMImportURL, ok1 := inputParams[IndexVMImportURL].(string)
	srRef, ok2 := inputParams[IndexSrRef].(string)
	fullRestore, ok3 := inputParams[IndexFullRestore].(bool)
	force, ok4 := inputParams[IndexForce].(bool)
	if !ok1 || !ok2 || !ok3 || !ok4 {
		t.Log("Parameter get error from json file")
		t.Fail()
		return
	}

	result, err := xenapi.VM.AsyncImport(session, VMImportURL, xenapi.SRRef(srRef), fullRestore, force)
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	if result != expectedResult {
		t.Log("The expected error is not the same with the returned one!")
		t.Fail()
		return
	}
}

func TestVMSpecialValue(t *testing.T) {
	session, err := GetSession("xapi-24/vm_special_value_13")
	if err != nil {
		t.Log(err)
		t.Fail()
		return
	}

	var vmRecord0 = xenapi.VMRecord{
		HVMShadowMultiplier: math.Inf(1),
		SnapshotTime:        time.Date(2021, time.July, 28, 13, 20, 00, 0, time.UTC),
	}
	var vmRecord1 = xenapi.VMRecord{
		HVMShadowMultiplier: math.Inf(1),
		SnapshotTime:        time.Date(2024, time.April, 20, 12, 00, 00, 0, time.UTC),
	}
	var vmRecord2 = xenapi.VMRecord{
		HVMShadowMultiplier: math.Inf(-1),
		SnapshotTime:        time.Date(2024, time.April, 20, 12, 00, 00, 0, time.UTC),
	}
	var vmRecord3 = xenapi.VMRecord{
		HVMShadowMultiplier: math.Inf(-1),
		SnapshotTime:        time.Date(2024, time.April, 20, 12, 00, 00, 0, time.UTC),
	}
	var expectedResult = map[xenapi.VMRef]xenapi.VMRecord{
		"OpaqueRef:vmref009-7e0e-411f-1b6c-4308fd33b164": vmRecord0,
		"OpaqueRef:vmref010-7e0e-411f-1b6c-4308fd33b164": vmRecord1,
		"OpaqueRef:vmref011-7e0e-411f-1b6c-4308fd33b164": vmRecord2,
		"OpaqueRef:vmref012-7e0e-411f-1b6c-4308fd33b164": vmRecord3,
	}

	result, err := xenapi.VM.GetAllRecords(session)
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
