package componenttest

import (
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"testing"
	"xenapi"
)

const ServerURL = "http://localhost:5000"

var session *xenapi.Session

var USERNAME_FLAG = flag.String("root", "", "the username of the host (e.g. root)")
var PASSWORD_FLAG = flag.String("secret", "", "the password of the host")

func GetSession(testId string) (*xenapi.Session, error) {
	var newSession *xenapi.Session
	newSession = xenapi.NewSession(&xenapi.ClientOpts{
		URL: ServerURL,
		Headers: map[string]string{
			"Test-ID": testId,
		},
	})

	return newSession, nil
}

func ReadJsonFile(filePath string) ([]byte, error) {
	dataByte, err := os.ReadFile(filePath)
	if err != nil {
		fmt.Println(err)
		return nil, err
	}

	return dataByte, nil
}

func GetTestId(data []byte) (string, error) {
	var result map[string]interface{}
	if err := json.Unmarshal(data, &result); err != nil {
		return "", err
	}

	var firstKey string
	for key := range result {
		firstKey = key
		break
	}

	return firstKey, nil
}

func ConvertInterfaceSliceToStringSlice(input []interface{}) ([]string, error) {
	var output []string
	for _, value := range input {
		strValue, ok := value.(string)
		if !ok {
			return nil, fmt.Errorf("non-string value found: %v", value)
		}
		output = append(output, strValue)
	}
	return output, nil
}

func ConvertMapToStringMap(input map[string]interface{}) (map[string]string, error) {
	output := make(map[string]string)
	for key, value := range input {
		strValue, ok := value.(string)
		if !ok {
			return nil, fmt.Errorf("non-string value found for key %s: %v", key, value)
		}
		output[key] = strValue
	}
	return output, nil
}

func ConvertMapToVdiMap(input map[string]interface{}) (map[xenapi.VDIRef]xenapi.SRRef, error) {
	output := make(map[xenapi.VDIRef]xenapi.SRRef)
	for key, value := range input {
		strValue, ok := value.(string)
		if !ok {
			return nil, fmt.Errorf("non-string value found for key %s: %v", key, value)
		}
		output[xenapi.VDIRef(key)] = xenapi.SRRef(strValue)
	}
	return output, nil
}

func ConvertMapToVifMap(input map[string]interface{}) (map[xenapi.VIFRef]xenapi.NetworkRef, error) {
	output := make(map[xenapi.VIFRef]xenapi.NetworkRef)
	for key, value := range input {
		strValue, ok := value.(string)
		if !ok {
			return nil, fmt.Errorf("non-string value found for key %s: %v", key, value)
		}
		output[xenapi.VIFRef(key)] = xenapi.NetworkRef(strValue)
	}
	return output, nil
}

func ConvertMapToVgpuMap(input map[string]interface{}) (map[xenapi.VGPURef]xenapi.GPUGroupRef, error) {
	output := make(map[xenapi.VGPURef]xenapi.GPUGroupRef)
	for key, value := range input {
		strValue, ok := value.(string)
		if !ok {
			return nil, fmt.Errorf("non-string value found for key %s: %v", key, value)
		}
		output[xenapi.VGPURef(key)] = xenapi.GPUGroupRef(strValue)
	}
	return output, nil
}

func TestMain(m *testing.M) {
	flag.Parse()
	exitVal := m.Run()
	os.Exit(exitVal)
}
