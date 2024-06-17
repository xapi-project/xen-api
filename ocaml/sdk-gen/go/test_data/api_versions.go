type APIVersion int

const (
	// XenServer 4.0 (rio)
	APIVersion1_1 APIVersion = iota + 1
	// XenServer 4.1 (miami)
	APIVersion1_2
	APIVersionLatest  APIVersion = 2
	APIVersionUnknown APIVersion = 99
)

func (v APIVersion) String() string {
	switch v {
	case APIVersion1_1:
		return "1.1"
	case APIVersion1_2:
		return "1.2"
	case APIVersionUnknown:
		return "Unknown"
	default:
		return "Unknown"
	}
}

var APIVersionMap = map[string]APIVersion{
	//
	"APIVersion1_1": APIVersion1_1,
	//
	"APIVersion1_2": APIVersion1_2,
	//
	"APIVersionLatest": APIVersionLatest,
	//
	"APIVersionUnknown": APIVersionUnknown,
}

func GetAPIVersion(major int, minor int) APIVersion {
	versionName := fmt.Sprintf("APIVersion%d_%d", major, minor)
	apiVersion, ok := APIVersionMap[versionName]
	if !ok {
		apiVersion = APIVersionUnknown
	}

	return apiVersion
}

func getPoolMaster(session *Session) (HostRef, error) {
	var master HostRef
	poolRefs, err := Pool.GetAll(session)
	if err != nil {
		return master, err
	}
	if len(poolRefs) > 0 {
		poolRecord, err := Pool.GetRecord(session, poolRefs[0])
		if err != nil {
			return master, err
		}
		return poolRecord.Master, nil
	}
	return master, errors.New("pool master not found")
}

func setSessionDetails(session *Session) error {
	err := setAPIVersion(session)
	if err != nil {
		return err
	}
	err = setXAPIVersion(session)
	if err != nil {
		return err
	}
	return nil
}

func setAPIVersion(session *Session) error {
	session.APIVersion = APIVersionUnknown
	masterRef, err := getPoolMaster(session)
	if err != nil {
		return err
	}
	hostRecord, err := Host.GetRecord(session, masterRef)
	if err != nil {
		return err
	}
	session.APIVersion = GetAPIVersion(hostRecord.APIVersionMajor, hostRecord.APIVersionMinor)
	return nil
}

func setXAPIVersion(session *Session) error {
	masterRef, err := getPoolMaster(session)
	if err != nil {
		return err
	}
	hostRecord, err := Host.GetRecord(session, masterRef)
	if err != nil {
		return err
	}
	version, ok := hostRecord.SoftwareVersion["xapi"]
	if !ok {
		return errors.New("xapi version not found")
	}
	session.XAPIVersion = version
	return nil
}