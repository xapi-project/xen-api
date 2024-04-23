type SessionRecord struct {
	// Unique identifier/object reference
	UUID string
	// Currently connected host
	ThisHost HostRef
}

type SessionRef string

// A session
type Session struct {
	APIVersion  APIVersion
	client      *rpcClient
	ref         SessionRef
	XAPIVersion string
}

func NewSession(opts *ClientOpts) *Session {
	client := newJSONRPCClient(opts)
	var session Session
	session.client = client

	return &session
}