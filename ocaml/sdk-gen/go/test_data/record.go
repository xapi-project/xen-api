type SessionRecord struct {
	// Unique identifier/object reference
	UUID string
	// Currently connected host
	ThisHost HostRef
}

type SessionRef string

// A session
type SessionClass struct {
	client *rpcClient
	ref    SessionRef
}

func NewSession(opts *ClientOpts) *SessionClass {
	client := NewJsonRPCClient(opts)
	var session SessionClass
	session.client = client

	return &session
}