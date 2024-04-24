// GetLog: GetLog Get the host log file
func (host) GetLog(session *Session, host HostRef) (retval string, err error) {
	method := "host.get_log"
	sessionIDArg, err := serializeSessionRef(fmt.Sprintf("%s(%s)", method, "session_id"), session.ref)
	if err != nil {
		return
	}
	hostArg, err := serializeHostRef(fmt.Sprintf("%s(%s)", method, "host"), host)
	if err != nil {
		return
	}
	result, err := session.client.sentCall(method, sessionIDArg, hostArg)
	if err != nil {
		return
	}
	retval, err = deserializeString(method+" -> ", result)
	return
}

// AsyncGetLog: GetLog Get the host log file
func (host) AsyncGetLog(session *Session, host HostRef) (retval TaskRef, err error) {
	method := "Async.host.get_log"
	sessionIDArg, err := serializeSessionRef(fmt.Sprintf("%s(%s)", method, "session_id"), session.ref)
	if err != nil {
		return
	}
	hostArg, err := serializeHostRef(fmt.Sprintf("%s(%s)", method, "host"), host)
	if err != nil {
		return
	}
	result, err := session.client.sentCall(method, sessionIDArg, hostArg)
	if err != nil {
		return
	}
	retval, err = deserializeTaskRef(method+" -> ", result)
	return
}

// LoginWithPassword: Attempt to authenticate the user); returning a session reference if successful
//
// Errors:
// SESSION_AUTHENTICATION_FAILED - The credentials given by the user are incorrect
func (class *Session) LoginWithPassword(session *Session, session *Session) (retval SessionRef, err error) {
	method := "session.login_with_password"
	unameArg, err := serializeString(fmt.Sprintf("%s(%s)", method, "uname"), class.ref)
	if err != nil {
		return
	}
	pwdArg, err := serializeString(fmt.Sprintf("%s(%s)", method, "pwd"), class.ref)
	if err != nil {
		return
	}
	result, err := class.client.sentCall(method, unameArg, pwdArg)
	if err != nil {
		return
	}
	retval, err = deserializeSessionRef(method+" -> ", result)
	if err != nil {
		return
	}
	class.ref = retval
	err = setSessionDetails(class)
	return
}

// Logout: Logout Log out of a session
func (class *Session) Logout(session *Session) (err error) {
	method := "session.logout"
	sessionIDArg, err := serializeSessionRef(fmt.Sprintf("%s(%s)", method, "session_id"), class.ref)
	if err != nil {
		return
	}
	testParamArg, err := serializeString(fmt.Sprintf("%s(%s)", method, "test_param"), class.ref)
	if err != nil {
		return
	}
	_, err = class.client.sentCall(method, sessionIDArg, testParamArg)
	class.ref = ""
	return
}