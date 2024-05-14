// LoginWithPassword: Attempt to authenticate the user); returning a session reference if successful
// Version: miami
//
// Errors:
// SESSION_AUTHENTICATION_FAILED - The credentials given by the user are incorrect
func (class *Session) LoginWithPassword(uname string, pwd string) (retval SessionRef, err error) {
	method := "session.login_with_password"
	unameArg, err := serializeString(fmt.Sprintf("%s(%s)", method, "uname"), uname)
	if err != nil {
		return
	}
	pwdArg, err := serializeString(fmt.Sprintf("%s(%s)", method, "pwd"), pwd)
	if err != nil {
		return
	}
	result, err := class.client.sendCall(method, unameArg, pwdArg)
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
// Version: miami
func (class *Session) Logout() (err error) {
	method := "session.logout"
	sessionIDArg, err := serializeSessionRef(fmt.Sprintf("%s(%s)", method, "session_id"), class.ref)
	if err != nil {
		return
	}
	_, err = class.client.sendCall(method, sessionIDArg)
	class.ref = ""
	return
}