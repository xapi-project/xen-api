hostname, username, password = "ivory", "root", "password"
 
import XenAPI
 
try:
    session=XenAPI.Session('https://'+hostname)
    session.login_with_password(username, password)
except XenAPI.Failure, e:
    if e.details[0]=='HOST_IS_SLAVE':
        session=XenAPI.Session('https://'+e.details[1])
        session.login_with_password(username, password)
    else:
        raise

sx=session.xenapi
