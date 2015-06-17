hostname, username, password = "ivory", "root", "password"
 
import XenAPI
 
try:
    session=XenAPI.Session('https://'+hostname)
    session.login_with_password(username, password, '1.0' ,'xen-api-tutorial-snippet2.py')
except XenAPI.Failure, e:
    if e.details[0]=='HOST_IS_SLAVE':
        session=XenAPI.Session('https://'+e.details[1])
        session.login_with_password(username, password, '1.0' ,'xen-api-tutorial-snippet2.py')
    else:
        raise

sx=session.xenapi
