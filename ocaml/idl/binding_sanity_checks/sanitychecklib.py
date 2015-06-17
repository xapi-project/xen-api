import XenAPI

server='ivory'
username='root'
password='password'

secondaryserver='ebony'
secondaryusername=username
secondarypassword=password

rioserver='inflames'
riousername=username
riopassword=password

network_storage_server='lork.uk.xensource.com'
network_storage_path  ='/nfs1/jaspden'

test_vm_name='sanitycheck VM'

def gs(server, username, password):
    session = XenAPI.Session('http://'+server)
    session.login_with_password(username, password, '1.0', 'xen-api-sanity-check')
    return session

def getsession():
    return gs(server, username, password)

def getsecondarysession():
    return gs(secondaryserver, secondaryusername, secondarypassword)

