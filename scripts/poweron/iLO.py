import sys,M2Crypto, XenAPI, XenAPIPlugin


class ILO_CONNECTION_ERROR(Exception):
    """Base Exception class for all transfer plugin errors."""
    def __init__(self, *args):
        Exception.__init__(self, *args)
		
class ILO_POWERON_FAILED(Exception):
    """Base Exception class for all transfer plugin errors."""
    def __init__(self, *args):
        Exception.__init__(self, *args)


def getXmlWithLogin(user, password):
    
    inputFile=open("@PLUGINDIR@/iLOPowerON.xml",'r')
    try:
        result= inputFile.read().replace('user',user).replace('password',password)
    finally:
        inputFile.close()
    return result


def iLO(power_on_ip, user, password):
	xmlWithlogin=getXmlWithLogin(user,password)+'\r\n'      
	
	''' Send and receive '''
	# "tlsv1" means v1.0 only, not 1.1, 1.2 etc.
	# It would be nice to specify all protocols ("sslv23") and then use
	# options to disable sslv2 and sslv3, but such a Context fails to
	# connect to the iLO server if the cipher_list is specified (even
	# though the same kind of Context can connect to a XenServer).
	ctx = M2Crypto.SSL.Context("tlsv1")
	# Setting options just in case.
	ctx.set_options(
		M2Crypto.m2.SSL_OP_NO_SSLv2 |
		M2Crypto.m2.SSL_OP_NO_SSLv3
	)
	ctx.set_cipher_list("!SSLv2:RSA+AES128-SHA256:RSA+AES256-SHA:RSA+AES128-SHA:RSA+RC4-SHA")
	ctx.set_session_timeout(500)
	s = M2Crypto.SSL.Connection(ctx)
	s.set_post_connection_check_callback(None)
	totalmsg=''
	try:
		s.connect((power_on_ip,443))
		written=s.sendall(xmlWithlogin)
		msg=s.read()
		totalmsg=msg
		while(len(msg)):
			msg=s.read()
			totalmsg+=msg
	except:
		s.close()
		raise ILO_CONNECTION_ERROR()
	'''Check that the server replies with no authentication error'''
	if len(totalmsg)>0 and totalmsg.find('STATUS="0x000A"')==-1:
		return str(True)
	else:
		raise ILO_POWERON_FAILED()
    
def main():
    if len(sys.argv)<3:
        exit(0)
    ip=sys.argv[1]
    user=sys.argv[2]
    password=sys.argv[3]
    print iLO(ip,user,password)
   



if __name__ == "__main__":
    main()
