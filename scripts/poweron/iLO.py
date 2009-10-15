import sys,M2Crypto 

def getXmlWithLogin(user, password):
    
    inputFile=open("/etc/xapi.d/plugins/iLOPowerON.xml",'r')
    try:
        result= inputFile.read().replace('user',user).replace('password',password)
    finally:
        inputFile.close()
    return result


def iLO(power_on_ip, user, password):
  
        xmlWithlogin=getXmlWithLogin(user,password)+'\r\n'      
    
        ''' Send and receive '''
        ctx = M2Crypto.SSL.Context()
        ctx.set_session_timeout(500)
        s = M2Crypto.SSL.Connection(ctx)
        totalmsg=''
        try:
            s.connect((power_on_ip,443))
            written=s.sendall(xmlWithlogin)
            msg=s.read()
            totalmsg=msg
            while(len(msg)):
                msg=s.read()
                totalmsg+=msg
        finally:
            s.close()
            '''Check that the server replies with no authentication error'''
            if len(totalmsg)>0 and totalmsg.find('STATUS="0x000A"')==-1:
                return str(True)
            else:
                return str(False)
    
def main():
    if len(sys.argv)<3:
        exit(0)
    ip=sys.argv[1]
    user=sys.argv[2]
    password=sys.argv[3]
    print iLO(ip,user,password)
   



if __name__ == "__main__":
    main()