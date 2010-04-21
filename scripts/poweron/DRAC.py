import subprocess, sys, os.path

class DRAC_NO_SUPP_PACK(Exception):
    """Base Exception class for all transfer plugin errors."""
    def __init__(self, *args):
        Exception.__init__(self, *args)
        
class DRAC_POWERON_FAILED(Exception):
    """Base Exception class for all transfer plugin errors."""
    def __init__(self, *args):
        Exception.__init__(self, *args)

def run2(command):
   run = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

   # Wait for the process to return
   out, err = [ e.splitlines() for e in run.communicate() ]

   return run.returncode, out, err

drac_path='/opt/dell/srvadmin/sbin/racadm'
def DRAC( power_on_ip, user, password):
    if( not os.path.exists(drac_path)):
        raise DRAC_NO_SUPP_PACK()
    cmd='%s -r %s -u %s -p %s serveraction powerup' % (drac_path, power_on_ip, user, password)
    retcode,out,err=run2(cmd)
    if(len(err)==0):
        return str(True)
    else:
        raise DRAC_POWERON_FAILED()
    


def main():
    if len(sys.argv)<3:
        exit(0)
    ip=sys.argv[1]
    user=sys.argv[2]
    password=sys.argv[3]
    print DRAC(ip,user,password)
   



if __name__ == "__main__":
    main()