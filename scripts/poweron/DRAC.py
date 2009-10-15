import subprocess, sys

def run2(command):
   run = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

   # Wait for the process to return
   out, err = [ e.splitlines() for e in run.communicate() ]

   return run.returncode, out, err

drac_path='/usr/sbin/racadm'
def DRAC( power_on_ip, user, password):
    cmd='%s -r %s -u %s -p %s serveraction powerup' % (drac_path, power_on_ip, user, password)
    retcode,out,err=run2(cmd)
    if(len(err)==0):
        return str(True)
    else:
        return str(False)
    


def main():
    if len(sys.argv)<3:
        exit(0)
    ip=sys.argv[1]
    user=sys.argv[2]
    password=sys.argv[3]
    print DRAC(ip,user,password)
   



if __name__ == "__main__":
    main()