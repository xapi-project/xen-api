hostname, username, password = "ivory", "root", "password"
 
#usual boilerplate login

import XenAPI
session=XenAPI.Session('https://'+hostname)
session.login_with_password(username, password, '1.0' ,'xen-api-tutorial-snippet.py')
sx=session.xenapi
 
# partition the hosts according to whether they're alive or not
hosts=sx.host.get_all()
hosts_with_status=[(sx.host.get_name_label(x),sx.host_metrics.get_live( sx.host.get_metrics(x) )) for x in hosts]
live_hosts=[name for (name,status) in hosts_with_status if (status==True)]
dead_hosts=[name for (name,status) in hosts_with_status if not (status==True)]

 
#our one line of output
print "live hosts", live_hosts, "dead hosts", dead_hosts
#retcode is the value we should return to the system
retcode = 2 if (len(dead_hosts)<>0) else 0
