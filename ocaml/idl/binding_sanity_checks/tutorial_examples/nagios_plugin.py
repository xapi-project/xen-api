#!/usr/bin/python

#This is an example plugin for the popular network monitoring program nagios.

#Check if all the hosts in a pool are live.
#If we log in to a slave by mistake (the master can sometimes change)
#then redirect the request to the real master

#example command line: ./check_pool.py -H ivory -p password -l root

#So: return codes
# 0 : everything is ok
# 1 : named host is slave, but all hosts in pool are up
# 2 : some of the hosts in the pool are down
# 3 : unexpected error

#entire program wrapped in try/except so that we can send exit code 3 to nagios on any error
try:

    import XenAPI
    import sys

    from optparse import OptionParser

    #Parse command line options
    #Python's standard option parser won't do what I want, so I'm subclassing it.
    #firstly, nagios wants exit code three if the options are bad
    #secondly, we want 'required options', which the option parser thinks is an oxymoron.
    #I on the other hand don't want to give defaults for the host and password, because nagios is difficult to set up correctly,
    #and the effect of that may be to hide a problem.
    class MyOptionParser(OptionParser):
        def error(self,msg):
            print msg
            sys.exit(3)
        #stolen from python library reference, add required option check
        def check_required(self, opt):
            option=self.get_option(opt)
            if getattr(self.values, option.dest) is None:
                self.error("%s option not supplied" % option)

    parser = MyOptionParser(description="Nagios plugin to check whether all hosts in a pool are live")

    parser.add_option("-H", "--hostname", dest="hostname", help="name of pool master")
    parser.add_option("-l", "--login-name", default="root", dest="username", help="name to log in as (usually root)")
    parser.add_option("-p", "--password", dest="password", help="password")

    (options, args) = parser.parse_args()

    #abort if host and password weren't specified explicitly on the command line
    parser.check_required("-H")
    parser.check_required("-p")


    #get a session. set host_is_slave true if we need to redirect to a new master
    host_is_slave=False
    try:
        session=XenAPI.Session('https://'+options.hostname)
        session.login_with_password(options.username, options.password,, '1.0' ,'xen-api-tutorial-plugin.py')
    except XenAPI.Failure, e:
        if e.details[0]=='HOST_IS_SLAVE':
            session=XenAPI.Session('https://'+e.details[1])
            session.login_with_password(options.username, options.password, '1.0' ,'xen-api-tutorial-plugin.py')
            host_is_slave=True
        else:
            raise
    sx=session.xenapi

    #work out which hosts in the pool are alive, and which dead
    hosts=sx.host.get_all()
    hosts_with_status=[(sx.host.get_name_label(x),sx.host_metrics.get_live( sx.host.get_metrics(x) )) for x in hosts]

    live_hosts=[name for (name,status) in hosts_with_status if (status==True)]
    dead_hosts=[name for (name,status) in hosts_with_status if not (status==True)]


    #log out
    session.logout()

    #nagios wants a single line of output
    print "live hosts", live_hosts, "dead hosts", dead_hosts,
    if host_is_slave:
        print "(%s is not the master)" % options.hostname,
    print

    #and an exit code
    if (len(dead_hosts)<>0):
        exitcode=2
    elif host_is_slave:
        exitcode=1
    else:
        exitcode=0

except Exception, e:
    print "Unexpected Exception [", e.__repr__(), "]"
    sys.exit(3) #Nagios wants error 3 if anything weird happens

sys.exit(exitcode)
