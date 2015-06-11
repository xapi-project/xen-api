#!/usr/bin/env python

import XenAPI
import sys,time


def format_list_of_floats(lof):
    return "["+", ".join(["%.2f" % x for x in lof])+"]"


def print_metrics(sx):
    for host in sx.host.get_all():
        print "host %s" % sx.host.get_name_label(host),
        print format_list_of_floats([sx.host_cpu.get_utilisation(cpu) for cpu in sx.host.get_host_CPUs(host)])


        for vm in sx.host.get_resident_VMs(host):
            print "  VM %s" % sx.VM.get_name_label(vm),
            print format_list_of_floats(sx.VM_metrics.get_VCPUs_utilisation(sx.VM.get_metrics(vm)).values())

def main(url, username, password):
    while True:
        session = XenAPI.Session(url)
        session.login_with_password(username, password, '1.0' ,'xen-api-tutorial-cpu-use.py'
        print_metrics(session.xenapi)
        session.logout()
        time.sleep(5)

if __name__ == "__main__":
    if   len(sys.argv) == 4:
        main(*sys.argv[1:])
    elif len(sys.argv) == 1:
        main("http://ivory", "root", "password")
    else:
        print "Usage:"
        print sys.argv[0], " <url> <username> <password>"
        sys.exit(1)
