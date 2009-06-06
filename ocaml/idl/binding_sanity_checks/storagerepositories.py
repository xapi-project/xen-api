#!/usr/bin/env python

import XenAPI
import sanitychecklib 

session=sanitychecklib.getsession()
sx=session.xenapi

srs = sx.SR.get_all()

print "Server ", sanitychecklib.server, " has ", len(srs), "Storage Repositories whose names (& descriptions) are:"
print 
print "\n".join(["%s (%s)" % (sx.SR.get_name_label(sr), sx.SR.get_name_description(sr)) for sr in srs])
print
print 

hosts = sx.host.get_all()
if len(hosts)==1:
        print "There is a single host:",
else:
        print "There are :", len(hosts), " hosts, which are:"
        print

print "\n".join(["%s (%s)"%(sx.host.get_name_label(host),sx.host.get_name_description(host)) for host in hosts])
print
print

print "SRs in detail"
for sr in srs:
        print "\"%s\"" %sx.SR.get_name_label(sr),
        vdis=sx.SR.get_VDIs(sr)
        if len(vdis)>0 : print " contains VDIS:"
        else: print " :empty"
        for vdi in vdis:
            print "    \"%s\"" % sx.VDI.get_name_label(vdi),
            vbds = sx.VDI.get_VBDs(vdi)
            if len(vbds)>0 : print " contains VBDS"
            else: print
            for vbd in vbds:
                print "        device \"%s\" on vm \"%s\"" % (sx.VBD.get_device(vbd), sx.VM.get_name_label(sx.VBD.get_VM(vbd)))

session.logout()




















