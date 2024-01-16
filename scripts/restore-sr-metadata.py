#!/usr/bin/python
# Restore SR metadata and VDI names from an XML file
# (c) Anil Madhavapeddy, Citrix Systems Inc, 2008

# pytype: disable=pyi-error
import XenAPI
import os, sys, time
import getopt
from xml.dom.minidom import parse

import codecs

sys.stdout = codecs.getwriter("utf-8")(sys.stdout)
sys.stderr = codecs.getwriter("utf-8")(sys.stderr)


def usage():
    print("%s -f <input file> -u <sr uuid>" % sys.argv[0], file=sys.stderr)
    sys.exit(1)

def main(argv):
    session = XenAPI.xapi_local()
    session.xenapi.login_with_password("", "", "1.0", "xen-api-scripts-restore-sr-metadata")

    try:
        opts, args = getopt.getopt(argv, "hf:u:", [])
    except getopt.GetoptError as err:
        print(str(err))
        usage()

    infile = None
    sruuid = None
    for o,a in opts:
        if o == "-f":
            infile = a
        if o == "-u":
            sruuid = a

    if infile == None:
        usage()

    try:
        doc = parse(infile)
    except:
        print("Error parsing %s" % infile, file=sys.stderr)
        sys.exit(1)

    if doc.documentElement.tagName != "meta":
        print("Unexpected root element while parsing %s" % infile, file=sys.stderr)
        sys.exit(1)
    
    for srxml in doc.documentElement.childNodes:
        try:
            uuid = srxml.getAttribute("uuid")
            name_label = srxml.getAttribute("name_label")
            name_descr = srxml.getAttribute("name_description")
        except:
            print("Error parsing SR tag", file=sys.stderr)
            continue
        # only set attributes on the selected SR passed in on cmd line
        if sruuid is None or sruuid == "all" or sruuid == uuid:
            try:
                srref = session.xenapi.SR.get_by_uuid(uuid)
                print("Setting SR (%s):" % uuid)
                session.xenapi.SR.set_name_label(srref, name_label)
                print("  Name: %s " % name_label)
                session.xenapi.SR.set_name_description(srref, name_descr)
                print("  Description: %s" % name_descr)
            except:
                print("Error setting SR data for: %s (%s)" % (uuid, name_label), file=sys.stderr)
                sys.exit(1)
            # go through all the SR VDIs and set the name_label and description
            for vdixml in srxml.childNodes:
                try:
                    vdi_uuid = vdixml.getAttribute("uuid")
                    vdi_label = vdixml.getAttribute("name_label")
                    vdi_descr = vdixml.getAttribute("name_description")
                except: 
                    print("Error parsing VDI tag", file=sys.stderr)
                    continue
                try:
                    vdiref = session.xenapi.VDI.get_by_uuid(vdi_uuid)
                    print("Setting VDI (%s):" % vdi_uuid)
                    session.xenapi.VDI.set_name_label(vdiref, vdi_label)
                    print("  Name: %s" % vdi_label)
                    session.xenapi.VDI.set_name_description(vdiref, vdi_descr)
                    print("  Description: %s" % vdi_descr)
                except:
                    print("Error setting VDI data for: %s (%s)" % (vdi_uuid, name_label), file=sys.stderr)
                    continue

if __name__ == "__main__":
    main(sys.argv[1:])


