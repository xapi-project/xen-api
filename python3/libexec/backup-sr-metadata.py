#!/usr/bin/python3
# Back up the SR metadata and VDI list into an XML file
# (c) Anil Madhavapeddy, Citrix Systems Inc, 2008

import atexit
import codecs
import contextlib
import getopt
import sys
from xml.dom.minidom import Document  # pytype: disable=pyi-error

import XenAPI


def usage():
    print("%s [-f <output file>]" % sys.argv[0], file=sys.stderr)


def set_if_exists(xml, record, key):
    if key in record:
        xml.setAttribute(key, record[key])
    else:
        xml.setAttribute(key, "")

def main(argv):
    session = XenAPI.xapi_local()

    def logout():
        with contextlib.suppress(Exception):
            session.xenapi.session.logout()

    atexit.register(logout)

    session.xenapi.login_with_password("", "", "1.0", "xen-api-scripts-backup-sr-metadata")

    try:
        opts, _ = getopt.getopt(argv, "hf:", [])
    except getopt.GetoptError as err:
        print(err)
        usage()
        sys.exit(1)

    outfile = None
    for o,a in opts:
        if o == "-f":
            outfile = a

    if outfile is None:
        usage()
        sys.exit(1)

    f = codecs.open(outfile, 'w', encoding="utf-8")

    srs = session.xenapi.SR.get_all_records()

    doc = Document()

    metaxml = doc.createElement("meta")
    doc.appendChild(metaxml)

    for srref in srs.keys():
        srrec = srs[srref]
        srxml = doc.createElement("sr")
        set_if_exists(srxml, srrec, 'uuid')
        set_if_exists(srxml, srrec, 'name_label')
        set_if_exists(srxml, srrec, 'name_description')

        for vdiref in srrec['VDIs']:
            try:
                vdirec = session.xenapi.VDI.get_record(vdiref)
                vdixml = doc.createElement("vdi")
                set_if_exists(vdixml, vdirec, 'uuid')
                set_if_exists(vdixml, vdirec, 'name_label')
                set_if_exists(vdixml, vdirec, 'name_description')
                srxml.appendChild(vdixml)
            except Exception:
                print("Failed to get VDI record for: %s" % vdiref, file=sys.stderr)

        metaxml.appendChild(srxml)

    doc.writexml(f, encoding="utf-8")
    f.close()
    session.xenapi.logout()

if __name__ == "__main__":
    main(sys.argv[1:])
