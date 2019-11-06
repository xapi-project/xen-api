#!/usr/bin/env python

# Rewrite the VDI.sm_config:SCSIid fields in XVA metadata

from __future__ import print_function
import tarfile, xmlrpclib, optparse, StringIO, sys

class Object(object):
    """Represents an XVA metadata object, for example a VM, VBD, VDI, SR, VIF or Network.

       Fields can be accessed directly (e.g. print x.name_label) and modified in-place
       (e.g. x.name_label="new name")."""
    def __init__(self, cls, id, snapshot):
        self._cls = cls
        self._id = id
        self._snapshot = snapshot
    def marshal(self):
        return { "class": self._cls, "id": self._id, "snapshot": self._snapshot }
    def __getattribute__(self, name):
        try:
            return object.__getattribute__(self, name)
        except AttributeError:
            return self._snapshot[name]
    def __str__(self):
        name = self._snapshot["uuid"]
        if "name_label" in self._snapshot:
            name = name + ", name_label=" + self._snapshot["name_label"]
        return "%s/%s=%s" % (self._cls, self._id, name)

class MarshallingError(Exception):
    """Raised whenever we fail to regenerate the XVA metadata."""
    def __init__(self, message):
        self.message = message
    def __str__(self):
        return "MarshallingError: " + self.message

class XVA(object):
    """Represents an XVA archive.

       Metadata objects can be listed, modified and then the whole archive can
       be saved to a fresh file. All disk blocks will be copied from the old
       archive to the new."""
    def __init__(self, input, ova):
        self._input = input
        self._version = ova["version"]
        self._objects = map(lambda x: Object(x["class"], x["id"], x["snapshot"]), ova["objects"])

    def list(self):
        return self._objects

    def save(self, fileobj):
        # Reconstruct the ova.xml from Objects
        ova_txt = xmlrpclib.dumps(({"version": self._version, "objects": map(lambda x:x.marshal(), self._objects)}, ))
        prefix="<params>\n<param>\n"
        suffix="</param>\n</params>\n"
        if not(ova_txt.startswith(prefix)) or not(ova_txt.endswith(suffix)):
            raise MarshallingError("xmlrpclib produced an unexpected prefix or suffix")
        ova_txt = ova_txt[len(prefix):(len(ova_txt)-len(suffix))]

        # Write the new ova.xml
        output = tarfile.TarFile(mode='w', fileobj=fileobj)
        tarinfo = tarfile.TarInfo("ova.xml")
        tarinfo.size = len(ova_txt)
        output.addfile(tarinfo, StringIO.StringIO(ova_txt))
        # Stream the contents of the input, copying to the output
        for name in self._input.getnames():
            if name == "ova.xml":
                continue
            member = self._input.getmember(name)
            output.addfile(member, self._input.extractfile(member))
        output.close()

def open_xva(name):
    t = tarfile.open(name = name)
    ova_txt = t.extractfile("ova.xml").read()
    ova = xmlrpclib.loads("<params><param>" + ova_txt + "</param></params>")[0][0]
    return XVA(t, ova)

if __name__ == "__main__":
    parser = optparse.OptionParser()
    parser.add_option("-i", "--input", dest="input", help="Filename of the input XVA", metavar="FILE")
    parser.add_option("-o", "--output", dest="output", help="Filename of the output CVA")
    parser.add_option("--oldprefix", dest="oldprefix", help="SCSIid prefix to replace")
    parser.add_option("--newprefix", dest="newprefix", help="Replacement SCSIid prefix")
    parser.add_option("--test", dest="test", action="store_true", default=False, help="Developer testing mode (disabled by default)")
    (options, args) = parser.parse_args()

    if options.input is None:
        print("Please supply an --input <filename> argument")
        parser.print_help()
        sys.exit(1)
    if options.output is None:
        print("Please supply an --output <filename> argument")
        parser.print_help()
        sys.exit(1)
    if options.oldprefix is None:
        print("Please supply an --oldprefix <SCSIid> argument")
        parser.print_help()
        sys.exit(1)
    if options.newprefix is None:
        print("Please supply a --newprefix <SCSIid> argument")
        parser.print_help()
        sys.exit(1)

    fields = [ "sm_config" ]
    if options.test:
        fields = [ "other_config", "sm_config" ]

    xva = open_xva(options.input)
    for o in xva.list():
        try:
            for f in fields:
                if o.__getattribute__(f)["SCSIid"].startswith(options.oldprefix):
                    o.__getattribute__(f)["SCSIid"] = options.newprefix + o.__getattribute__(f)["SCSIid"][len(options.oldprefix):]
        except:
            pass
    xva.save(open(options.output, "w"))
