#!/usr/bin/env python
# simple python-dbus service that exposes 1 method called hello()

import getopt 
import sys
import subprocess
import gtk
import dbus
import dbus.service
from dbus.mainloop.glib import DBusGMainLoop

my_name = "dbus-resource-script"

name = None
script = None

try:
  opts, args = getopt.getopt(sys.argv[1:],"hn:c:s:",["name=","script="])
except getopt.GetoptError:
  print '%s -n <object> -s <script>' % my_name
  sys.exit(2)
for opt, arg in opts:
  if opt == '-h':
    print '%s -n <object> -s <script>' % my_name
    sys.exit()
  elif opt in ("-n", "--name"):
    name = arg
  elif opt in ("-s", "--script"):
    script = arg

if not(name):
  print "Please supply a bus name argument (-n or --name=)"
  sys.exit(2)
if not(script):
  print "Please supply a script name argument (-s or --script="
  sys.exit(2)

class Resource(dbus.service.Object):
    def __init__(self):
        self.bus = dbus.SessionBus()
        bus_name = dbus.service.BusName(name, bus=self.bus)
        dbus.service.Object.__init__(self, bus_name, "/" + name.replace(".", "/"))

    @dbus.service.method(dbus_interface=name, in_signature="s", out_signature="ss")
    def attach(self, global_uri):
        print "attach global_uri = %s" % global_uri
        p = subprocess.Popen([script, "attach", global_uri], stdout=subprocess.PIPE)
        stdout, stderr = p.communicate()
        return (stdout, "1")

    @dbus.service.method(dbus_interface=name, in_signature="s", out_signature="")
    def detach(self, id):
        print "detach id = %s" % id
        p = subprocess.Popen([script, "detach", id], stdout=subprocess.PIPE)
        stdout, stderr = p.communicate()
        pass

DBusGMainLoop(set_as_default=True)
resource = Resource()
gtk.main()
