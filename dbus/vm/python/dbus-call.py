#!/usr/bin/env python

# Wrapper around single-shot resource attach/detach scripts.
#
# The 'resource' interface allows storage and networks to be attached
# and detached by single-shot scripts. All operations are asynchronous,
# 'attach' and 'detach' return a 'task' object which has a 'cancel'
# operation. When 'cancel' is called, the running subprocesses are
# first sent SIGTERM and then SIGKILL.

import syslog
import threading
import urlparse
import time
import getopt
import os
import sys
import subprocess
import gtk
import gobject
import dbus
import dbus.service
from dbus.mainloop.glib import DBusGMainLoop

my_name = "dbus-call"

uri = None
debug_on = False
def debug(message):
  if debug_on:
    print "DEBUG: ", message
    sys.stdout.flush()

syslog.openlog(my_name)
def info(message):
  if debug_on:
    print "INFO: ", message
    sys.stdout.flush()
  syslog.syslog(message)
def error(message):
  if debug_on:
    print "ERROR: ", message
    sys.stdout.flush()
  syslog.syslog(syslog.LOG_ERR, message)

try:
  opts, args = getopt.getopt(sys.argv[1:],"du",["debug","uri="])
except getopt.GetoptError:
  print '%s -u <uri>' % my_name
  sys.exit(2)
for opt, arg in opts:
  if opt in ("-d", "--debug"):
    debug_on = True
  elif opt == '-h':
    print '%s -u <uri>' % my_name
    sys.exit()
  elif opt in ("-u", "--uri"):
    uri = arg

if not(uri):
  print "Please supply URI to resource object (-u or --uri=)"
  sys.exit(2)

if len(args) <> 2:
  print "Please supply a command and argument (e.g. attach iscsi://foo)"
  sys.exit(2)
command = args[0]
argument = args[1]

TASK_INTERFACE="org.xenserver.Task1"
TASKOWNER_INTERFACE="org.xenserver.TaskOwner1"
RESOURCE_INTERFACE="org.xenserver.Resource1"

class TaskOwner(dbus.service.Object):
    def __init__(self):
        self.bus = dbus.SessionBus()
        self.path = "/org/xenserver/task/owner1"
        scheme = self.bus.get_unique_name()[1:] #remove : prefix
        self.uri = scheme + "://" + self.path
        self.owned_uris = []
        info("TaskOwner1 registered at %s" % self.uri)
        dbus.service.Object.__init__(self, self.bus, self.path)

    @dbus.service.method(dbus_interface=TASKOWNER_INTERFACE, in_signature="as", out_signature="ab")
    def ping(self, uris):
        results = []
        for uri in uris:
            if self.owned_uris == []:
                debug("%s: I might own that" % uri)
                results.append(True)
            elif uri in self.owned_uris:
                debug("%s: I definitely own that" % uri)
            else:
                debug("%s: I do not own that" % uri)
                results.append(False)
        return results

    def add(self, uri):
        self.owned_uris.append(uri)

    @dbus.service.method(dbus_interface=dbus.PROPERTIES_IFACE, in_signature='ss', out_signature='v')
    def Get(self, interface_name, property_name):
        return self.GetAll(interface_name)[property_name]

    @dbus.service.method(dbus_interface=dbus.PROPERTIES_IFACE, in_signature='s', out_signature='a{sv}')
    def GetAll(self, interface_name):
        if interface_name == TASKOWNER_INTERFACE:
            return { "tasks": self.owned_uris }
        else:
            raise dbus.exceptions.DBusException(
                'com.example.UnknownInterface',
                'The Task object does not implement the %s interface'
                    % interface_name)

#gobject.threads_init()
DBusGMainLoop(set_as_default=True)
taskOwner = TaskOwner()
uri = urlparse.urlparse(uri)
bus = dbus.SessionBus()
proxy = bus.get_object(uri.scheme, uri.path)
operation_id = "%s.%d" % (my_name, os.getpid())

if command == "attach":
    task = proxy.attach(argument, taskOwner.uri, operation_id, dbus_interface=RESOURCE_INTERFACE)
elif command == "detach":
    task = proxy.detach(argument, taskOwner.uri, operation_id, dbus_interface=RESOURCE_INTERFACE)
else:
    print "Unknown command %s, expected either 'attach' or 'detach'" % command
    sys.exit(2)
taskOwner.add(task)
info("%s: created" % task)

uri = urlparse.urlparse(task)
task_proxy = bus.get_object(uri.scheme, uri.path)

def handler():
    info("%s: got Completed signal" % task)
    result = task_proxy.getResult(dbus_interface=TASK_INTERFACE)
    info("%s: result = %s" % (task, result))
    print result
    task_proxy.destroy(dbus_interface=TASK_INTERFACE)
    loop.quit()

task_proxy.connect_to_signal("Completed", handler)
loop = gobject.MainLoop()
loop.run()
