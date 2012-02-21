# consumes a method in a service on the dbus
 
import dbus
 
bus = dbus.SessionBus()
service = bus.get_object('org.xen.xcp.xapi', '/org/xen/xcp/xapi')
f = service.get_dbus_method('test', 'org.xen.xcp.xapi')
print f()
