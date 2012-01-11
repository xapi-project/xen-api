# simple python-dbus service that exposes 1 method called hello()
 
import gtk
import dbus
import dbus.service
from dbus.mainloop.glib import DBusGMainLoop
 
class Xapi(dbus.service.Object):
    def __init__(self):
        self.bus = dbus.SessionBus()
        bus_name = dbus.service.BusName('org.xen.xcp.xapi', bus=self.bus)
        dbus.service.Object.__init__(self, bus_name, '/org/xen/xcp/xapi')

    @dbus.service.method('org.xen.xcp.xapi')
    def test(self):
        xenopsd = self.bus.get_object('org.xen.xcp.xenopsd', '/org/xen/xcp/xenopsd')
        start = xenopsd.get_dbus_method('VM_start', 'org.xen.xcp.xenopsd')
        print "About to call start method"
        # Deadlocks
        return start()

DBusGMainLoop(set_as_default=True)
xapi = Xapi()
gtk.main()
