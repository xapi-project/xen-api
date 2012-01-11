# simple python-dbus service that exposes 1 method called hello()
 
import gtk
import dbus
import dbus.service
from dbus.mainloop.glib import DBusGMainLoop
 
class Squeezed(dbus.service.Object):
    def __init__(self):
        self.bus = dbus.SessionBus()
        bus_name = dbus.service.BusName('org.xen.xcp.squeezed', bus=self.bus)
        dbus.service.Object.__init__(self, bus_name, '/org/xen/xcp/squeezed')

    @dbus.service.method('org.xen.xcp.squeezed')
    def reserve_memory_range(self, min, max):
        print "reserve_memory_range"
        return (max, 1)

    @dbus.service.method('org.xen.xcp.squeezed')
    def transfer_reservation_to_domain(self, reservation_id):
        print "transfer_reservation_to_domain"
        pass

DBusGMainLoop(set_as_default=True)
squeezed = Squeezed()
gtk.main()
