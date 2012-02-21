# simple python-dbus service that exposes 1 method called hello()
 
import gtk
import dbus
import dbus.service
from dbus.mainloop.glib import DBusGMainLoop
 
class Xenopsd(dbus.service.Object):
    def __init__(self):
        self.bus = dbus.SessionBus()
        bus_name = dbus.service.BusName('org.xen.xcp.xenopsd', bus=self.bus)
        dbus.service.Object.__init__(self, bus_name, '/org/xen/xcp/xenopsd')
 
    @dbus.service.method('org.xen.xcp.xenopsd')
    def VM_start(self):
        squeezed = self.bus.get_object('org.xen.xcp.squeezed', '/org/xen/xcp/squeezed')
        (amount, reservation) = squeezed.get_dbus_method('reserve_memory_range', 'org.xen.xcp.squeezed')(0, 1000)
        result = squeezed.get_dbus_method('transfer_reservation_to_domain', 'org.xen.xcp.squeezed')(reservation)
        return "VM started!"

DBusGMainLoop(set_as_default=True)
xenopsd = Xenopsd()
gtk.main()
