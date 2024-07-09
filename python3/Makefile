include ../config.mk

IPROG=install -m 755
IDATA=install -m 644

SITE3_DIR=$(shell python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())")

install:
	# Create destination directories using install -m 755 -d:
	$(IPROG) -d $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) -d $(DESTDIR)$(SITE3_DIR)
	$(IPROG) -d $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) -d $(DESTDIR)$(PLUGINDIR)
	$(IPROG) -d $(DESTDIR)/etc/sysconfig
	$(IPROG) -d $(DESTDIR)/usr/lib/systemd/system
	$(IPROG) -d $(DESTDIR)$(EXTENSIONDIR)

	
	$(IDATA) packages/inventory.py $(DESTDIR)$(SITE3_DIR)/
	$(IDATA) packages/observer.py $(DESTDIR)$(SITE3_DIR)/

	$(IPROG) libexec/host-display $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/link-vms-by-sr.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/usb_reset.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/usb_scan.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/nbd_client_manager.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/probe-device-for-file $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/print-custom-templates $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/mail-alarm $(DESTDIR)$(LIBEXECDIR)


	$(IPROG) bin/hfx_filename $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) bin/perfmon $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) bin/xe-scsi-dev-map $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) extensions/pool_update.apply $(DESTDIR)$(EXTENSIONDIR)

	$(IPROG) extensions/Test.test $(DESTDIR)$(EXTENSIONDIR)
	$(IPROG) plugins/disk-space $(DESTDIR)$(PLUGINDIR)
	$(IPROG) plugins/install-supp-pack $(DESTDIR)$(PLUGINDIR)
	$(IPROG) plugins/echo.py $(DESTDIR)$(PLUGINDIR)/echo
	$(IPROG) plugins/openvswitch-config-update $(DESTDIR)$(PLUGINDIR)

	$(IPROG) perfmon/perfmon $(DESTDIR)$(PLUGINDIR)
	$(IDATA) perfmon/perfmon.service $(DESTDIR)/usr/lib/systemd/system/perfmon.service
	$(IPROG) perfmon/sysconfig-perfmon $(DESTDIR)/etc/sysconfig/perfmon


# poweron
	$(IPROG) poweron/wlan.py $(DESTDIR)$(PLUGINDIR)/wlan.py
	$(IPROG) poweron/wlan.py $(DESTDIR)$(PLUGINDIR)/wake-on-lan
	$(IPROG) poweron/DRAC.py $(DESTDIR)$(PLUGINDIR)/DRAC.py
	$(IPROG) poweron/power-on.py $(DESTDIR)$(PLUGINDIR)/power-on-host
