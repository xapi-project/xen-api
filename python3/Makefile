include ../config.mk

# To replace strings in *.service like: @OPTDIR@ -> ${OPTDIR}
IPROG=../scripts/install.sh 755
IDATA=../scripts/install.sh 644

SITE3_DIR=$(shell python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())")

install:
	# Create destination directories using install -m 755 -d:
	install -m 755 -d $(DESTDIR)$(OPTDIR)/bin
	install -m 755 -d $(DESTDIR)$(SITE3_DIR)
	install -m 755 -d $(DESTDIR)$(LIBEXECDIR)
	install -m 755 -d $(DESTDIR)$(PLUGINDIR)
	install -m 755 -d $(DESTDIR)/etc/sysconfig
	install -m 755 -d $(DESTDIR)/usr/lib/systemd/system
	install -m 755 -d $(DESTDIR)$(EXTENSIONDIR)

	$(IDATA) packages/inventory.py $(DESTDIR)$(SITE3_DIR)/
	$(IDATA) packages/observer.py $(DESTDIR)$(SITE3_DIR)/

	$(IPROG) libexec/metrics.py $(DESTDIR)$(OPTDIR)/debug
	$(IPROG) libexec/metricsgraph.py $(DESTDIR)$(OPTDIR)/debug
	$(IPROG) libexec/host-display $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/link-vms-by-sr.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/usb_reset.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/usb_scan.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/nbd_client_manager.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/print-custom-templates $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/mail-alarm $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/backup-sr-metadata.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/restore-sr-metadata.py $(DESTDIR)$(LIBEXECDIR)

	$(IPROG) bin/hfx_filename $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) bin/xe-reset-networking $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) bin/perfmon $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) bin/xe-scsi-dev-map $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) bin/static-vdis $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) extensions/pool_update.apply $(DESTDIR)$(EXTENSIONDIR)
	$(IPROG) extensions/pool_update.precheck $(DESTDIR)$(EXTENSIONDIR)
	$(IPROG) extensions/Test.test $(DESTDIR)$(EXTENSIONDIR)
	$(IPROG) plugins/disk-space $(DESTDIR)$(PLUGINDIR)
	$(IPROG) plugins/install-supp-pack $(DESTDIR)$(PLUGINDIR)
	$(IPROG) plugins/echo.py $(DESTDIR)$(PLUGINDIR)/echo
	$(IPROG) plugins/openvswitch-config-update $(DESTDIR)$(PLUGINDIR)
	$(IPROG) plugins/extauth-hook $(DESTDIR)$(PLUGINDIR)
	$(IPROG) plugins/extauth-hook-AD.py $(DESTDIR)$(PLUGINDIR)

	$(IPROG) perfmon/perfmon $(DESTDIR)$(PLUGINDIR)
	$(IDATA) perfmon/perfmon.service $(DESTDIR)/usr/lib/systemd/system/perfmon.service
	$(IPROG) perfmon/sysconfig-perfmon $(DESTDIR)/etc/sysconfig/perfmon

# example/python
	$(IDATA) examples/XenAPIPlugin.py $(DESTDIR)$(SITE3_DIR)/
	$(IDATA) examples/XenAPI/XenAPI.py $(DESTDIR)$(SITE3_DIR)/


# poweron
	$(IPROG) poweron/wlan.py $(DESTDIR)$(PLUGINDIR)/wlan.py
	$(IPROG) poweron/wlan.py $(DESTDIR)$(PLUGINDIR)/wake-on-lan
	$(IPROG) poweron/power-on.py $(DESTDIR)$(PLUGINDIR)/power-on-host
	$(IPROG) poweron/IPMI.py $(DESTDIR)$(PLUGINDIR)/IPMI.py
