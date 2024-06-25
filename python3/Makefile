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

	
	$(IDATA) packages/observer.py $(DESTDIR)$(SITE3_DIR)/

	$(IPROG) libexec/link-vms-by-sr.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/usb_reset.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/usb_scan.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/nbd_client_manager.py $(DESTDIR)$(LIBEXECDIR)
	
	$(IPROG) bin/hfx_filename $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) bin/perfmon $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) bin/xe-scsi-dev-map $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) plugins/disk-space $(DESTDIR)$(PLUGINDIR)
	$(IPROG) poweron/wlan.py $(DESTDIR)$(PLUGINDIR)/wlan.py
	$(IPROG) poweron/wlan.py $(DESTDIR)$(PLUGINDIR)/wake-on-lan
	$(IPROG) poweron/DRAC.py $(DESTDIR)$(PLUGINDIR)/DRAC.py
