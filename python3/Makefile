include ../config.mk

IPROG=install -m 755
IDATA=install -m 644

SITE3_DIR=$(shell python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())")

install:
	mkdir -p $(DESTDIR)$(OPTDIR)/bin
	mkdir -p $(DESTDIR)$(SITE3_DIR)
	mkdir -p $(DESTDIR)$(LIBEXECDIR)

	
	$(IDATA) packages/observer.py $(DESTDIR)$(SITE3_DIR)/

	$(IPROG) libexec/usb_reset.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/usb_scan.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) libexec/nbd_client_manager.py $(DESTDIR)$(LIBEXECDIR)
	
	$(IPROG) bin/hfx_filename $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) bin/perfmon $(DESTDIR)$(OPTDIR)/bin
