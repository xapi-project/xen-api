include ../config.mk

IPROG=install -m 755
IDATA=install -m 644

SITE3_DIR=$(shell python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())")

install:
	mkdir -p $(DESTDIR)$(OPTDIR)/bin
	mkdir -p $(DESTDIR)$(SITE3_DIR)

	$(IPROG) bin/hfx_filename $(DESTDIR)$(OPTDIR)/bin
	$(IDATA) packages/observer.py $(DESTDIR)$(SITE3_DIR)/
