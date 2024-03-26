include ../config.mk

SITE3_DIR=$(shell python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())")

IDATA=install -m 644
DNF_PLUGIN_DIR=dnf-plugins

install:
	mkdir -p $(DESTDIR)$(SITE3_DIR)
	mkdir -p $(DESTDIR)$(SITE3_DIR)/$(DNF_PLUGIN_DIR)
	$(IDATA) packages/observer.py $(DESTDIR)$(SITE3_DIR)/
	$(IDATA) dnf_plugins/accesstoken.py $(DESTDIR)$(SITE3_DIR)/$(DNF_PLUGIN_DIR)/
	$(IDATA) dnf_plugins/ptoken.py $(DESTDIR)$(SITE3_DIR)/$(DNF_PLUGIN_DIR)/
