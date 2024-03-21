include ../config.mk

SITE3_DIR=$(shell python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())")

IDATA=install -m 644

install:
	mkdir -p $(DESTDIR)$(SITE3_DIR)
	$(IDATA) packages/observer.py $(DESTDIR)$(SITE3_DIR)/
