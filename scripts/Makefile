IPROG=install -m 755 -o root -g root

SCRIPTS=/etc/xensource/scripts/install

REPO_SUFFIX:=$(shell basename "${REPO}")

.PHONY: bininstall
bininstall:
		mkdir -p $(DESTDIR)$(SCRIPTS)
		$(IPROG) install.sh $(DESTDIR)$(SCRIPTS)/install-$(REPO_SUFFIX).sh

.PHONY: binuninstall
		rm -f $(DESTDIR)$(SCRIPTS)/install-$(REPO_SUFFIX).sh
