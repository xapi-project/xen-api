include ../config.mk

SITE_DIR=$(shell python -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())")

IPROG=./install.sh 755
IDATA=./install.sh 644

.PHONY: noarch-install install

install:
	mkdir -p $(DESTDIR)$(BINDIR)
	mkdir -p $(DESTDIR)$(VARPATCHDIR)
	mkdir -p $(DESTDIR)$(VARPATCHDIR)/applied
	mkdir -p $(DESTDIR)$(HOOKSDIR)
	mkdir -p $(DESTDIR)$(ETCDIR)
	mkdir -p $(DESTDIR)/etc/logrotate.d
	mkdir -p $(DESTDIR)$(ETCDIR)/master.d
	mkdir -p $(DESTDIR)$(LIBEXECDIR)
	mkdir -p $(DESTDIR)/var/lib/xcp
	mkdir -p $(DESTDIR)/usr/lib/systemd/system
	mkdir -p $(DESTDIR)$(OPTDIR)/packages/post-install-scripts
	mkdir -p $(DESTDIR)$(SITE_DIR)
	$(IPROG) base-path $(DESTDIR)/etc/xapi.d
	$(IPROG) sm_diagnostics $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) xn_diagnostics $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) thread_diagnostics $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) list_plugins $(DESTDIR)$(LIBEXECDIR)
	mkdir -p $(DESTDIR)$(ETCDIR)/bugtool/xapi
	mkdir -p $(DESTDIR)$(ETCDIR)/bugtool/xenopsd
	$(IDATA) bugtool-plugin/xapi.xml $(DESTDIR)$(ETCDIR)/bugtool
	$(IDATA) bugtool-plugin/xapi/stuff.xml $(DESTDIR)$(ETCDIR)/bugtool/xapi
	$(IDATA) bugtool-plugin/xenopsd.xml $(DESTDIR)$(ETCDIR)/bugtool
	$(IDATA) bugtool-plugin/xenopsd/stuff.xml $(DESTDIR)$(ETCDIR)/bugtool/xenopsd
	$(IPROG) fence $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) xha-lc $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) xapi-health-check $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) mail-alarm $(DESTDIR)$(LIBEXECDIR)
	$(IDATA) audit-logrotate $(DESTDIR)/etc/logrotate.d/audit
	$(IDATA) xapi-logrotate.conf $(DESTDIR)$(ETCDIR)
	$(IPROG) xapi-logrotate.sh $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) xapi-wait-init-complete $(DESTDIR)$(BINDIR)
	$(IPROG) xapi-autostart-vms $(DESTDIR)$(BINDIR)
	$(IPROG) udhcpd.skel $(DESTDIR)$(ETCDIR)/udhcpd.skel
	$(IDATA) xapi.conf $(DESTDIR)$(XAPICONF)
	$(IPROG) db.conf.skel $(DESTDIR)$(ETCDIR)/db.conf
	$(IPROG) rio.db.conf.skel $(DESTDIR)$(ETCDIR)/db.conf.rio
	$(IPROG) init.d-xapi $(DESTDIR)$(LIBEXECDIR)/xapi-init
	$(IPROG) init.d-xapissl $(DESTDIR)$(LIBEXECDIR)/xapissl
	$(IPROG) init.d-management-interface $(DESTDIR)$(LIBEXECDIR)/legacy-management-interface
	$(IPROG) init.d-save-boot-info $(DESTDIR)$(LIBEXECDIR)/save-boot-info
	$(IPROG) init.d-attach-static-vdis $(DESTDIR)$(LIBEXECDIR)/attach-static-vdis
	$(IDATA) cdrommon@.service $(DESTDIR)/usr/lib/systemd/system/cdrommon@.service
	$(IDATA) xapi-domains.service $(DESTDIR)/usr/lib/systemd/system/xapi-domains.service
	$(IDATA) perfmon.service $(DESTDIR)/usr/lib/systemd/system/perfmon.service
	$(IDATA) genptoken.service $(DESTDIR)/usr/lib/systemd/system/genptoken.service
	$(IDATA) xapi.service $(DESTDIR)/usr/lib/systemd/system/xapi.service
	$(IDATA) attach-static-vdis.service $(DESTDIR)/usr/lib/systemd/system/attach-static-vdis.service
	$(IDATA) save-boot-info.service $(DESTDIR)/usr/lib/systemd/system/save-boot-info.service
	$(IDATA) mpathalert.service $(DESTDIR)/usr/lib/systemd/system/mpathalert.service
	mkdir -p $(DESTDIR)$(ETCDIR)/master.d
	$(IPROG) on-master-start $(DESTDIR)$(ETCDIR)/master.d/01-example
	$(IPROG) mpathalert-daemon $(DESTDIR)$(ETCDIR)/master.d/03-mpathalert-daemon
	mkdir -p $(DESTDIR)/etc/sysconfig
	$(IPROG) sysconfig-xapi $(DESTDIR)/etc/sysconfig/xapi
	$(IPROG) generate_ssl_cert $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) nbd-firewall-config.sh $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) fix_firewall.sh $(DESTDIR)$(BINDIR)
	mkdir -p $(DESTDIR)$(OPTDIR)/debug
	$(IPROG) debug_ha_query_liveset $(DESTDIR)$(OPTDIR)/debug
	$(IPROG) xe-scsi-dev-map $(DESTDIR)$(BINDIR)
	$(IPROG) xe-mount-iso-sr $(DESTDIR)$(BINDIR)
	$(IPROG) xe-reset-networking $(DESTDIR)$(BINDIR)
	$(IPROG) xe-set-iscsi-iqn $(DESTDIR)$(BINDIR)
	$(IPROG) xe-toolstack-restart $(DESTDIR)$(BINDIR)
	$(IPROG) xe-xentrace $(DESTDIR)$(BINDIR)
	$(IPROG) xe-edit-bootloader $(DESTDIR)$(BINDIR)
	$(IPROG) xe-get-network-backend $(DESTDIR)$(BINDIR)
	$(IPROG) xe-enable-all-plugin-metrics $(DESTDIR)$(BINDIR)
	$(IPROG) static-vdis $(DESTDIR)$(BINDIR)
	$(IPROG) with-vdi $(DESTDIR)$(OPTDIR)/debug
	$(IPROG) import-update-key $(DESTDIR)$(OPTDIR)/debug
	$(IPROG) pool.conf $(DESTDIR)$(ETCDIR)
	mkdir -p $(DESTDIR)/etc/pam.d
	$(IPROG) pam.d-xapi $(DESTDIR)/etc/pam.d/xapi
	$(IPROG) upload-wrapper logs-download $(DESTDIR)$(LIBEXECDIR)
	$(IDATA) usb-policy.conf $(DESTDIR)$(ETCDIR)
	$(IPROG) usb_reset.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) usb_scan.py $(DESTDIR)$(LIBEXECDIR)
	mkdir -p $(DESTDIR)$(OPTDIR)/packages/iso #omg XXX
	$(IPROG) interface-reconfigure $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) InterfaceReconfigure.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) InterfaceReconfigureBridge.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) InterfaceReconfigureVswitch.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) interface-visualise $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) xapi-rolling-upgrade-miami $(DESTDIR)$(LIBEXECDIR)/xapi-rolling-upgrade
	$(IPROG) set-hostname $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) update-mh-info $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) host-bugreport-upload $(DESTDIR)$(LIBEXECDIR)/host-bugreport-upload
	$(IPROG) host-display $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) xe-backup-metadata $(DESTDIR)$(BINDIR)
	$(IPROG) xe-restore-metadata $(DESTDIR)$(BINDIR)
	$(IPROG) link-vms-by-sr.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) print-custom-templates $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) backup-sr-metadata.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) restore-sr-metadata.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) probe-device-for-file $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) backup-metadata-cron $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) pbis-force-domain-leave $(DESTDIR)$(LIBEXECDIR)
	mkdir -p $(DESTDIR)/etc/sysconfig
	$(IPROG) sysconfig-perfmon $(DESTDIR)/etc/sysconfig/perfmon
	$(IPROG) perfmon $(DESTDIR)$(BINDIR)
	mkdir -p $(DESTDIR)$(EXTENSIONDIR)
	$(IPROG) extensions/Test.test $(DESTDIR)$(EXTENSIONDIR)
	$(IPROG) extensions/pool_update.precheck $(DESTDIR)$(EXTENSIONDIR)
	$(IPROG) extensions/pool_update.apply $(DESTDIR)$(EXTENSIONDIR)
	mkdir -p $(DESTDIR)$(PLUGINDIR)
	$(IPROG) plugins/perfmon $(DESTDIR)$(PLUGINDIR)
	$(IPROG) plugins/extauth-hook $(DESTDIR)$(PLUGINDIR)
	$(IPROG) plugins/extauth-hook-AD.py $(DESTDIR)$(PLUGINDIR)
	$(IPROG) plugins/iovirt $(DESTDIR)$(PLUGINDIR)
	$(IPROG) plugins/install-supp-pack $(DESTDIR)$(PLUGINDIR)
	$(IPROG) plugins/disk-space $(DESTDIR)$(PLUGINDIR)
	$(IPROG) plugins/firewall-port $(DESTDIR)$(PLUGINDIR)
	$(IPROG) plugins/openvswitch-config-update $(DESTDIR)$(PLUGINDIR)
	mkdir -p $(DESTDIR)$(HOOKSDIR)/host-post-declare-dead
	$(IPROG) 10resetvdis $(DESTDIR)$(HOOKSDIR)/host-post-declare-dead
	mkdir -p $(DESTDIR)/etc/bash_completion.d
	$(IPROG) xe-switch-network-backend-bash-completion $(DESTDIR)/etc/bash_completion.d/xe-switch-network-backend
	$(IPROG) xe-switch-network-backend $(DESTDIR)$(BINDIR)
	$(IPROG) xe-syslog-reconfigure $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) xe-install-supplemental-pack $(DESTDIR)$(BINDIR)
	$(IPROG) xe-enable-ipv6 $(DESTDIR)$(BINDIR)
	$(IPROG) hfx_filename $(DESTDIR)$(BINDIR)
	$(IPROG) pv2hvm $(DESTDIR)$(BINDIR)
	mkdir -p $(DESTDIR)/etc/cron.daily
	$(IPROG) license-check $(DESTDIR)/etc/cron.daily/
	mkdir -p $(DESTDIR)/etc/cron.d
	$(IDATA) xapi-logrotate.cron $(DESTDIR)/etc/cron.d/xapi-logrotate.cron
	mkdir -p $(DESTDIR)/opt/xensource/gpg
# templates
	$(IPROG) templates/debian $(DESTDIR)$(OPTDIR)/packages/post-install-scripts/debian-etch
	$(IPROG) templates/debug $(DESTDIR)$(OPTDIR)/packages/post-install-scripts
# host-backup-restore
	$(IPROG) host-backup-restore/host-backup $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) host-backup-restore/host-restore $(DESTDIR)$(LIBEXECDIR)
# example/python
	$(IDATA) examples/python/XenAPIPlugin.py $(DESTDIR)$(SITE_DIR)/
	$(IDATA) examples/python/XenAPI.py $(DESTDIR)$(SITE_DIR)/
	$(IDATA) examples/python/inventory.py $(DESTDIR)$(SITE_DIR)/
	$(IPROG) examples/python/echo.py $(DESTDIR)$(PLUGINDIR)/echo
	$(IPROG) examples/python/wake-on-lan.py $(DESTDIR)$(PLUGINDIR)/wake-on-lan
	$(IPROG) examples/python/shell.py $(DESTDIR)$(LIBEXECDIR)/shell.py
	$(IPROG) examples/python/xva.py $(DESTDIR)$(BINDIR)/xva-rewrite-scsiid
# poweron
	$(IPROG) poweron/wlan.py $(DESTDIR)$(PLUGINDIR)/wlan.py
	$(IPROG) poweron/iLOPowerON.xml $(DESTDIR)$(PLUGINDIR)/iLOPowerON.xml
	$(IPROG) poweron/iLO.py $(DESTDIR)$(PLUGINDIR)/iLO.py
	$(IPROG) poweron/DRAC.py $(DESTDIR)$(PLUGINDIR)/DRAC.py
	$(IPROG) poweron/power-on.py $(DESTDIR)$(PLUGINDIR)/power-on-host
# maillanguages
	mkdir -p $(DESTDIR)/etc/xapi.d/mail-languages
	$(IDATA) mail-languages/en-US.json $(DESTDIR)/etc/xapi.d/mail-languages
	$(IDATA) mail-languages/zh-CN.json $(DESTDIR)/etc/xapi.d/mail-languages
	$(IDATA) mail-languages/ja-JP.json $(DESTDIR)/etc/xapi.d/mail-languages
	

