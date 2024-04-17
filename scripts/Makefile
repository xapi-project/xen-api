include ../config.mk

SITE_DIR=$(shell python -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())")
SITE3_DIR=$(shell python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())")

IPROG=./install.sh 755
IDATA=./install.sh 644

.PHONY: noarch-install install

install:
	mkdir -p $(DESTDIR)$(OPTDIR)/bin
	mkdir -p $(DESTDIR)$(VARPATCHDIR)
	mkdir -p $(DESTDIR)$(VARPATCHDIR)/applied
	mkdir -p $(DESTDIR)$(HOOKSDIR)
	mkdir -p $(DESTDIR)$(ETCXENDIR)
	mkdir -p $(DESTDIR)/etc/logrotate.d
	mkdir -p $(DESTDIR)/etc/yum/pluginconf.d
	mkdir -p $(DESTDIR)$(ETCXENDIR)/master.d
	mkdir -p $(DESTDIR)$(LIBEXECDIR)
	mkdir -p $(DESTDIR)/var/lib/xcp
	mkdir -p $(DESTDIR)/usr/lib/systemd/system
	mkdir -p $(DESTDIR)/usr/lib/yum-plugins
	mkdir -p $(DESTDIR)$(OPTDIR)/packages/post-install-scripts
	mkdir -p $(DESTDIR)$(SITE_DIR)
	mkdir -p $(DESTDIR)$(SITE3_DIR)
	mkdir -p $(DESTDIR)/etc/systemd/system/stunnel@xapi.service.d/
	$(IPROG) base-path $(DESTDIR)/etc/xapi.d
	$(IPROG) sm_diagnostics $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) xn_diagnostics $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) thread_diagnostics $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) list_plugins $(DESTDIR)$(LIBEXECDIR)
	mkdir -p $(DESTDIR)$(ETCXENDIR)/bugtool/xapi
	mkdir -p $(DESTDIR)$(ETCXENDIR)/bugtool/xenopsd
	mkdir -p $(DESTDIR)$(ETCXENDIR)/bugtool/observer
	$(IDATA) bugtool-plugin/xapi.xml $(DESTDIR)$(ETCXENDIR)/bugtool
	$(IDATA) bugtool-plugin/xapi/stuff.xml $(DESTDIR)$(ETCXENDIR)/bugtool/xapi
	$(IDATA) bugtool-plugin/xenopsd.xml $(DESTDIR)$(ETCXENDIR)/bugtool
	$(IDATA) bugtool-plugin/xenopsd/stuff.xml $(DESTDIR)$(ETCXENDIR)/bugtool/xenopsd
	$(IDATA) bugtool-plugin/observer.xml $(DESTDIR)$(ETCXENDIR)/bugtool
	$(IDATA) bugtool-plugin/observer/stuff.xml $(DESTDIR)$(ETCXENDIR)/bugtool/observer
	$(IPROG) fence $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) xha-lc $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) xapi-health-check $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) mail-alarm $(DESTDIR)$(LIBEXECDIR)
	$(IDATA) audit-logrotate $(DESTDIR)/etc/logrotate.d/audit
	$(IDATA) xapi-logrotate.conf $(DESTDIR)$(ETCXENDIR)
	$(IPROG) xapi-logrotate.sh $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) xapi-tracing-log-trim.sh $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) xapi-wait-init-complete $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) xapi-autostart-vms $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) udhcpd.skel $(DESTDIR)$(ETCXENDIR)/udhcpd.skel
	$(IDATA) xapi.conf $(DESTDIR)$(XAPICONF)
	$(IPROG) db.conf.skel $(DESTDIR)$(ETCXENDIR)/db.conf
	$(IPROG) rio.db.conf.skel $(DESTDIR)$(ETCXENDIR)/db.conf.rio
	$(IPROG) init.d-xapi $(DESTDIR)$(LIBEXECDIR)/xapi-init
	$(IPROG) init.d-save-boot-info $(DESTDIR)$(LIBEXECDIR)/save-boot-info
	$(IPROG) init.d-attach-static-vdis $(DESTDIR)$(LIBEXECDIR)/attach-static-vdis
	$(IPROG) generate-iscsi-iqn $(DESTDIR)$(LIBEXECDIR)/generate-iscsi-iqn
	$(IPROG) network-init $(DESTDIR)$(LIBEXECDIR)/network-init
	$(IPROG) control-domain-params-init $(DESTDIR)$(LIBEXECDIR)/control-domain-params-init
	$(IPROG) reset-and-reboot $(DESTDIR)$(LIBEXECDIR)/reset-and-reboot
	$(IDATA) cdrommon@.service $(DESTDIR)/usr/lib/systemd/system/cdrommon@.service
	$(IDATA) gencert.service $(DESTDIR)/usr/lib/systemd/system/gencert.service
	$(IDATA) xapi-domains.service $(DESTDIR)/usr/lib/systemd/system/xapi-domains.service
	$(IDATA) perfmon.service $(DESTDIR)/usr/lib/systemd/system/perfmon.service
	$(IDATA) generate-iscsi-iqn.service $(DESTDIR)/usr/lib/systemd/system/generate-iscsi-iqn.service
	$(IDATA) xapi.service $(DESTDIR)/usr/lib/systemd/system/xapi.service
	$(IDATA) attach-static-vdis.service $(DESTDIR)/usr/lib/systemd/system/attach-static-vdis.service
	$(IDATA) save-boot-info.service $(DESTDIR)/usr/lib/systemd/system/save-boot-info.service
	$(IDATA) mpathalert.service $(DESTDIR)/usr/lib/systemd/system/mpathalert.service
	$(IDATA) xapi-init-complete.target $(DESTDIR)/usr/lib/systemd/system/xapi-init-complete.target
	$(IDATA) xapi-wait-init-complete.service $(DESTDIR)/usr/lib/systemd/system/xapi-wait-init-complete.service
	$(IDATA) xcp-networkd.service $(DESTDIR)/usr/lib/systemd/system/xcp-networkd.service
	$(IDATA) xcp-networkd.conf $(DESTDIR)/etc/xcp-networkd.conf
	$(IDATA) wsproxy.service $(DESTDIR)/usr/lib/systemd/system/wsproxy.service
	$(IDATA) wsproxy.socket $(DESTDIR)/usr/lib/systemd/system/wsproxy.socket
	$(IDATA) varstored-guard.service $(DESTDIR)/usr/lib/systemd/system/varstored-guard.service
	$(IDATA) network-init.service $(DESTDIR)/usr/lib/systemd/system/network-init.service
	$(IDATA) control-domain-params-init.service $(DESTDIR)/usr/lib/systemd/system/control-domain-params-init.service
	$(IDATA) xapi-nbd.service $(DESTDIR)/usr/lib/systemd/system/xapi-nbd.service
	$(IDATA) xapi-nbd.path $(DESTDIR)/usr/lib/systemd/system/xapi-nbd.path
	$(IDATA) 10-stunnel-increase-number-of-file-descriptors.conf $(DESTDIR)/etc/systemd/system/stunnel@xapi.service.d/10-stunnel-increase-number-of-file-descriptors.conf
	$(IDATA) 11-stunnel-gencert.conf $(DESTDIR)/etc/systemd/system/stunnel@xapi.service.d/11-stunnel-gencert.conf
	mkdir -p $(DESTDIR)$(ETCXENDIR)/master.d
	$(IPROG) on-master-start $(DESTDIR)$(ETCXENDIR)/master.d/01-example
	$(IPROG) mpathalert-daemon $(DESTDIR)$(ETCXENDIR)/master.d/03-mpathalert-daemon
	mkdir -p $(DESTDIR)/etc/sysconfig
	$(IPROG) sysconfig-xapi $(DESTDIR)/etc/sysconfig/xapi
	$(IPROG) nbd-firewall-config.sh $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) fix_firewall.sh $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) update-ca-bundle.sh $(DESTDIR)$(OPTDIR)/bin
	mkdir -p $(DESTDIR)$(OPTDIR)/debug
	$(IPROG) debug_ha_query_liveset $(DESTDIR)$(OPTDIR)/debug
	$(IPROG) xe-scsi-dev-map $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) xe-mount-iso-sr $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) xe-reset-networking $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) xe-toolstack-restart $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) xe-xentrace $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) xe-edit-bootloader $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) xe-get-network-backend $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) xe-enable-all-plugin-metrics $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) static-vdis $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) with-vdi $(DESTDIR)$(OPTDIR)/debug
	$(IPROG) import-update-key $(DESTDIR)$(OPTDIR)/debug
	$(IPROG) pool.conf $(DESTDIR)$(ETCXENDIR)
	mkdir -p $(DESTDIR)/etc/pam.d
	$(IPROG) pam.d-xapi $(DESTDIR)/etc/pam.d/xapi
	$(IPROG) upload-wrapper logs-download $(DESTDIR)$(LIBEXECDIR)
	$(IDATA) usb-policy.conf $(DESTDIR)$(ETCXENDIR)
	mkdir -p $(DESTDIR)$(OPTDIR)/packages/iso #omg XXX
	$(IPROG) xapi-rolling-upgrade-miami $(DESTDIR)$(LIBEXECDIR)/xapi-rolling-upgrade
	$(IPROG) set-hostname $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) update-mh-info $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) host-bugreport-upload $(DESTDIR)$(LIBEXECDIR)/host-bugreport-upload
	$(IPROG) host-display $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) xe-backup-metadata $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) xe-restore-metadata $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) link-vms-by-sr.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) print-custom-templates $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) backup-sr-metadata.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) restore-sr-metadata.py $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) probe-device-for-file $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) backup-metadata-cron $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) pbis-force-domain-leave $(DESTDIR)$(LIBEXECDIR)
	mkdir -p $(DESTDIR)/etc/sysconfig
	$(IPROG) sysconfig-perfmon $(DESTDIR)/etc/sysconfig/perfmon
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
	$(IPROG) xe-switch-network-backend $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) xe-syslog-reconfigure $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) xe-install-supplemental-pack $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) xe-enable-ipv6 $(DESTDIR)$(OPTDIR)/bin
	$(IPROG) pv2hvm $(DESTDIR)$(OPTDIR)/bin
	mkdir -p $(DESTDIR)/etc/cron.daily
	mkdir -p $(DESTDIR)/etc/cron.hourly
	$(IPROG) license-check $(DESTDIR)/etc/cron.daily/
	$(IPROG) certificate-check $(DESTDIR)/etc/cron.daily/
	$(IPROG) certificate-refresh $(DESTDIR)/etc/cron.hourly/
	mkdir -p $(DESTDIR)/etc/cron.d
	$(IDATA) xapi-logrotate.cron $(DESTDIR)/etc/cron.d/xapi-logrotate.cron
	$(IDATA) xapi-tracing-log-trim.cron $(DESTDIR)/etc/cron.d/xapi-tracing-log-trim.cron
	mkdir -p $(DESTDIR)/opt/xensource/gpg
# templates
	$(IPROG) templates/debian $(DESTDIR)$(OPTDIR)/packages/post-install-scripts/debian-etch
	$(IPROG) templates/debug $(DESTDIR)$(OPTDIR)/packages/post-install-scripts
# host-backup-restore
	$(IPROG) host-backup-restore/host-backup $(DESTDIR)$(LIBEXECDIR)
	$(IPROG) host-backup-restore/host-restore $(DESTDIR)$(LIBEXECDIR)
# example/python
ifneq ($(BUILD_PY2), NO)
	$(IDATA) examples/python/XenAPIPlugin.py $(DESTDIR)$(SITE_DIR)/
	$(IDATA) examples/python/XenAPI/XenAPI.py $(DESTDIR)$(SITE_DIR)/
	$(IDATA) examples/python/inventory.py $(DESTDIR)$(SITE_DIR)/
endif
	$(IDATA) examples/python/XenAPIPlugin.py $(DESTDIR)$(SITE3_DIR)/
	sed -i 's/#!\/usr\/bin\/python/#!\/usr\/bin\/python3/' $(DESTDIR)$(SITE3_DIR)/XenAPIPlugin.py
	$(IDATA) examples/python/XenAPI/XenAPI.py $(DESTDIR)$(SITE3_DIR)/
	$(IDATA) examples/python/inventory.py $(DESTDIR)$(SITE3_DIR)/
	$(IPROG) examples/python/echo.py $(DESTDIR)$(PLUGINDIR)/echo
	$(IPROG) examples/python/shell.py $(DESTDIR)$(LIBEXECDIR)/shell.py
# poweron
	$(IPROG) poweron/wlan.py $(DESTDIR)$(PLUGINDIR)/wlan.py
	$(IPROG) poweron/wlan.py $(DESTDIR)$(PLUGINDIR)/wake-on-lan
	$(IPROG) poweron/DRAC.py $(DESTDIR)$(PLUGINDIR)/DRAC.py
	$(IPROG) poweron/power-on.py $(DESTDIR)$(PLUGINDIR)/power-on-host
# YUM plugins
	$(IPROG) yum-plugins/accesstoken.py $(DESTDIR)$(YUMPLUGINDIR)
	$(IDATA) yum-plugins/accesstoken.conf $(DESTDIR)$(YUMPLUGINCONFDIR)
	$(IPROG) yum-plugins/ptoken.py $(DESTDIR)$(YUMPLUGINDIR)
	$(IDATA) yum-plugins/ptoken.conf $(DESTDIR)$(YUMPLUGINCONFDIR)
# maillanguages
	mkdir -p $(DESTDIR)/etc/xapi.d/mail-languages
	$(IDATA) mail-languages/en-US.json $(DESTDIR)/etc/xapi.d/mail-languages
	$(IDATA) mail-languages/zh-CN.json $(DESTDIR)/etc/xapi.d/mail-languages
	$(IDATA) mail-languages/ja-JP.json $(DESTDIR)/etc/xapi.d/mail-languages
# uefi
	mkdir -p $(DESTDIR)/etc/xapi.d/efi-clone
