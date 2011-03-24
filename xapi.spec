# -*- rpm-spec -*-

%define XEN_RELEASE %(test -z "${XEN_RELEASE}" && echo unknown || echo $XEN_RELEASE)

Summary: xapi - xen toolstack for XCP
Name:    xapi
Version: 0.2
Release: %{XEN_RELEASE}
Group:   System/Hypervisor
License: LGPL+linking exception
URL:  http://www.xen.org
Source0: xapi-%{version}.tar.bz2
Source1: jquery-1.1.3.1.pack.js
Source2: jquery.treeview.zip
Patch0: xapi-version.patch
BuildRoot: %{_tmppath}/%{name}-%{version}-root
BuildRequires: pam-devel tetex-latex xapi-libs-devel ocaml omake ocaml-findlib ocaml-xmlm ocaml-type-conv ocaml-xmlm-devel xen-devel zlib-devel

%description
XCP toolstack.

%package core
Summary: The xapi toolstack
Group: System/Hypervisor

%description core
This package contains the xapi toolstack.

%package xe
Summary: The xapi toolstack CLI
Group: System/Hypervisor

%description xe
The command-line interface for controlling XCP hosts.

%package www
Summary: The XenAPI www interface
Group: System/Hypervisor

%description www
This package contains the XenAPI www interface

%package tests
Summary: Toolstack test programs
Group: System/Hypervisor

%description tests
This package contains a series of simple regression tests.

%package squeezed
Summary: The memory ballooning daemon
Group: System/Hypervisor

%description squeezed
This package contains the Xen virtual firmware (hvmloader)

%package v6d
Summary: The editions and features daemon
Group: System/Hypervisor

%description v6d
This package contains daemon that defines and controls XCP editions and
associated features

%package xenops
Summary: Low-level debugging tools
Group: System/Hypervisor

%description xenops
This package contains the xenops-based low-level debugging tools.

%package client-devel
Summary: xapi Development Headers and Libraries
Group:   Development/Libraries

%description client-devel
This package contains the xapi development libraries and header files
for building addon tools.

%package datamodel-devel
Summary: xapi Datamodel headers and libraries
Group:   Development/Libraries

%description datamodel-devel
This package contains the internal xapi datamodel as a library suitable
for writing additional code generators.

%package docs
Summary: Xen-API documentation and examples
Group:   Development/Documentation

%description docs
This package contains Xen-API documentation and examples in several programming languages.

%prep 
%setup -q
cp $RPM_SOURCE_DIR/jquery* $RPM_BUILD_DIR/xapi-0.2/ocaml/idl
%patch0 -p0 -b xapi-version.patch

%build
COMPILE_JAVA=no %{__make}

%install
rm -rf %{buildroot}

DESTDIR=$RPM_BUILD_ROOT %{__make} install
DESTDIR=$RPM_BUILD_ROOT %{__make} lib-install
DESTDIR=$RPM_BUILD_ROOT %{__make} sdk-install

%clean
rm -rf $RPM_BUILD_ROOT

%post core
[ ! -x /sbin/chkconfig ] || chkconfig --add xapi
[ ! -x /sbin/chkconfig ] || chkconfig --add management-interface
[ ! -x /sbin/chkconfig ] || chkconfig --add xenservices
[ ! -x /sbin/chkconfig ] || chkconfig --add xapi-domains
[ ! -x /sbin/chkconfig ] || chkconfig --add perfmon
[ ! -x /sbin/chkconfig ] || chkconfig --add genptoken

%post squeezed
[ ! -x /sbin/chkconfig ] || chkconfig squeezed on

%post v6d
[ ! -x /sbin/chkconfig ] || chkconfig --add v6d

%files core
%defattr(-,root,root,-)
/opt/xensource/bin/xapi
/etc/logrotate.d/audit
/etc/logrotate.d/v6d
/etc/logrotate.d/xapi
/etc/pam.d/xapi
/etc/rc.d/init.d/management-interface
/etc/rc.d/init.d/perfmon
/etc/rc.d/init.d/xapi
/etc/rc.d/init.d/xapi-domains
/etc/rc.d/init.d/xapissl
/etc/rc.d/init.d/xenservices
/etc/rc.d/init.d/sdkinit
/etc/rc.d/init.d/genptoken
/etc/sysconfig/perfmon
/etc/sysconfig/xapi
/etc/udev/rules.d/xen-backend.rules
/etc/udev/rules.d/xen-frontend.rules
/etc/udev/xen-backend.rules
/etc/udev/xen-frontend.rules
/etc/xapi.d/plugins/DRAC.py
/etc/xapi.d/plugins/DRAC.pyo
/etc/xapi.d/plugins/DRAC.pyc
/etc/xapi.d/plugins/echo
/etc/xapi.d/plugins/extauth-hook
/etc/xapi.d/plugins/extauth-hook-AD.py
/etc/xapi.d/plugins/extauth-hook-AD.pyo
/etc/xapi.d/plugins/extauth-hook-AD.pyc
/etc/xapi.d/plugins/iLO.py
/etc/xapi.d/plugins/iLO.pyo
/etc/xapi.d/plugins/iLO.pyc
/etc/xapi.d/plugins/iLOPowerON.xml
/etc/xapi.d/plugins/perfmon
/etc/xapi.d/plugins/power-on-host
/etc/xapi.d/plugins/wake-on-lan
/etc/xapi.d/plugins/wlan.py
/etc/xapi.d/plugins/wlan.pyo
/etc/xapi.d/plugins/wlan.pyc
/etc/xensource/db.conf
/etc/xensource/db.conf.rio
/etc/xensource/log.conf
/etc/xensource/master.d/01-example
/etc/xensource/master.d/03-mpathalert-daemon
/etc/xensource/pool.conf
/etc/xensource/scripts/block
/etc/xensource/scripts/block-frontend
/etc/xensource/scripts/tap
/etc/xensource/scripts/vif
/etc/xensource/xapi-ssl.conf
/etc/xensource/xapi.conf
/opt/xensource/bin/fix_firewall.sh
/opt/xensource/bin/list_domains
/opt/xensource/bin/mpathalert
/opt/xensource/bin/perfmon
/opt/xensource/bin/static-vdis
/opt/xensource/bin/v6d-reopen-logs
/opt/xensource/bin/xapi-db-process
/opt/xensource/bin/xapi-wait-init-complete
/opt/xensource/bin/xe-backup-metadata
/opt/xensource/bin/xe-edit-bootloader
/opt/xensource/bin/xe-mount-iso-sr
/opt/xensource/bin/xe-restore-metadata
/opt/xensource/bin/xe-reset-networking
/opt/xensource/bin/xe-scsi-dev-map
/opt/xensource/bin/xe-set-iscsi-iqn
/opt/xensource/bin/xe-toolstack-restart
/opt/xensource/bin/xe-xentrace
/opt/xensource/bin/xsh
/opt/xensource/libexec/fakeguestagent
/opt/xensource/libexec/InterfaceReconfigure.py
/opt/xensource/libexec/InterfaceReconfigure.pyo
/opt/xensource/libexec/InterfaceReconfigure.pyc
/opt/xensource/libexec/InterfaceReconfigureBridge.py
/opt/xensource/libexec/InterfaceReconfigureBridge.pyo
/opt/xensource/libexec/InterfaceReconfigureBridge.pyc
/opt/xensource/libexec/InterfaceReconfigureVswitch.py
/opt/xensource/libexec/InterfaceReconfigureVswitch.pyo
/opt/xensource/libexec/InterfaceReconfigureVswitch.pyc
/opt/xensource/libexec/backup-metadata-cron
/opt/xensource/libexec/backup-sr-metadata.py
/opt/xensource/libexec/backup-sr-metadata.pyo
/opt/xensource/libexec/backup-sr-metadata.pyc
/opt/xensource/libexec/block_device_io
/opt/xensource/libexec/c_rehash
/opt/xensource/libexec/cdrommon
/opt/xensource/libexec/dumpcore
/opt/xensource/libexec/fence
/opt/xensource/libexec/fence.bin
/opt/xensource/libexec/generate_ssl_cert
/opt/xensource/libexec/host-backup
/opt/xensource/libexec/host-bugreport-upload
/opt/xensource/libexec/host-restore
/opt/xensource/libexec/interface-reconfigure
/opt/xensource/libexec/interface-visualise
/opt/xensource/libexec/license-check.py
/opt/xensource/libexec/license-check.pyo
/opt/xensource/libexec/license-check.pyc
/opt/xensource/libexec/link-vms-by-sr.py
/opt/xensource/libexec/link-vms-by-sr.pyo
/opt/xensource/libexec/link-vms-by-sr.pyc
/opt/xensource/libexec/logrotate.sh
/opt/xensource/libexec/logs-download
/opt/xensource/libexec/lw-force-domain-leave
/opt/xensource/libexec/mail-alarm
/opt/xensource/libexec/print-custom-templates
/opt/xensource/libexec/probe-device-for-file
/opt/xensource/libexec/genptoken
/opt/xensource/libexec/qemu-dm-wrapper
/opt/xensource/libexec/restore-sr-metadata.py
/opt/xensource/libexec/restore-sr-metadata.pyo
/opt/xensource/libexec/restore-sr-metadata.pyc
/opt/xensource/libexec/rewrite-management-interface
/opt/xensource/libexec/set-dom0-memory-target-from-packs
/opt/xensource/libexec/set-hostname
/opt/xensource/libexec/shell.py
/opt/xensource/libexec/shell.pyo
/opt/xensource/libexec/shell.pyc
/opt/xensource/libexec/shutdown
/opt/xensource/libexec/sparse_dd
/opt/xensource/libexec/update-mh-info
/opt/xensource/libexec/upload-wrapper
/opt/xensource/libexec/vncterm-wrapper
/opt/xensource/libexec/xapi-health-check
/opt/xensource/libexec/xapi-rolling-upgrade
/opt/xensource/libexec/xenguest
/opt/xensource/libexec/xha-lc
/opt/xensource/libexec/xiu
/opt/xensource/libexec/pci-info
/opt/xensource/packages/post-install-scripts/debian-etch
/opt/xensource/packages/post-install-scripts/debug
/usr/lib/python2.4/site-packages/XenAPI.py
/usr/lib/python2.4/site-packages/XenAPI.pyo
/usr/lib/python2.4/site-packages/XenAPI.pyc
/usr/lib/python2.4/site-packages/XenAPIPlugin.py
/usr/lib/python2.4/site-packages/XenAPIPlugin.pyo
/usr/lib/python2.4/site-packages/XenAPIPlugin.pyc
/usr/lib/python2.4/site-packages/inventory.py
/usr/lib/python2.4/site-packages/inventory.pyo
/usr/lib/python2.4/site-packages/inventory.pyc
%exclude   /usr/lib/python2.4/site-packages/xen/*
%exclude   /usr/lib/python2.4/site-packages/xen/lowlevel/*
/var/xapi/udhcpd.skel
/opt/xensource/debug/rbac_static.csv
/etc/xapi.d/host-post-declare-dead/10resetvdis

%files xe
%defattr(-,root,root,-)
/opt/xensource/bin/xe
/usr/bin/xe
/etc/bash_completion.d/cli

%files squeezed
%defattr(-,root,root,-)
/opt/xensource/libexec/squeezed
/etc/logrotate.d/squeezed
/etc/rc.d/init.d/squeezed
/opt/xensource/bin/squeezed_client

%files v6d
%defattr(-,root,root,-)
/opt/xensource/libexec/v6d
/etc/rc.d/init.d/v6d

%files xenops
%defattr(-,root,root,-)
/opt/xensource/debug/xenops
/opt/xensource/debug/add_vbd
/opt/xensource/debug/add_vif
/opt/xensource/debug/build_domain
/opt/xensource/debug/build_hvm
/opt/xensource/debug/create_domain
/opt/xensource/debug/debug_ha_query_liveset
/opt/xensource/debug/destroy_domain
/opt/xensource/debug/event_listen
/opt/xensource/debug/graph
/opt/xensource/debug/memory_breakdown
/opt/xensource/debug/memory_summary
/opt/xensource/debug/pause_domain
/opt/xensource/debug/restore_domain
/opt/xensource/debug/rrddump
/opt/xensource/debug/shutdown_domain
/opt/xensource/debug/sm_stress
/opt/xensource/debug/suspend_domain
/opt/xensource/debug/unpause_domain
/opt/xensource/debug/vncproxy
/opt/xensource/debug/with-vdi
/opt/xensource/debug/xal
/opt/xensource/debug/xs

%files www
%defattr(-,root,root,-)
#/opt/xensource/debug/www/XenServerConsole.jar
/opt/xensource/debug/www/api.js
/opt/xensource/debug/www/apicall.css
/opt/xensource/debug/www/apicall.js
/opt/xensource/debug/www/construct_tree.js
/opt/xensource/debug/www/devweb.css
/opt/xensource/debug/www/devweb.js
/opt/xensource/debug/www/editables.js
/opt/xensource/debug/www/graphs.js
/opt/xensource/debug/www/haplan.css
/opt/xensource/debug/www/haplan.js
/opt/xensource/debug/www/images/xen_logo.gif
/opt/xensource/debug/www/index.html
/opt/xensource/debug/www/jquery/jqDnR.css
/opt/xensource/debug/www/jquery/jqDnR.js
/opt/xensource/debug/www/jquery/jqModal.css
/opt/xensource/debug/www/jquery/jqModal.js
/opt/xensource/debug/www/jquery/jquery-1.3.2.js
/opt/xensource/debug/www/jquery/jquery-dom.js
/opt/xensource/debug/www/jquery/jquery.autocomplete.css
/opt/xensource/debug/www/jquery/jquery.autocomplete.js
/opt/xensource/debug/www/jquery/jquery.bgiframe.js
/opt/xensource/debug/www/jquery/jquery.color.js
/opt/xensource/debug/www/jquery/jquery.cookie.js
/opt/xensource/debug/www/jquery/jquery.jeditable.js
/opt/xensource/debug/www/jquery/jquery.rpc.js
/opt/xensource/debug/www/jquery/jquery.treeview.js
/opt/xensource/debug/www/main.js
/opt/xensource/debug/www/messages.js
/opt/xensource/debug/www/networks.css
/opt/xensource/debug/www/networks.js
/opt/xensource/debug/www/objectviewoverrides.js
/opt/xensource/debug/www/offline.js
/opt/xensource/debug/www/process_rrd.js
/opt/xensource/debug/www/tree.css
/opt/xensource/debug/www/vmsearch.js
/opt/xensource/debug/www/xenapi.js

%files tests
%defattr(-,root,root,-)
/etc/xapi.d/plugins/lvhdrt-helper
/etc/xapi.d/plugins/lvhdrt-trash-vdi
/etc/xapi.d/plugins/multipathrt-helper
/opt/xensource/debug/cli-rt-domu-shar.sh
/opt/xensource/debug/cli_test
/opt/xensource/debug/install-debian-pv-inside.sh
/opt/xensource/debug/install-debian-pv.sh
/opt/xensource/debug/lvhdrt
/opt/xensource/debug/multipathrt
/opt/xensource/debug/myfirstpatch.asc
/opt/xensource/debug/perftest
/opt/xensource/debug/quicktest
/opt/xensource/debug/quicktestbin
/opt/xensource/debug/watch_test
/cli-rt/*

%files client-devel
%defattr(-,root,root,-)
/usr/lib/ocaml/xapi-client/*

%files datamodel-devel
%defattr(-,root,root,-)
/usr/lib/ocaml/xapi-datamodel/*

%files docs
%defattr(-,root,root,-)
/usr/share/doc/xapi/*
#/usr/share/doc/xapi/client-examples/*/*
#/usr/share/doc/xapi/docs/html/API/Classes/*/index.html
#/usr/share/doc/xapi/docs/html/API/Classes/*/Explicit/*
#/usr/share/doc/xapi/docs/html/API/Classes/*/Implicit/*
#/usr/share/doc/xapi/docs/html/API/Classes/*/Fields/*
#/usr/share/doc/xapi/docs/html/*
#/usr/share/doc/xapi/docs/html/images/*
#/usr/share/doc/xapi/docs/pdf/xenenterpriseapi.pdf

%changelog








