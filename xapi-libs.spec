# -*- rpm-spec -*-

%define XEN_RELEASE %(test -z "${XEN_RELEASE}" && echo unknown || echo $XEN_RELEASE)

Summary: xapi-libs - xen toolstack for XCP libraries
Name:    xapi-libs
Version: 0
Release: %{XEN_RELEASE}
Group:   System/Hypervisor
License: LGPL+linking exception
URL:  http://www.xen.org
Source0: xapi-libs-%{version}.tar.bz2
BuildRoot: %{_tmppath}/%{name}-%{version}-root
BuildRequires: ocaml, ocaml-findlib, ocaml-camlp4, ocaml-type-conv, ocaml-getopt, ocaml-xmlm, ocaml-xmlm-devel, autoconf, automake, xen-devel, blktap-devel, e2fsprogs-devel

%description
The xapi toolstack development libraries and tools.

%package utils
Summary: Some utilities associated with the xapi toolstack
Group: System/Hypervisor

%description utils
Some miscellaneous utilities

%package devel
Summary: The xapi toolstack development libraries
Group: System/Hypervisor

%description devel
This package contains core development libraries required for building xapi.

%package fe
Summary: The fork-and-exec daemon
Group: System/Hypervisor

%description fe
This package contains the fork-and-exec daemon, required by xapi at runtime.

%prep 
%setup -q
%build
sh autogen.sh
./configure
%{__make} all allxen bins

%install
rm -rf %{buildroot}

DESTDIR=$RPM_BUILD_ROOT %{__make} install installxen bininstall

%clean
rm -rf $RPM_BUILD_ROOT

%post fe
[ ! -x /sbin/chkconfig ] || chkconfig fe on


%files fe
%defattr(-,root,root,-)
/opt/xensource/libexec/fe
/etc/rc.d/init.d/fe
/opt/xensource/libexec/fe_cli
/opt/xensource/libexec/fe_test

%files utils
%defattr(-,root,root,-)
   /opt/xensource/libexec/base64pp
   /opt/xensource/libexec/closeandexec
   /opt/xensource/libexec/pciutil
   /opt/xensource/libexec/sexprpp
   /opt/xensource/libexec/xmlpp
   /opt/xensource/libexec/extentlistset_test

%files devel
%defattr(-,root,root,-)
   /usr/lib/ocaml/camldm/META
   /usr/lib/ocaml/camldm/camldm.a
   /usr/lib/ocaml/camldm/camldm.cma
   /usr/lib/ocaml/camldm/camldm.cmi
   /usr/lib/ocaml/camldm/camldm.cmx
   /usr/lib/ocaml/camldm/camldm.cmxa
   /usr/lib/ocaml/camldm/dllcamldm_stubs.so
   /usr/lib/ocaml/camldm/libcamldm_stubs.a
   /usr/lib/ocaml/cdrom/META
   /usr/lib/ocaml/cdrom/cdrom.a
   /usr/lib/ocaml/cdrom/cdrom.cma
   /usr/lib/ocaml/cdrom/cdrom.cmi
   /usr/lib/ocaml/cdrom/cdrom.cmx
   /usr/lib/ocaml/cdrom/cdrom.cmxa
   /usr/lib/ocaml/cdrom/dllcdrom_stubs.so
   /usr/lib/ocaml/cdrom/libcdrom_stubs.a
   /usr/lib/ocaml/close-and-exec/META
   /usr/lib/ocaml/close-and-exec/closeandexec.a
   /usr/lib/ocaml/close-and-exec/closeandexec.cma
   /usr/lib/ocaml/close-and-exec/closeandexec.cmi
   /usr/lib/ocaml/close-and-exec/closeandexec.cmx
   /usr/lib/ocaml/close-and-exec/closeandexec.cmxa
   /usr/lib/ocaml/http-svr/META
   /usr/lib/ocaml/http-svr/buf_io.cmi
   /usr/lib/ocaml/http-svr/buf_io.cmx
   /usr/lib/ocaml/http-svr/http.cmi
   /usr/lib/ocaml/http-svr/http.cmx
   /usr/lib/ocaml/http-svr/http_svr.a
   /usr/lib/ocaml/http-svr/http_svr.cma
   /usr/lib/ocaml/http-svr/http_svr.cmi
   /usr/lib/ocaml/http-svr/http_svr.cmx
   /usr/lib/ocaml/http-svr/http_svr.cmxa
   /usr/lib/ocaml/http-svr/server_io.cmi
   /usr/lib/ocaml/http-svr/server_io.cmx
   /usr/lib/ocaml/http-svr/http_client.cmi
   /usr/lib/ocaml/http-svr/http_client.cmx
   /usr/lib/ocaml/log/META
   /usr/lib/ocaml/log/debug.cmi
   /usr/lib/ocaml/log/debug.cmx
   /usr/lib/ocaml/log/dllsyslog_stubs.so
   /usr/lib/ocaml/log/libsyslog_stubs.a
   /usr/lib/ocaml/log/log.a
   /usr/lib/ocaml/log/log.cma
   /usr/lib/ocaml/log/log.cmi
   /usr/lib/ocaml/log/log.cmx
   /usr/lib/ocaml/log/log.cmxa
   /usr/lib/ocaml/log/logs.cmi
   /usr/lib/ocaml/log/logs.cmx
   /usr/lib/ocaml/log/syslog.cmi
   /usr/lib/ocaml/log/syslog.cmx
   /usr/lib/ocaml/lvm/META
   /usr/lib/ocaml/lvm/lvm.a
   /usr/lib/ocaml/lvm/lvm.cma
   /usr/lib/ocaml/lvm/lvm.cmi
   /usr/lib/ocaml/lvm/lvm.cmxa
   /usr/lib/ocaml/pciutil/META
   /usr/lib/ocaml/pciutil/pciutil.a
   /usr/lib/ocaml/pciutil/pciutil.cma
   /usr/lib/ocaml/pciutil/pciutil.cmi
   /usr/lib/ocaml/pciutil/pciutil.cmx
   /usr/lib/ocaml/pciutil/pciutil.cmxa
   /usr/lib/ocaml/rpc-light/META
   /usr/lib/ocaml/rpc-light/jsonrpc.cmi
   /usr/lib/ocaml/rpc-light/jsonrpc.cmo
   /usr/lib/ocaml/rpc-light/jsonrpc.cmx
   /usr/lib/ocaml/rpc-light/jsonrpc.o
   /usr/lib/ocaml/rpc-light/pa_rpc.cma
   /usr/lib/ocaml/rpc-light/rpc.cmi
   /usr/lib/ocaml/rpc-light/rpc.cmo
   /usr/lib/ocaml/rpc-light/rpc.cmx
   /usr/lib/ocaml/rpc-light/rpc.o
   /usr/lib/ocaml/rpc-light/xmlrpc.cmi
   /usr/lib/ocaml/rpc-light/xmlrpc.cmo
   /usr/lib/ocaml/rpc-light/xmlrpc.cmx
   /usr/lib/ocaml/rpc-light/xmlrpc.o
   /usr/lib/ocaml/rss/META
   /usr/lib/ocaml/rss/rss.a
   /usr/lib/ocaml/rss/rss.cma
   /usr/lib/ocaml/rss/rss.cmi
   /usr/lib/ocaml/rss/rss.cmx
   /usr/lib/ocaml/rss/rss.cmxa
   /usr/lib/ocaml/sexpr/META
   /usr/lib/ocaml/sexpr/sExpr.cmi
   /usr/lib/ocaml/sexpr/sExpr.cmx
   /usr/lib/ocaml/sexpr/sExprLexer.cmi
   /usr/lib/ocaml/sexpr/sExprLexer.cmx
   /usr/lib/ocaml/sexpr/sExprParser.cmi
   /usr/lib/ocaml/sexpr/sExprParser.cmx
   /usr/lib/ocaml/sexpr/sExpr_TS.cmi
   /usr/lib/ocaml/sexpr/sExpr_TS.cmx
   /usr/lib/ocaml/sexpr/sexpr.a
   /usr/lib/ocaml/sexpr/sexpr.cma
   /usr/lib/ocaml/sexpr/sexpr.cmxa
   /usr/lib/ocaml/stdext/META
   /usr/lib/ocaml/stdext/arrayext.cmi
   /usr/lib/ocaml/stdext/arrayext.cmx
   /usr/lib/ocaml/stdext/backtrace.cmi
   /usr/lib/ocaml/stdext/backtrace.cmx
   /usr/lib/ocaml/stdext/base64.cmi
   /usr/lib/ocaml/stdext/base64.cmx
   /usr/lib/ocaml/stdext/bigbuffer.cmi
   /usr/lib/ocaml/stdext/bigbuffer.cmx
   /usr/lib/ocaml/stdext/config.cmi
   /usr/lib/ocaml/stdext/config.cmx
   /usr/lib/ocaml/stdext/date.cmi
   /usr/lib/ocaml/stdext/date.cmx
   /usr/lib/ocaml/stdext/dllstdext_stubs.so
   /usr/lib/ocaml/stdext/encodings.cmi
   /usr/lib/ocaml/stdext/encodings.cmx
   /usr/lib/ocaml/stdext/extentlistSet.cmi
   /usr/lib/ocaml/stdext/extentlistSet.cmx
   /usr/lib/ocaml/stdext/extentlistset_test.cmx
   /usr/lib/ocaml/stdext/fe.cmi
   /usr/lib/ocaml/stdext/fe.cmx
   /usr/lib/ocaml/stdext/fecomms.cmi
   /usr/lib/ocaml/stdext/fecomms.cmx
   /usr/lib/ocaml/stdext/filenameext.cmi
   /usr/lib/ocaml/stdext/filenameext.cmx
   /usr/lib/ocaml/stdext/forkhelpers.cmi
   /usr/lib/ocaml/stdext/forkhelpers.cmx
   /usr/lib/ocaml/stdext/fring.cmi
   /usr/lib/ocaml/stdext/fring.cmx
   /usr/lib/ocaml/stdext/fun.cmi
   /usr/lib/ocaml/stdext/fun.cmx
   /usr/lib/ocaml/stdext/gzip.cmi
   /usr/lib/ocaml/stdext/gzip.cmx
   /usr/lib/ocaml/stdext/hashtblext.cmi
   /usr/lib/ocaml/stdext/hashtblext.cmx
   /usr/lib/ocaml/stdext/int64ext.cmi
   /usr/lib/ocaml/stdext/int64ext.cmx
   /usr/lib/ocaml/stdext/libstdext_stubs.a
   /usr/lib/ocaml/stdext/lazyList.cmi
   /usr/lib/ocaml/stdext/lazyList.cmx
   /usr/lib/ocaml/stdext/listext.cmi
   /usr/lib/ocaml/stdext/listext.cmx
   /usr/lib/ocaml/stdext/mapext.cmi
   /usr/lib/ocaml/stdext/mapext.cmx
   /usr/lib/ocaml/stdext/monad.cmi
   /usr/lib/ocaml/stdext/monad.cmx
   /usr/lib/ocaml/stdext/opt.cmi
   /usr/lib/ocaml/stdext/opt.cmx
   /usr/lib/ocaml/stdext/pervasiveext.cmi
   /usr/lib/ocaml/stdext/pervasiveext.cmx
   /usr/lib/ocaml/stdext/qring.cmi
   /usr/lib/ocaml/stdext/qring.cmx
   /usr/lib/ocaml/stdext/range.cmi
   /usr/lib/ocaml/stdext/range.cmx
   /usr/lib/ocaml/stdext/ring.cmi
   /usr/lib/ocaml/stdext/ring.cmx
   /usr/lib/ocaml/stdext/set_test.cmi
   /usr/lib/ocaml/stdext/set_test.cmx
   /usr/lib/ocaml/stdext/sha1sum.cmi
   /usr/lib/ocaml/stdext/sha1sum.cmx
   /usr/lib/ocaml/stdext/stdext.a
   /usr/lib/ocaml/stdext/stdext.cma
   /usr/lib/ocaml/stdext/stdext.cmxa
   /usr/lib/ocaml/stdext/stringext.cmi
   /usr/lib/ocaml/stdext/stringext.cmx
   /usr/lib/ocaml/stdext/tar.cmi
   /usr/lib/ocaml/stdext/tar.cmx
   /usr/lib/ocaml/stdext/threadext.cmi
   /usr/lib/ocaml/stdext/threadext.cmx
   /usr/lib/ocaml/stdext/trie.cmi
   /usr/lib/ocaml/stdext/trie.cmx
   /usr/lib/ocaml/stdext/unixext.cmi
   /usr/lib/ocaml/stdext/unixext.cmx
   /usr/lib/ocaml/stdext/vIO.cmi
   /usr/lib/ocaml/stdext/vIO.cmx
   /usr/lib/ocaml/stdext/zerocheck.cmi
   /usr/lib/ocaml/stdext/zerocheck.cmx
   /usr/lib/ocaml/stdext/either.cmi
   /usr/lib/ocaml/stdext/either.cmx
   /usr/lib/ocaml/stdext/os.cmi
   /usr/lib/ocaml/stdext/os.cmx
   /usr/lib/ocaml/stunnel/META
   /usr/lib/ocaml/stunnel/stunnel.a
   /usr/lib/ocaml/stunnel/stunnel.cma
   /usr/lib/ocaml/stunnel/stunnel.cmi
   /usr/lib/ocaml/stunnel/stunnel.cmx
   /usr/lib/ocaml/stunnel/stunnel.cmxa
   /usr/lib/ocaml/stunnel/stunnel_cache.cmi
   /usr/lib/ocaml/stunnel/stunnel_cache.cmx
   /usr/lib/ocaml/uuid/META
   /usr/lib/ocaml/uuid/uuid.a
   /usr/lib/ocaml/uuid/uuid.cma
   /usr/lib/ocaml/uuid/uuid.cmi
   /usr/lib/ocaml/uuid/uuid.cmx
   /usr/lib/ocaml/uuid/uuid.cmxa
   /usr/lib/ocaml/xen-utils/META
   /usr/lib/ocaml/xen-utils/xen-utils.a
   /usr/lib/ocaml/xen-utils/xen-utils.cma
   /usr/lib/ocaml/xen-utils/xen-utils.cmxa
   /usr/lib/ocaml/xen-utils/xen_cmdline.cmi
   /usr/lib/ocaml/xen-utils/xen_cmdline.cmx
   /usr/lib/ocaml/xml-light2/META
   /usr/lib/ocaml/xml-light2/xml-light2.a
   /usr/lib/ocaml/xml-light2/xml-light2.cma
   /usr/lib/ocaml/xml-light2/xml-light2.cmxa
   /usr/lib/ocaml/xml-light2/xml.cmi
   /usr/lib/ocaml/xml-light2/xml.cmx
   /usr/lib/ocaml/vhd/META
   /usr/lib/ocaml/vhd/dllvhd_stubs.so
   /usr/lib/ocaml/vhd/libvhd_stubs.a
   /usr/lib/ocaml/vhd/vhd.a
   /usr/lib/ocaml/vhd/vhd.cma
   /usr/lib/ocaml/vhd/vhd.cmi
   /usr/lib/ocaml/vhd/vhd.cmx
   /usr/lib/ocaml/vhd/vhd.cmxa

%exclude /usr/lib/ocaml/close-and-exec/closeandexec_main.cmx
%exclude /usr/lib/ocaml/pciutil/pciutil_main.cmx
%exclude /usr/lib/ocaml/sexpr/sexprpp.cmx
%exclude /usr/lib/ocaml/stdext/base64_main.cmx
%exclude /usr/lib/ocaml/stdext/fe_cli.cmx
%exclude /usr/lib/ocaml/stdext/fe_test.cmx
%exclude /usr/lib/ocaml/xml-light2/xmlpp.cmx
%exclude /usr/lib/ocaml/*/*.sp?t

   /usr/lib/ocaml/cpuid/META
   /usr/lib/ocaml/cpuid/cpuid.a
   /usr/lib/ocaml/cpuid/cpuid.cma
   /usr/lib/ocaml/cpuid/cpuid.cmi
   /usr/lib/ocaml/cpuid/cpuid.cmx
   /usr/lib/ocaml/cpuid/cpuid.cmxa
   /usr/lib/ocaml/cpuid/dllcpuid_stubs.so
   /usr/lib/ocaml/cpuid/libcpuid_stubs.a
   /usr/lib/ocaml/tapctl/META
   /usr/lib/ocaml/tapctl/tapctl.a
   /usr/lib/ocaml/tapctl/tapctl.cma
   /usr/lib/ocaml/tapctl/tapctl.cmi
   /usr/lib/ocaml/tapctl/tapctl.cmx
   /usr/lib/ocaml/tapctl/tapctl.cmxa
   /usr/lib/ocaml/netdev/*
   /usr/lib/ocaml/eventchn/META
   /usr/lib/ocaml/eventchn/dlleventchn_stubs.so
   /usr/lib/ocaml/eventchn/eventchn.a
   /usr/lib/ocaml/eventchn/eventchn.cma
   /usr/lib/ocaml/eventchn/eventchn.cmi
   /usr/lib/ocaml/eventchn/eventchn.cmx
   /usr/lib/ocaml/eventchn/eventchn.cmxa
   /usr/lib/ocaml/eventchn/libeventchn_stubs.a
   /usr/lib/ocaml/mmap/META
   /usr/lib/ocaml/mmap/dllmmap_stubs.so
   /usr/lib/ocaml/mmap/libmmap_stubs.a
   /usr/lib/ocaml/mmap/mmap.a
   /usr/lib/ocaml/mmap/mmap.cma
   /usr/lib/ocaml/mmap/mmap.cmi
   /usr/lib/ocaml/mmap/mmap.cmx
   /usr/lib/ocaml/mmap/mmap.cmxa
   /usr/lib/ocaml/xb/META
   /usr/lib/ocaml/xb/dllxb_stubs.so
   /usr/lib/ocaml/xb/libxb_stubs.a
   /usr/lib/ocaml/xb/op.cmi
   /usr/lib/ocaml/xb/op.cmx
   /usr/lib/ocaml/xb/packet.cmi
   /usr/lib/ocaml/xb/packet.cmx
   /usr/lib/ocaml/xb/partial.cmx
   /usr/lib/ocaml/xb/xb.a
   /usr/lib/ocaml/xb/xb.cma
   /usr/lib/ocaml/xb/xb.cmi
   /usr/lib/ocaml/xb/xb.cmx
   /usr/lib/ocaml/xb/xb.cmxa
   /usr/lib/ocaml/xb/xs_ring.cmx
   /usr/lib/ocaml/xc/META
   /usr/lib/ocaml/xc/dllxc_stubs.so
   /usr/lib/ocaml/xc/libxc_stubs.a
   /usr/lib/ocaml/xc/xc.a
   /usr/lib/ocaml/xc/xc.cma
   /usr/lib/ocaml/xc/xc.cmi
   /usr/lib/ocaml/xc/xc.cmx
   /usr/lib/ocaml/xc/xc.cmxa
   /usr/lib/ocaml/xs/META
   /usr/lib/ocaml/xs/queueop.cmx
   /usr/lib/ocaml/xs/xs.a
   /usr/lib/ocaml/xs/xs.cma
   /usr/lib/ocaml/xs/xs.cmi
   /usr/lib/ocaml/xs/xs.cmx
   /usr/lib/ocaml/xs/xs.cmxa
   /usr/lib/ocaml/xs/xs.mli
   /usr/lib/ocaml/xs/xsraw.cmi
   /usr/lib/ocaml/xs/xsraw.cmx
   /usr/lib/ocaml/xs/xsraw.mli
   /usr/lib/ocaml/xs/xst.cmi
   /usr/lib/ocaml/xs/xst.cmx
   /usr/lib/ocaml/xs/xst.mli
   /usr/lib/ocaml/xsrpc/META
   /usr/lib/ocaml/xsrpc/xsrpc.a
   /usr/lib/ocaml/xsrpc/xsrpc.cma
   /usr/lib/ocaml/xsrpc/xsrpc.cmi
   /usr/lib/ocaml/xsrpc/xsrpc.cmx
   /usr/lib/ocaml/xsrpc/xsrpc.cmxa

%changelog
