#
# Copyright (c) Citrix Systems, Inc.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#  1) Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#
#  2) Redistributions in binary form must reproduce the above
#     copyright notice, this list of conditions and the following
#     disclaimer in the documentation and/or other materials
#     provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGE.
#

ifndef SDK_VERSION
	SDK_VERSION=0.0.0
endif

ifndef PRODUCT_GUID
	PRODUCT_GUID=0
endif

ifndef SR_XML
	SR_XML=XE_SR_ERRORCODES.xml
endif

API_MAJOR=2
API_MINOR=7

# OASIS_START

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data: setup.ml
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

setup.ml: _oasis
	oasis setup -setup-update dynamic

# executables

gen_c_binding.native: build
gen_csharp_binding.native: build
main.native: build
gen_powershell_binding.native: build

# bindings

c: gen_c_binding.native
	mkdir -p _build/c/autogen/include/xen
	mkdir -p _build/c/autogen/src
	mkdir -p _build/c/autogen/test
	./gen_c_binding.native -d _build/c/autogen
#source
	(cd _build/c/autogen && patch -p0 < ../../../c/compat.patch)
	rm _build/c/autogen/*.orig
	mv _build/c/autogen/xen _build/c/autogen/include
	mv _build/c/autogen/*.h _build/c/autogen/include
	mv _build/c/autogen/*.c _build/c/autogen/src
	cp c/xen_internal.h _build/c/autogen/include
	cp c/xen_common.h c/xen_string_set.h c/xen_int_set.h _build/c/autogen/include/xen/api
	cp c/xen_common.c c/xen_string_set.c c/xen_int_set.c _build/c/autogen/src
	sed -e s/@LIB_MAJOR@/$(API_MAJOR)/g -e s/@LIB_MINOR@/$(API_MINOR)/g c/Makefile.dist > _build/c/autogen/Makefile
	make -C _build/c/autogen -f Makefile ueberheader
#tests
	cp c/test/*.c _build/c/autogen/test
#other
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' c/README.dist > _build/c/autogen/README
	cp LICENSE _build/c/autogen/COPYING

csharp: gen_csharp_binding.native
	mkdir -p _build/csharp/autogen/src/Properties
	mkdir -p _build/csharp/autogen/gui
	mkdir -p _build/csharp/autogen/samples
	./gen_csharp_binding.native -r csharp/FriendlyErrorNames.resx -s $(SR_XML) -d _build/csharp/autogen
#source
	mv _build/csharp/autogen/XenObjectDownloader.cs _build/csharp/autogen/gui
	sh csharp/subst-autogen-csproj.sh _build/csharp/autogen csharp/src/XenServer.csproj _build/csharp/autogen/src/XenServer.csproj
	cp csharp/src/*.cs _build/csharp/autogen
	cp _build/csharp/autogen/*.* _build/csharp/autogen/src
	mv _build/csharp/autogen/src/AssemblyInfo.cs _build/csharp/autogen/src/Properties/AssemblyInfo.cs
	sed -i -e 's/1\.0\.0\.0/$(SDK_VERSION).0/g' _build/csharp/autogen/src/Properties/AssemblyInfo.cs
#source gui
	mv _build/csharp/autogen/*.* _build/csharp/autogen/gui
	rm _build/csharp/autogen/gui/AssemblyInfo.cs
#samples
	cp -r csharp/samples/* _build/csharp/autogen/samples
#other
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' csharp/README.dist > _build/csharp/autogen/README.txt
	cp LICENSE _build/csharp/autogen/LICENSE.txt
	sh windows-line-endings.sh _build/csharp/autogen

java: main.native
	mkdir -p _build/java/autogen/com/xensource/xenapi
	mkdir -p _build/java/autogen/samples
	./main.native _build/java/autogen
#source
	cp java/lib/com/xensource/xenapi/*.java _build/java/autogen/com/xensource/xenapi
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' java/lib/com/xensource/xenapi/Connection.java > _build/java/autogen/com/xensource/xenapi/Connection.java
#samples
	cp java/samples/*.java _build/java/autogen/samples
#other
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' java/lib/Makefile.dist > _build/java/autogen/Makefile
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' java/README.dist > _build/java/autogen/README.txt
	cp LICENSE _build/java/autogen/LICENSE.txt
	cp java/LICENSE.Apache-2.0.txt _build/java/autogen

powershell: gen_powershell_binding.native
	mkdir -p _build/powershell/autogen/src
	mkdir -p _build/powershell/autogen/samples
	./gen_powershell_binding.native _build/powershell/autogen
#source
	mv _build/powershell/autogen/*.cs _build/powershell/autogen/src
	cp powershell/src/*.cs _build/powershell/autogen/src
	sed -i -e 's/1\.0\.0\.0/$(SDK_VERSION)/g' -e 's/1000/$(SDK_VERSION).0/g' _build/powershell/autogen/src/AssemblyInfo.cs
#samples
	cp powershell/samples/*.ps1 _build/powershell/autogen/samples
#other
	cp powershell/*.ps1xml powershell/*.ps1 _build/powershell/autogen
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' \
	    -e "s/@PRODUCT_GUID@/$(PRODUCT_GUID)/g" \
	    powershell/XenServerPSModule.psd1 > _build/powershell/autogen/XenServerPSModule.psd1
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' powershell/about_XenServer.help.txt > _build/powershell/autogen/about_XenServer.help.txt
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' powershell/README.dist > _build/powershell/autogen/README.txt
	cp LICENSE _build/powershell/autogen/LICENSE.txt
	sh windows-line-endings.sh _build/powershell/autogen

sdk_all: c csharp java powershell

.PHONY: sdk_all c csharp java powershell

.DEFAULT_GOAL := sdk_all
