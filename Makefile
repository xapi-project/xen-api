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

build:
	jbuilder build \
		c/gen_c_binding.exe \
		csharp/gen_csharp_binding.exe \
		java/main.exe \
		powershell/gen_powershell_binding.exe

clean:
	jbuilder clean

reindent:
	git ls-files '*.ml*' '**/*.ml*' | xargs ocp-indent --inplace

.PHONY: build clean reindent

c: build
	_build/default/c/gen_c_binding.exe -d _build/default/c/autogen -t c/templates
#source
	cp c/xen_common.h c/xen_string_set.h c/xen_int_set.h c/sources/xen_event_batch.h _build/default/c/autogen/include/xen/api
	cp c/xen_common.c c/xen_string_set.c c/xen_int_set.c c/sources/xen_event_batch.c _build/default/c/autogen/src
#tests
	mkdir -p _build/default/c/autogen/test
	cp c/test/*.c _build/default/c/autogen/test
#other
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' c/README.dist > _build/default/c/autogen/README
	cp LICENSE _build/default/c/autogen/COPYING

csharp: build
	mkdir -p _build/default/csharp/autogen/src/Properties
	mkdir -p _build/default/csharp/autogen/samples
	_build/default/csharp/gen_csharp_binding.exe -r csharp/FriendlyErrorNames.resx -s $(SR_XML) -d _build/default/csharp/autogen/src -t csharp/templates
#source
	cp csharp/src/*.cs _build/default/csharp/autogen/src
	mv _build/default/csharp/autogen/src/AssemblyInfo.cs _build/default/csharp/autogen/src/Properties/AssemblyInfo.cs
	sed -i -e 's/1\.0\.0\.0/$(SDK_VERSION).0/g' _build/default/csharp/autogen/src/Properties/AssemblyInfo.cs
#samples
	cp -r csharp/samples/* _build/default/csharp/autogen/samples
#other
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' csharp/README.dist > _build/default/csharp/autogen/README.txt
	cp LICENSE _build/default/csharp/autogen/LICENSE.txt
	sh windows-line-endings.sh _build/default/csharp/autogen

java: build
	mkdir -p _build/default/java/autogen/com/xensource/xenapi
	mkdir -p _build/default/java/autogen/samples
	_build/default/java/main.exe -d _build/default/java/autogen -t java/templates
#source
	cp java/lib/com/xensource/xenapi/*.java _build/default/java/autogen/com/xensource/xenapi
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' java/lib/com/xensource/xenapi/Connection.java > _build/default/java/autogen/com/xensource/xenapi/Connection.java
#samples
	cp java/samples/*.java _build/default/java/autogen/samples
#other
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' java/lib/Makefile.dist > _build/default/java/autogen/Makefile
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' java/README.dist > _build/default/java/autogen/README.txt
	cp LICENSE _build/default/java/autogen/LICENSE.txt
	cp java/LICENSE.Apache-2.0.txt _build/default/java/autogen

powershell: build
	mkdir -p _build/default/powershell/autogen/src/Properties
	mkdir -p _build/default/powershell/autogen/samples
	_build/default/powershell/gen_powershell_binding.exe -d _build/default/powershell/autogen/src -t powershell/templates
#source
	cp powershell/src/*.cs _build/default/powershell/autogen/src
	mv _build/default/powershell/autogen/src/AssemblyInfo.cs _build/default/powershell/autogen/src/Properties/AssemblyInfo.cs
	sed -i -e 's/1\.0\.0\.0/$(SDK_VERSION)/g' -e 's/1000/$(SDK_VERSION).0/g' _build/default/powershell/autogen/src/Properties/AssemblyInfo.cs
#samples
	cp powershell/samples/*.ps1 _build/default/powershell/autogen/samples
#other
	cp powershell/*.ps1xml powershell/*.ps1 _build/default/powershell/autogen
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' \
	    -e "s/@PRODUCT_GUID@/$(PRODUCT_GUID)/g" \
	    powershell/XenServerPSModule.psd1 > _build/default/powershell/autogen/XenServerPSModule.psd1
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' powershell/about_XenServer.help.txt > _build/default/powershell/autogen/about_XenServer.help.txt
	sed -e 's/@SDK_VERSION@/$(SDK_VERSION)/g' powershell/README.dist > _build/default/powershell/autogen/README.txt
	cp LICENSE _build/default/powershell/autogen/LICENSE.txt
	sh windows-line-endings.sh _build/default/powershell/autogen

sdk_all: c csharp java powershell

.PHONY: sdk_all c csharp java powershell

.DEFAULT_GOAL := sdk_all
