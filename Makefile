#
# Copyright (c) Citrix Systems, Inc.
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 
#	 1) Redistributions of source code must retain the above copyright
#			notice, this list of conditions and the following disclaimer.
# 
#	 2) Redistributions in binary form must reproduce the above
#			copyright notice, this list of conditions and the following
#			disclaimer in the documentation and/or other materials
#			provided with the distribution.
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

#==============================================================
#Micro version override - please keep at the top of the script
#==============================================================
#Uncomment and set to override the micro version obtained from branding
#SDK_MICRO_VERSION_OVERRIDE = 

USE_BRANDING := yes
USE_BUILD_NUMBER := yes
IMPORT_BRANDING := yes
IMPORT_VERSIONS := yes
include $(B_BASE)/common.mk
include $(B_BASE)/winbuild.mk

SDK_MICRO_VERSION = $(PRODUCT_MICRO_VERSION)

ifneq ($(SDK_MICRO_VERSION_OVERRIDE),)
	SDK_MICRO_VERSION = $(SDK_MICRO_VERSION_OVERRIDE)
endif

SDK_VERSION = $(PRODUCT_MAJOR_VERSION).$(PRODUCT_MINOR_VERSION).$(SDK_MICRO_VERSION)

REPONAME=xen-api-sdk
REPO=$(call git_loc,$(REPONAME))

API_SRC=$(MY_OBJ_DIR)/api-src
API_PYTHON=$(API_SRC)/xen-api-src/scripts/examples/python
PYTHON_FILES=XenAPI.py provision.py
PYTHON_SAMPLES=install.py powercycle.py permute.py vm_start_async.py watch-all-events.py fixpbds.py shell.py license.py

API_REF=$(MY_OBJ_DIR)/api-ref
API_REF_HTML=$(API_REF)/usr/share/doc/xapi/docs/html/xenserver
API_REF_PDF=$(API_REF)/usr/share/doc/xapi/docs/pdf

SDK_ZIP=XenServer-SDK-$(SDK_VERSION).zip
SDK_PY_FOLDER=$(MY_OUTPUT_DIR)/XenServer-SDK/XenServerPython
SDK_PY_FOLDER_SAMPLES=$(SDK_PY_FOLDER)/samples
API_REF_FOLDER=$(MY_OUTPUT_DIR)/XenServer-SDK/API-reference

export SDK_VERSION
export PROJECT_OUTPUTDIR

.PHONY: python-module
python-module:
	mkdir -p $(API_SRC) && tar -xf $(PROJECT_OUTPUTDIR)/api/SOURCES/xen-api-src.tar.bz2 -C $(API_SRC) xen-api-src/scripts/examples/python
	mkdir -p $(SDK_PY_FOLDER) && cp $(addprefix $(API_PYTHON)/, $(PYTHON_FILES)) $(SDK_PY_FOLDER)
	mkdir -p $(SDK_PY_FOLDER_SAMPLES) && cp $(addprefix $(API_PYTHON)/, $(PYTHON_SAMPLES) README) $(SDK_PY_FOLDER_SAMPLES)

.PHONY: api-ref
api-ref:
	mkdir -p $(API_REF) && tar -xf $(PROJECT_OUTPUTDIR)/api/sdk.tar.gz -C $(API_REF)
	mkdir -p $(API_REF_FOLDER)/html && cp -r $(API_REF_HTML)/* $(API_REF_FOLDER)/html
	mkdir -p $(API_REF_FOLDER)/pdf && cp $(API_REF_PDF)/xenenterpriseapi.pdf $(API_REF_FOLDER)/pdf/XenServerAPI.pdf

$(MY_OUTPUT_DIR)/$(SDK_ZIP): python-module api-ref
	cd $(REPO) && omake dist
	mkdir -p $(MY_OUTPUT_DIR)/XenServer-SDK/libxenserver
	mkdir -p $(MY_OUTPUT_DIR)/XenServer-SDK/XenServer.NET
	mkdir -p $(MY_OUTPUT_DIR)/XenServer-SDK/XenServerJava
	mkdir -p $(MY_OUTPUT_DIR)/XenServer-SDK/XenServerPowerShell
	cp -r $(REPO)/c/autogen/*          $(MY_OUTPUT_DIR)/XenServer-SDK/libxenserver
	cp -r $(REPO)/csharp/autogen/*     $(MY_OUTPUT_DIR)/XenServer-SDK/XenServer.NET
	cp -r $(REPO)/java/autogen/*       $(MY_OUTPUT_DIR)/XenServer-SDK/XenServerJava
	cp -r $(REPO)/powershell/autogen/* $(MY_OUTPUT_DIR)/XenServer-SDK/XenServerPowerShell
	cd $(MY_OUTPUT_DIR) && zip -q -r9 $(SDK_ZIP) XenServer-SDK
	ln -sf $(SDK_ZIP) $(MY_OUTPUT_DIR)/XenServer-SDK.zip
	rm -Rf $(MY_OUTPUT_DIR)/XenServer-SDK

.PHONY: build
build: clean $(MY_OUTPUT_DIR)/$(SDK_ZIP)

.PHONY: clean
clean:
	cd $(REPO) && omake clean
	rm -Rf $(API_SRC)
	rm -f $(MY_OBJ_DIR)/{*.gz,*.zip,*.msi,*.dll}
