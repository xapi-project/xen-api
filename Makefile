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

SDK_MICRO_VERSION = $(PRODUCT_MICRO_VERSION)

ifneq ($(SDK_MICRO_VERSION_OVERRIDE),)
	SDK_MICRO_VERSION = $(SDK_MICRO_VERSION_OVERRIDE)
endif

SDK_VERSION = $(PRODUCT_MAJOR_VERSION).$(PRODUCT_MINOR_VERSION).$(SDK_MICRO_VERSION)
export SDK_VERSION
export SR_XML

.PHONY: build
build: clean
	omake dist

.PHONY: clean
clean:
	omake clean
