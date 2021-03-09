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

PROFILE=release

build:
	dune build --profile=$(PROFILE) \
		c/gen_c_binding.exe \
		csharp/gen_csharp_binding.exe \
		java/main.exe \
		powershell/gen_powershell_binding.exe

clean:
	dune clean

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --inplace

.PHONY: build clean reindent

c:
	dune build --profile=$(PROFILE) -f @c/generate

csharp:
	dune build --profile=$(PROFILE) -f @csharp/generate
	sh windows-line-endings.sh _build/default/csharp/autogen

java:
	dune build --profile=$(PROFILE) -f @java/generate

powershell:
	dune build --profile=$(PROFILE) -f @powershell/generate
	sh windows-line-endings.sh _build/default/powershell/autogen


sdk_all: c csharp java powershell

.PHONY: sdk_all c csharp java powershell

.DEFAULT_GOAL := sdk_all
