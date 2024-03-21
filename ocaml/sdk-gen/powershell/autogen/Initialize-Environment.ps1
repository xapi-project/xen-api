#
# Copyright (c) Cloud Software Group, Inc.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#   1) Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#   2) Redistributions in binary form must reproduce the above
#      copyright notice, this list of conditions and the following
#      disclaimer in the documentation and/or other materials
#      provided with the distribution.
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


if ((Test-Path variable:XenServer_Environment_Initialized) -eq $true) {
    return
}

$systemWideXsProfile = Join-Path -Path $PSHOME -ChildPath "XenServerProfile.ps1"
$perUserXsProfile = Join-Path -Path (Split-Path $PROFILE) -ChildPath "XenServerProfile.ps1"

if (Test-Path $systemWideXsProfile) {
    . $systemWideXsProfile
}

if (Test-Path $perUserXsProfile) {
    . $perUserXsProfile
}

Remove-Item variable:systemWideXsProfile
Remove-Item variable:perUserXsProfile

$global:KnownServerCertificatesFilePath = Join-Path -Path (Split-Path $PROFILE) -ChildPath "XenServer_Known_Certificates.xml"

$XenServer_Environment_Initialized = $true
