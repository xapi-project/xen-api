#
# Copyright (c) Citrix Systems, Inc.
# All rights reserved.
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

#
# Don't customize this script -- your changes will be lost on upgrade.
#
# Instead, create and customize XenServerProfile.ps1.
# Put it in the same folder as this script for system-wide configuration, or
# %AppData%\Citrix\XenServerPSSnapIn for per-user configuration.
#

if (Get-Variable XenServer_Environment_Initialized -ValueOnly -ErrorAction SilentlyContinue)
{
    return
}

$XenServer_thisdir = Split-Path -Path $MyInvocation.MyCommand.Definition -Parent
$XenServer_appdata = [Environment]::GetFolderPath("ApplicationData") + "\Citrix\XenServerPSSnapIn"

Update-TypeData "$XenServer_thisdir\XenServer.types.ps1xml"
Update-FormatData "$XenServer_thisdir\XenServer.format.ps1xml"

cd $Home

if (Test-Path "$XenServer_thisdir\XenServerProfile.ps1")
{
    . "$XenServer_thisdir\XenServerProfile.ps1"
}

if (Test-Path "$XenServer_appdata\XenServerProfile.ps1")
{
    . "$XenServer_appdata\XenServerProfile.ps1"
}

Remove-Item variable:XenServer_thisdir
Remove-Item variable:XenServer_appdata

$XenServer_Environment_Initialized = $true
