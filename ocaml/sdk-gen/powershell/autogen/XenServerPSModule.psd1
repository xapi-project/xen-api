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

@{

#Module Info
ModuleVersion = '@SDK_VERSION@'
Description = 'XenServer PowerShell Module'
GUID = 'D695A8B9-039A-443C-99A4-0D48D7C6AD76'

#Copyright
Author = ''
CompanyName = 'Citrix Systems, Inc'
Copyright = 'Copyright (c) Citrix Systems, Inc. All rights reserved.'

# Requirements
PowerShellVersion = '7.2'
PowerShellHostName = ''
PowerShellHostVersion = ''
ProcessorArchitecture = 'None'

#Contents
ModuleToProcess = 'XenServerPowerShell.dll'
RequiredModules = @()
NestedModules = @()
RequiredAssemblies = @('Newtonsoft.Json.CH.dll',
                       'XenServer.dll')
ScriptsToProcess = @('Initialize-Environment.ps1')
TypesToProcess = @('XenServer.types.ps1xml')
FormatsToProcess = @('XenServer.format.ps1xml')
FileList = @('about_XenServer.help.txt',
             'Newtonsoft.Json.CH.dll',
             'Initialize-Environment.ps1',
             'LICENSE.Newtonsoft.Json.txt',
             'LICENSE.txt',
             'README.txt',
             'XenServer.dll',
             'XenServer.format.ps1xml',
             'XenServer.types.ps1xml',
             'XenServerPowerShell.dll',
             'XenServerPSModule.psd1')

#Public interface
FunctionsToExport = ''
CmdletsToExport = '*'
VariablesToExport = @('Citrix.XenServer.Sessions','XenServer_Default_Session')
AliasesToExport = '*'

PrivateData = ''

}
