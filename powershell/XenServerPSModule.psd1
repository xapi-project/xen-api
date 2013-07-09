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
ModuleVersion = '@PRODUCT_VERSION@'
Description = 'XenServer PowerShell Module'
GUID = '@PRODUCT_GUID@'

#Copyright
Author = ''
CompanyName = 'Citrix Systems, Inc'
Copyright = 'Copyright (c) Citrix Systems, Inc. All rights reserved.'

# Requirements
PowerShellVersion = '2.0'
PowerShellHostName = ''
PowerShellHostVersion = ''
DotNetFrameworkVersion = '3.5'
CLRVersion = '2.0'
ProcessorArchitecture = 'None'

#Contents
ModuleToProcess = 'XenServerPowerShell.dll'
RequiredModules = @()
NestedModules = @()
ModuleList = @('XenServerPowerShell.dll')
RequiredAssemblies = @('CookComputing.XmlRpcV2.dll','XenServer.dll')
ScriptsToProcess = @('Initialize-Environment.ps1')
TypesToProcess = @('XenServer.types.ps1xml')
FormatsToProcess = @('XenServer.format.ps1xml')
FileList = @('about_XenServer.help.txt',
             'AutomatedTestCore.ps1',
             'CookComputing.XmlRpcV2.dll',
             'Initialize-Environment.ps1',
             'LICENSE.CookComputing.XmlRpcV2',
             'LICENSE.txt',
             'README.txt',
             'XenServer.dll',
             'XenServer.format.ps1xml',
             'XenServer.types.ps1xml',
             'XenServerPowerShell.psd1',
             'XenServerPowerShell.dll')

#Public interface
DefaultCommandPrefix = ''
FunctionsToExport = ''
CmdletsToExport = '*'
VariablesToExport = @('Citrix.XenServer.Sessions','XenServer_Default_Session')
AliasesToExport = '*'

PrivateData = ''

HelpInfoURI = ''

}
