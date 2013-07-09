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

set -e

FRAMEWORKDIR="${ROOT}/WINDOWS/Microsoft.NET/Framework/v3.5"
CSC="$FRAMEWORKDIR/csc.exe"
# see http://www.interact-sw.co.uk/iangblog/2005/09/12/cmdspawnerror
SYSPATHS="${ROOT}/WINDOWS/:${ROOT}/WINDOWS/System32:${ROOT}/WINDOWS/System32/wbem"

AUTOMATION_DLL="${TMPDIR}/System.Management.Automation.dll"
XAPI_DLL="${TMPDIR}/XenServer.dll"
XMLRPC_DLL="${TMPDIR}/CookComputing.XmlRpcV2.dll"
PSDIR="$TMPDIR/XenServerPowerShell"

remote_cmd_passwd2 "cp ${AUTOMATION_DLL} ${PSDIR}"
remote_cmd_passwd2 "cp ${XMLRPC_DLL} ${PSDIR}"
remote_cmd_passwd2 "cp ${XAPI_DLL} ${PSDIR}"
remote_cmd_passwd2 "cd ${PSDIR} && ${CSC} /target:library /out:XenServerPowerShell.dll /r:System.Management.Automation.dll /R:XenServer.dll /R:CookComputing.XmlRpcV2.dll *.cs"
remote_cmd_passwd2 "cp ${PSDIR}/XenServerPowerShell.dll ${TMPDIR}"

if [ "$SKIP_SIGNING" != "yes" ]
then
    remote_cmd_passwd2 "cd ${TMPDIR} && ${CMD_EXEC} sign.bat XenServerPowerShell.dll 'Citrix XenServer PowerShell Module'"
    remote_cmd_passwd2 "cd ${TMPDIR} && ${CMD_EXEC} sign-ps.bat Initialize-Environment.ps1"
    remote_cmd_passwd2 "cd ${TMPDIR} && ${CMD_EXEC} sign-ps.bat AutomatedTestCore.ps1"
    remote_cmd_passwd2 "cd ${TMPDIR} && ${CMD_EXEC} sign-ps.bat XenServer.format.ps1xml"
    remote_cmd_passwd2 "cd ${TMPDIR} && ${CMD_EXEC} sign-ps.bat XenServer.types.ps1xml"
fi

EXTRA_FILES="AutomatedTestCore.ps1 Initialize-Environment.ps1 XenServer.format.ps1xml XenServer.types.ps1xml"
