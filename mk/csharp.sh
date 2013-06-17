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
MSBUILD="$FRAMEWORKDIR/MSBuild.exe"
SDKDIR="${ROOT}/Program Files/Microsoft SDKs/Windows/v6.0A/bin"
RESGEN="$SDKDIR/ResGen.exe"

# see http://www.interact-sw.co.uk/iangblog/2005/09/12/cmdspawnerror
SYSPATHS="${ROOT}/WINDOWS/:${ROOT}/WINDOWS/System32:${ROOT}/WINDOWS/System32/wbem"

#SKIP_SIGNING=yes

remote_cmd_passwd2 "cd ${TMPDIR}/XenServer.NET/ && \"${RESGEN}\" FriendlyErrorNames.resx /str:cs,XenAPI,FriendlyErrorNames,FriendlyErrorNames.Designer.cs /publicClass"
remote_cmd_passwd2 "cd ${TMPDIR}/XenServer.NET/ && ${MSBUILD} XenServer.csproj /t:Build /p:Configuration=Release"
remote_cmd_passwd2 "cp ${TMPDIR}/XenServer.NET/bin/Release/XenServer.dll ${TMPDIR}"
remote_cmd_passwd2 "cp ${TMPDIR}/XenServer.NET/FriendlyErrorNames.Designer.cs ${TMPDIR}"

if [ "$SKIP_SIGNING" != "yes" ]
then
    remote_cmd_passwd2 "cd ${TMPDIR} && ${CMD_EXEC} sign.bat XenServer.dll 'XenServer.NET'"
fi

EXTRA_FILES="XenServer.dll FriendlyErrorNames.Designer.cs"
