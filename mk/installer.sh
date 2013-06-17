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

# see http://www.interact-sw.co.uk/iangblog/2005/09/12/cmdspawnerror
SYSPATHS="${ROOT}/WINDOWS/:${ROOT}/WINDOWS/System32:${ROOT}/WINDOWS/System32/wbem"

if [ "$SKIP_SIGNING" != "yes" ]
then
    remote_cmd_passwd2 "cd ${TMPDIR} && ${CMD_EXEC} sign-ps.bat Initialize-Environment.ps1"
    remote_cmd_passwd2 "cd ${TMPDIR} && ${CMD_EXEC} sign-ps.bat XenServer.format.ps1xml"
    remote_cmd_passwd2 "cd ${TMPDIR} && ${CMD_EXEC} sign-ps.bat XenServer.types.ps1xml"
fi

remote_cmd_passwd2 "cd ${TMPDIR} && mkdir -p obj"
remote_cmd_passwd2 "cd ${TMPDIR} && ${TMPDIR}/candle.exe -out obj/ wixui/installdir/WixUI_InstallDir.wxs wixui/BrowseDlg.wxs wixui/CancelDlg.wxs wixui/Common.wxs wixui/CustomizeDlg.wxs wixui/DiskCostDlg.wxs wixui/ErrorDlg.wxs wixui/ErrorProgressText.wxs wixui/ExitDialog.wxs wixui/FatalError.wxs wixui/FilesInUse.wxs wixui/InstallDirDlg.wxs wixui/LicenseAgreementDlg.wxs wixui/MaintenanceTypeDlg.wxs wixui/MaintenanceWelcomeDlg.wxs wixui/MsiRMFilesInUse.wxs wixui/OutOfDiskDlg.wxs wixui/OutOfRbDiskDlg.wxs wixui/PrepareDlg.wxs wixui/ProgressDlg.wxs wixui/ResumeDlg.wxs wixui/SetupTypeDlg.wxs wixui/UserExit.wxs wixui/VerifyReadyDlg.wxs wixui/WaitForCostingDlg.wxs wixui/WelcomeDlg.wxs wixui/WelcomeEulaDlg.wxs"

remote_cmd_passwd2 "cd ${TMPDIR} && mkdir -p lib"
remote_cmd_passwd2 "cd ${TMPDIR} && ${TMPDIR}/lit.exe -out lib/WixUI_InstallDir.wixlib obj/WixUI_InstallDir.wixobj obj/BrowseDlg.wixobj obj/CancelDlg.wixobj obj/Common.wixobj obj/CustomizeDlg.wixobj obj/DiskCostDlg.wixobj obj/ErrorDlg.wixobj obj/ErrorProgressText.wixobj obj/ExitDialog.wixobj obj/FatalError.wixobj obj/FilesInUse.wixobj obj/InstallDirDlg.wixobj obj/LicenseAgreementDlg.wixobj obj/MaintenanceTypeDlg.wixobj obj/MaintenanceWelcomeDlg.wixobj obj/MsiRMFilesInUse.wixobj obj/OutOfDiskDlg.wixobj obj/OutOfRbDiskDlg.wixobj obj/PrepareDlg.wixobj obj/ProgressDlg.wixobj obj/ResumeDlg.wixobj obj/SetupTypeDlg.wixobj obj/UserExit.wixobj obj/VerifyReadyDlg.wixobj obj/WaitForCostingDlg.wixobj obj/WelcomeDlg.wixobj obj/WelcomeEulaDlg.wixobj"

remote_cmd_passwd2 "cd ${TMPDIR} && mkdir -p obj"
remote_cmd_passwd2 "cd ${TMPDIR} && ${TMPDIR}/candle.exe -out obj/ XenServerPSSnapIn.wxs"
remote_cmd_passwd2 "cd ${TMPDIR} && mkdir -p out"
remote_cmd_passwd2 "cd ${TMPDIR} && ${TMPDIR}/light.exe -out XenServerPSSnapIn.msi obj/XenServerPSSnapIn.wixobj lib/WixUI_InstallDir.wixlib -loc wixui/wixui_en-us.wxl"

if [ "$SKIP_SIGNING" != "yes" ]
then
    remote_cmd_passwd2 "cd ${TMPDIR} && chmod u+rwx XenServerPSSnapIn.msi"
    remote_cmd_passwd2 "cd ${TMPDIR} && ${CMD_EXEC} sign.bat XenServerPSSnapIn.msi 'Citrix XenServer PowerShell SnapIn'"
fi
