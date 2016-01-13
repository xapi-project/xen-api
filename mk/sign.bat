@echo off
rem Copyright (c) Citrix Systems Inc.
rem All rights reserved.
rem
rem Redistribution and use in source and binary forms,
rem with or without modification, are permitted provided
rem that the following conditions are met:
rem
rem *   Redistributions of source code must retain the above
rem     copyright notice, this list of conditions and the
rem     following disclaimer.
rem *   Redistributions in binary form must reproduce the above
rem     copyright notice, this list of conditions and the
rem     following disclaimer in the documentation and/or other
rem     materials provided with the distribution.
rem
rem THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
rem CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
rem INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
rem MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
rem DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
rem CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
rem SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
rem BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
rem SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
rem INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
rem WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
rem NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
rem OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
rem SUCH DAMAGE.

set ddk_path=noDDK
if exist c:\winddk\6001.18001 set ddk_path=c:\winddk\6001.18001
if exist c:\winddk\6000 set ddk_path=c:\winddk\6000
signtool sign /? >nul 2>&1 && (where signtool & set ddk_path=SigntoolInPath) || (echo "Unable to find signtool in the path.")

if "%ddk_path%" == "noDDK" goto no_ddk
goto found_ddk

:no_ddk
echo "Cannot find a DDK in either c:\winddk\6000 or c:\winddk\6001.18001"
goto end

:found_ddk
rem do not display this because the tool is called too many times and it pollutes the output.
if defined debug (echo ddk is %ddk_path%)

set descr="Citrix XenCenter"
set timestamp_sha1=http://timestamp.verisign.com/scripts/timestamp.dll
set timestamp_sha2=http://timestamp.geotrust.com/tsa
set check_msi=%1
if not [%2]==[] set descr=%2

if /I not "%ddk_path%" == "SigntoolInPath" (
    %ddk_path%\bin\catalog\signtool.exe sign -a -s my -n "Citrix Systems, Inc" -d `\"%descr%`\" -t %timestamp_sha1% %1
) else (
    if /I not x%check_msi:.msi=% == x%check_msi% (
        signtool sign -v -sm -sha1 3d502d724093ef56499e24ce9c664a3471382ea9 -d `\"%descr%`\" -tr %timestamp_sha2% -td sha256 %1
        goto end
    ) else (
        C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe -Command "$c = Get-ChildItem -Path cert:\\CurrentUser\\My, cert:\\LocalMachine\\My | Where-Object { $_.Thumbprint -like \"9c0ac20110aaf4733fc0eae7ec3ec8d5cc74dbcc\" }; If ($c) { signtool sign -v -sm -sha1 3d502d724093ef56499e24ce9c664a3471382ea9 -d `\"%descr%`\" -t %timestamp_sha1% %1; signtool sign -v -sm -as -sha1 9c0ac20110aaf4733fc0eae7ec3ec8d5cc74dbcc -d `\"%descr%`\" -tr %timestamp_sha2% -td sha256 %1 } else {signtool sign -v -s -sha1 0699c0e67181f87ecdf7a7a6ad6f4481ee6c76cf -d `\"%descr%`\" -tr %timestamp_sha1% %1 ; }" <NUL
    )
)
:end
