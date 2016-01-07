@echo off
rem Copyright (c) Citrix Systems, Inc.
rem All rights reserved.
rem 
rem Redistribution and use in source and binary forms, with or without
rem modification, are permitted provided that the following conditions
rem are met:
rem 
rem   1) Redistributions of source code must retain the above copyright
rem      notice, this list of conditions and the following disclaimer.
rem 
rem   2) Redistributions in binary form must reproduce the above
rem      copyright notice, this list of conditions and the following
rem      disclaimer in the documentation and/or other materials
rem      provided with the distribution.
rem 
rem THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
rem "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
rem LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
rem FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
rem COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
rem INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
rem (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
rem SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
rem HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
rem STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
rem ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
rem OF THE POSSIBILITY OF SUCH DAMAGE.

@echo on
if not exist C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe goto done

C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe -Command "$c = Get-ChildItem -Path cert:\\CurrentUser\\My, cert:\\LocalMachine\\My | Where-Object { $_.Thumbprint -like \"9c0ac20110aaf4733fc0eae7ec3ec8d5cc74dbcc\" }; If ($c) { signtool sign -sm -as -sha1 \"9c0ac20110aaf4733fc0eae7ec3ec8d5cc74dbcc\" -d \"%2\" \"%1\"; signtool sign -sm -as -sha1 /"3d502d724093ef56499e24ce9c664a3471382ea9/" -d \"%2\" \"%1\" } else { signtool sign -s -as -sha1 /"0699c0e67181f87ecdf7a7a6ad6f4481ee6c76cf/" -d \"%2\" \"%1\" }"
