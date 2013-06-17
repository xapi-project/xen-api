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

start "Citrix XenServer PowerShell SnapIn" /B C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe -PSConsoleFile XenServerPSSnapIn.psc1 -Noexit -Nologo -command "if ($(Get-ExecutionPolicy) -ne \"Restricted\") { . .\Initialize-Environment.ps1 } else { Write-Warning \"ExecutionPolicy is set to Restricted.  Some XenServer commands are disabled.\" } ; %*"
exit
