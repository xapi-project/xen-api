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


Param([Parameter(Mandatory=$true)][String]$svr,
        [Parameter(Mandatory=$true)][String]$usr,
        [Parameter(Mandatory=$true)][String]$pwd,
        [Parameter(Mandatory=$true)][String]$patchPath)

### Connect to a server

[Net.ServicePointManager]::SecurityProtocol='tls,tls11,tls12'
Connect-XenServer -Server $svr -UserName $usr -Password $pwd


### Create a VM

$template = @(Get-XenVM -Name 'Windows *' | where {$_.is_a_template})[0]

Invoke-XenVM -VM $template -XenAction Clone -NewName "testVM" -Async `
             -PassThru | Wait-XenTask -ShowProgress
  
$vm = Get-XenVM -Name "testVM"  
$sr = Get-XenSR -Ref (Get-XenPool).default_SR
$other_config = $vm.other_config
$other_config["disks"] = $other_config["disks"].Replace('sr=""', 'sr="{0}"' -f $sr.uuid)

New-XenVBD -VM $vm -VDI $null -Userdevice 3 -Bootable $false -Mode RO `
           -Type CD -Unpluggable $true -Empty $true -OtherConfig @{} `
           -QosAlgorithmType "" -QosAlgorithmParams @{}

Set-XenVM -VM $vm -OtherConfig $other_config
Invoke-XenVM -VM $vm -XenAction Provision -Async -PassThru | Wait-XenTask -ShowProgress


# Export the VM using the DataCopiedDelegate parameter to track bytes received

$path = $env:TEMP + "\vm.xva"

$trackDataReceived = [XenAPI.HTTP+DataCopiedDelegate]{
    param($bytes);
    Write-Host "Bytes received: $bytes" }

Export-XenVm -XenHost $svr -Uuid $vm.uuid -Path $path -DataCopiedDelegate $trackDataReceived

$vm | Remove-XenVM


### Import the previously exported VM using the ProgressDelegate parameter to track send progress

$trackProgress = [XenAPI.HTTP+UpdateProgressDelegate]{
    param($percent);
    Write-Progress -Activity "Importing Vm..." -PercentComplete $percent }

Import-XenVm -XenHost $svr -Path $path -ProgressDelegate $trackProgress


### Upload a patch

$trackProgress = [XenAPI.HTTP+UpdateProgressDelegate]{
    param($percent);
    Write-Progress -Activity "Uploading patch..." -PercentComplete $percent }

Send-XenPoolPatch -XenHost $svr -Path $patchPath


### Get host RRDs

$path = $env:TEMP + "\rrd.xml"
Receive-XenHostRrd -XenHost $svr -Path $path -DataCopiedDelegate $trackDataReceived


### Disconnect before finishing

Get-XenSession | Disconnect-XenServer
