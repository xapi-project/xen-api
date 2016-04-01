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

# Powershell Automated Tests

Param([Parameter(Mandatory=$true)][String]$out_xml,
       [Parameter(Mandatory=$true)][String]$svr,
       [Parameter(Mandatory=$true)][String]$usr,
       [Parameter(Mandatory=$true)][String]$pwd,
       [Parameter(Mandatory=$true)][String]$sr_svr,
       [Parameter(Mandatory=$true)][String]$sr_path)

# Initial Setup

[Net.ServicePointManager]::SecurityProtocol='tls,tls11,tls12'
$BestEffort = $false
$NoWarnCertificates = $true
$info = $true
$warn = $true
$err = $true
$prog = $false

$Eap = $ErrorActionPreference
$Vp = $VerbosePreference
$Wp = $WarningPreference
$Ep = $ErrorPreference

$ErrorActionPreference = "Stop"
$VerbosePreference="Continue"
$WarningPreference="Continue"
$ErrorPreference="Continue"
$ErrorVariable

# End Initial Setup

# Helper Functions

function log_info([String]$msg)
{
  process
  {
    if($info)
	{
	  write-verbose $msg
	}
  }
}

function log_warn([String]$msg)
{
  process
  {
    if($warn)
	{
      write-warning $msg
	}
  }
}

function log_error([String]$msg)
{
  process
  {
    if($err) 
	{
      write-error $msg
	}
  }
}

function escape_for_xml([String]$content)
{
  return $content.replace("&", "&amp;").replace("'", "&apos;").replace('"', "&quot;").replace("<", "&lt;").replace(">", "&gt;")
}

function prep_xml_output([String]$out_file)
{
  $date = Get-Date
  "<results>" > $out_file
  ("<testrun>Test Run Info: PowerShell bindings test {0}</testrun>" -f $date) >> $out_file
  "<group>" >> $out_file
}

function close_xml_output([String]$out_file)
{
  "</group>" >> $out_file
  "</results>" >> $out_file
}

function add_result([String]$out_file,[String]$cmd, [String]$test_name, [Exception]$err)
{
  $out_cmd = escape_for_xml $cmd
  $out_test_name = escape_for_xml $test_name
  $out_err = escape_for_xml $err
  "<test>" >> $out_file
  ("<name>{0}</name>" -f $out_test_name) >> $out_file
  if (($err -ne $null))
  {
    "<state>Fail</state>" >> $out_file
	"<log>" >> $out_file
    ("Cmd: '{0}'" -f $out_cmd) >> $out_file
	("Exception: {0}" -f $out_err) >> $out_file
	"</log>" >> $out_file
  }
  else
  {
    "<state>Pass</state>" >> $out_file
    "<log />" >> $out_file
  }
  "</test>" >> $out_file
}


function exec([String]$test_name, [String]$cmd, [String]$expected)
{
  trap [Exception]
  {
     add_result $out_xml $cmd $test $_.Exception
	 $fails.Add($test_name, $_.Exception)
	 break
  }
  
  log_info ("Test '{0}' Started: cmd = {1}, expected = {2}" -f $test_name,$cmd,$expected)
  $result = Invoke-Expression $cmd
  if ($result -eq $expected)
  {
    add_result $out_xml $cmd $test_name $null
	return $true
  }
  else
  {
    $exc = new-object Exception("Test '{0}' Failed: expected '{1}'; actual '{2}'" `
                                -f $test_name,$expected,$result)
    add_result $out_xml $cmd $test_name $exc
	$fails.Add($test_name, $exc)
	return $false
  }
}

# End Helper Functions

# Connect Functions

function connect_server([String]$svr, [String]$usr, [String]$pwd)
{
  log_info ("connecting to server '{0}'" -f $svr)
  $session = Connect-XenServer -Server $svr -UserName $usr -Password $pwd -PassThru

  if($session -eq $null)
  {
    return $false
  }
  return $true

}

function disconnect_server([String]$svr)
{
  log_info ("disconnecting from server '{0}'" -f $svr)
  Get-XenSession -Server $svr | Disconnect-XenServer

  if ((Get-XenSession -Server $svr) -eq $null)
  {
    return $true
  }
  return $false
}

# End Connect Functions

# VM Functions

function destroy_vm([XenAPI.VM]$vm)
{
  if ($vm -eq $null)
  {
    return
  }

  log_info ("destroying vm '{0}'" -f $vm.name_label)
  
  $vdis = @()
  
  foreach($vbd in $vm.VBDs)
  {
	if((Get-XenVBDProperty -Ref $vbd -XenProperty Mode) -eq [XenAPI.vbd_mode]::RW)
	{
      $vdis += Get-XenVBDProperty -Ref $vbd -XenProperty VDI
	}
  }
  
  Remove-XenVM -VM $vm -Async -PassThru | Wait-XenTask -ShowProgress
  
  foreach($vdi in $vdis)
  {
    Remove-XenVDI -VDI $vdi -Async -PassThru | Wait-XenTask -ShowProgress
  }
}

function install_vm([String]$name, [String]$sr_name)
{
  trap [Exception]
  {
  	trap [Exception]
	{
	  log_warn "Clean up after failed vm install unsuccessful"
	  log_info "...failed!"
	  break
	}

    log_info "Attempting to clean up after failed vm install..."

	$vms = Get-XenVM -Name $name

	foreach($vm in $vms)
  	{
  	  destroy_vm($vm)
  	}
	log_info "...success."
	break
  }

  #find a windows template
  log_info "looking for a Windows template..."
  $template = @(Get-XenVM -Name 'Windows *' | where {$_.is_a_template})[0]

  log_info ("installing vm '{0}' from template '{1}'" -f $template.name_label,$name)
  
  #clone template
  log_info ("cloning vm '{0}' to '{1}'" -f $template.name_label,$name)
  Invoke-XenVM -VM $template -XenAction Clone -NewName $name -Async `
                     -PassThru | Wait-XenTask -ShowProgress
  
  $vm = Get-XenVM -Name $name  
  $sr = Get-XenSR -Name $sr_name
  $other_config = $vm.other_config
  $other_config["disks"] = $other_config["disks"].Replace('sr=""', 'sr="{0}"' -f $sr.uuid)
  
  #add cd drive
  log_info ("creating cd drive for vm '{0}'" -f $vm.name_label)
  New-XenVBD -VM $vm -VDI $null -Userdevice 3 -Bootable $false -Mode RO `
             -Type CD -Unpluggable $true -Empty $true -OtherConfig @{} `
             -QosAlgorithmType "" -QosAlgorithmParams @{}

  Set-XenVM -VM $vm -OtherConfig $other_config
  
  #provision vm 
  log_info ("provisioning vm '{0}'" -f $vm.name_label)
  Invoke-XenVM -VM $vm -XenAction Provision -Async -PassThru | Wait-XenTask -ShowProgress
  
  return $true
}

function uninstall_vm([String]$name)
{
  log_info ("uninstalling vm '{0}'" -f $name)
   
  $vms = Get-XenVM -Name $name
  
  foreach($vm in $vms)
  {
    destroy_vm($vm)
  }
  
  return $true
}

function vm_can_boot($vm_name, [XenApi.Host[]] $servers)
{
  trap [Exception]
  {
    $script:exceptions += $_.Exception
	continue
  }

  $script:exceptions = @()
  foreach ($server in $servers)
  {
    Invoke-XenVM -Name $vm_name -XenAction AssertCanBootHere -XenHost $server
  }
  
  if ($exceptions.Length -lt $servers.Length)
  {
  	return $true
  }
  
  log_info "No suitable place to boot VM:"
  
  foreach ($excep in $script:exceptions)
  {
  	log_info ("Reason: {0}" -f $excep.Message)
  }

  return $false
}

function start_vm([String]$vm_name)
{
  if (vm_can_boot $vm_name @(Get-XenHost))
  {
  	log_info ("starting vm '{0}'" -f $vm_name)
  }

  # even if we cant start it, attempt so we get the exception, reasons have been logged in vm_can_boot
  Invoke-XenVM -Name $vm_name -XenAction Start -Async -PassThru | Wait-XenTask -ShowProgress
  return Get-XenVM -Name $vm_name | Get-XenVMProperty -XenProperty PowerState
}

function shutdown_vm([String]$vm_name)
{
  log_info ("shutting down vm '{0}'" -f $vm_name)
  Invoke-XenVM -Name $vm_name -XenAction HardShutdown -Async -PassThru | Wait-XenTask -ShowProgress
  return (Get-XenVM -Name $vm_name).power_state
}

# End VM Functions

# Host Functions

function get_master()
{
  $pool = Get-XenPool
  return Get-XenHost -Ref $pool.master
}

# End Host Functions

# SR Functions

function get_default_sr()
{
  log_info ("getting default sr")
  $pool = Get-XenPool
  return (Get-XenPool).default_SR | Get-XenSR 
}

function create_nfs_sr([String]$sr_svr, [String]$sr_path, [String]$sr_name)
{
  log_info ("creating sr {0} at {1}:{2}" -f $sr_name,$sr_svr,$sr_path)
  $master = get_master
  $sr_opq = New-XenSR -XenHost $master -DeviceConfig @{ "server"=$sr_svr; "serverpath"=$sr_path; "options"="" } `
                  -PhysicalSize 0 -NameLabel $sr_name -NameDescription "" -Type "nfs" -ContentType "" `
                  -Shared $true -SmConfig @{} -Async -PassThru `
        | Wait-XenTask -ShowProgress -PassThru

  if ($sr_opq -eq $null)
  {
    return $false
  }
  return $true
}

function detach_nfs_sr([String]$sr_name)
{
  log_info ("destroying sr {0}" -f $sr_name)

  $pbds = Get-XenPBD
  $sr_opq = (Get-XenSR -Name $sr_name).opaque_ref

  foreach($pbd in $pbds)
  {
    if(($pbd.SR.opaque_ref -eq $sr_opq) -and $pbd.currently_attached)
    {
      Invoke-XenPBD -PBD $pbd -XenAction Unplug
    }
  }
  
  $sr_opq = Remove-XenSR -Name $sr_name -Async -PassThru | Wait-XenTask -ShowProgress
 
  if ($sr_opq -eq $null)
  {
    return $true
  }
  return $false
}

# End SR Functions

# Helper Functions

function append_random_string_to([String]$toAppend, $length = 10)
{
	$randomisedString = $toAppend
	$charSet = "0123456789abcdefghijklmnopqrstuvwxyz".ToCharArray()
	for($i; $i -le $length; $i++)
	{
		$randomisedString += $charSet | Get-Random
	}
	return $randomisedString
}

# End Helper Functions

# Test List

$tests = @(
            @("Connect Server", "connect_server $svr $usr $pwd", $true),
			@("Create SR", "create_nfs_sr $sr_svr $sr_path PowerShellAutoTestSR", $true),
            @("Install VM", "install_vm PowerShellAutoTestVM PowerShellAutoTestSR", $true),
			@("Start VM", "start_vm PowerShellAutoTestVM", "Running"),
			@("Shutdown VM", "shutdown_vm PowerShellAutoTestVM", "Halted"),
			@("Uninstall VM", "uninstall_vm 'PowerShellAutoTestVM'", $true),
			@("Destroy SR", "detach_nfs_sr PowerShellAutoTestSR", $true),
            @("Disconnect Server", "disconnect_server $svr", $true)
          )
# End Test List

# Main Test Execution
$complete = 0;
$max = $tests.Count;

$fails = @{}

prep_xml_output $out_xml

$vmName = append_random_string_to "PowerShellAutoTestVM"
$srName = append_random_string_to "PowerShellAutoTestSR"

foreach($test in $tests)
{
  trap [Exception]
  {
    # we encountered an exception in running the test before it completed
	# its already been logged, so continue
	continue
  }
  $success = $false
  
  # Add randomness to the names of the test VM and SR to 
  # allow a parallel execution context
  $cmd = $test[1]
  $cmd = $cmd -replace "PowerShellAutoTestVM", $vmName
  $cmd = $cmd -replace "PowerShellAutoTestSR", $srName
  
  $success = exec $test[0] $cmd $test[2]
  if ($success)
  {
    $complete++
  }
}

close_xml_output $out_xml

$result = "Result: {0} completed out of {1}" -f $complete,$max;

write-host $result -f 2

if($fails.Count -gt 0)
{
  write-host "Failures:"
  $fails
}

$ErrorActionPreference = $Eap
$VerbosePreference = $Vp
$WarningPreference = $Wp
$ErrorPreference = $Ep

# End Main Test Execution
