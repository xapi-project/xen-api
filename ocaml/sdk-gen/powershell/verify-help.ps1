#
# Copyright (c) Cloud Software Group, Inc.
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

<#
    Checks that the generated Get-Help content agrees with the cmdlets it
    describes.

    External help replaces the metadata PowerShell would otherwise derive by
    reflection, rather than adding to it, so anything the help file gets wrong
    is simply what the user sees - there is nothing left to contradict it. Each
    check below compares the help against the compiled cmdlet. None of them
    needs a server.

    Usage:
      pwsh -File verify-help.ps1 -ModulePath <directory containing the psd1>
#>
param([Parameter(Mandatory)] [string] $ModulePath)

$ErrorActionPreference = "Stop"
$manifest = Join-Path (Resolve-Path $ModulePath).Path "XenServerPSModule.psd1"
Import-Module $manifest -ErrorAction Stop

# Every cmdlet in the module is checked, the hand-written ones under
# autogen/src included. Their help is written out by hand in
# gen_powershell_help.ml rather than derived from the datamodel, which makes
# them the entries most likely to drift: add a [Parameter] to
# Connect-XenServer.cs and nothing else in the build will notice. This is what
# notices - and an undocumented parameter there is not a gap in the help, it is
# a parameter REMOVED from Get-Help, because external help replaces the
# reflected metadata rather than adding to it.
$problems = [System.Collections.Generic.List[string]]::new()
$counts = [ordered]@{}
function Note($key, $n = 1) { if (-not $counts[$key]) { $counts[$key] = 0 }; $counts[$key] += $n }
function Problem($msg) { $problems.Add($msg) }

$asm = [AppDomain]::CurrentDomain.GetAssemblies() |
       Where-Object { $_.GetName().Name -eq "XenServerPowerShell" }
$dynTypes = @($asm.GetTypes() | Where-Object { $_.Name -like "*DynamicParameters" })

$cmdlets = Get-Command -Module XenServerPSModule

foreach ($c in $cmdlets) {
    $h = Get-Help $c.Name -Full
    $docd = @($h.parameters.parameter | Where-Object { $_.name -and $_.name -ne "CommonParameters" })
    Note "cmdlets"

    if (-not ($h.Synopsis    | Out-String).Trim()) { Problem "$($c.Name): no synopsis" }
    if (-not ($h.description | Out-String).Trim()) { Problem "$($c.Name): no description" }

    # A command documenting no parameters at all replaces the reflected syntax
    # with an empty one; it does not fall back to it.
    if ($docd.Count -eq 0) { Problem "$($c.Name): documents no parameters"; continue }

    foreach ($p in $docd) {
        Note "parameters"
        $real = $c.Parameters[$p.name]

        # Dynamic parameters are not in $c.Parameters, so only flag a name the
        # assembly knows nothing about.
        if (-not $real) {
            $known = $dynTypes | Where-Object { $_.GetProperty($p.name) }
            if (-not $known) { Problem "$($c.Name) -$($p.name): documented but no such parameter" }
            continue
        }

        $advertised = @()
        if ($p.aliases -and $p.aliases.Trim() -and $p.aliases.Trim() -ne "None") {
            $advertised = @($p.aliases -split "," | ForEach-Object { $_.Trim() })
        }
        foreach ($a in $advertised) {
            Note "aliases"
            if ($real.Aliases -notcontains $a) {
                Problem "$($c.Name) -$($p.name): help advertises alias '$a', cmdlet has '$($real.Aliases -join ",")'"
            }
        }
        foreach ($a in $real.Aliases) {
            if ($advertised -notcontains $a) {
                Problem "$($c.Name) -$($p.name): cmdlet has alias '$a', help does not mention it"
            }
        }

        $attr = $real.Attributes | Where-Object { $_ -is [System.Management.Automation.ParameterAttribute] }
        $byValue = [bool](@($attr | Where-Object { $_.ValueFromPipeline }).Count)
        $byProp  = [bool](@($attr | Where-Object { $_.ValueFromPipelineByPropertyName }).Count)
        if     ($byValue -and $byProp) { $expected = "true (ByValue, ByPropertyName)" }
        elseif ($byValue)              { $expected = "true (ByValue)" }
        elseif ($byProp)               { $expected = "true (ByPropertyName)" }
        else                           { $expected = "false" }
        Note "pipeline"
        if ($p.pipelineInput -ne $expected) {
            Problem "$($c.Name) -$($p.name): help says pipelineInput '$($p.pipelineInput)', cmdlet is '$expected'"
        }

        if ($real.ParameterType.IsEnum) {
            Note "enums"
            $listed  = (@($p.parameterValueGroup.parameterValue) | Sort-Object) -join ","
            $members = ([Enum]::GetNames($real.ParameterType) | Sort-Object) -join ","
            if ($listed -ne $members) {
                Problem "$($c.Name) -$($p.name): value group does not match the enum members"
            }
        }
    }

    foreach ($name in $c.Parameters.Keys) {
        if ([System.Management.Automation.PSCmdlet]::CommonParameters -contains $name) { continue }
        if ([System.Management.Automation.PSCmdlet]::OptionalCommonParameters -contains $name) { continue }
        if ($docd.name -notcontains $name) { Problem "$($c.Name) -${name}: parameter exists but is undocumented" }
    }

    # One syntax item per parameter set, so that mutually exclusive parameters
    # are not presented as though they could be combined.
    $items = @($h.syntax.syntaxItem).Count
    Note "syntaxItems" $items
    if ($items -ne $c.ParameterSets.Count) {
        Problem "$($c.Name): $items syntax items but $($c.ParameterSets.Count) parameter sets"
    }

    $realOut = @($c.OutputType | ForEach-Object { $_.Name })
    if ($realOut.Count -gt 0) {
        Note "outputs"
        $d = (@($h.returnValues.returnValue.type.name |
               ForEach-Object { $_ -replace "^(System.|XenAPI.)",""  -replace "^Void$","void" }) | Sort-Object -Unique) -join ","
        $r = (@($realOut |
               ForEach-Object { $_ -replace "^(System.|XenAPI.)",""  -replace "^Void$","void" }) | Sort-Object -Unique) -join ","
        if ($d -ne $r) {
            Problem "$($c.Name): help OUTPUTS [$d] does not match OutputType [$r]"
        }
    }
}

# Every "Accepted when -XenAction is A, B" claim must match the generated
# dynamic-parameter class for each action it names, and every property those
# classes offer must be documented.
$dynClaims = 0
foreach ($c in $cmdlets) {
    if     ($c.Name -match "^Invoke-Xen(.+)$")      { $stem = $matches[1]; $kind = "Action" }
    elseif ($c.Name -match "^Get-Xen(.+)Property$") { $stem = $matches[1]; $kind = "Property" }
    else { continue }

    $offered = @{}
    foreach ($t in ($dynTypes | Where-Object { $_.Name -like "Xen$stem$kind*DynamicParameters" })) {
        if ($t.Name -match "^Xen$stem$kind(.+)DynamicParameters$") {
            $offered[$matches[1]] = @($t.GetProperties() | ForEach-Object { $_.Name })
        }
    }
    if ($offered.Count -eq 0) { continue }

    $h = Get-Help $c.Name -Full
    foreach ($p in $h.parameters.parameter) {
        $txt = ($p.description | Out-String)
        if ($txt -notmatch "Accepted when -Xen$kind is (.+?)\.\s*$") { continue }
        foreach ($act in ($matches[1] -split ",\s*")) {
            $act = $act.Trim(); $dynClaims++
            if (-not $offered.ContainsKey($act)) {
                Problem "$($c.Name) -$($p.name): claims -Xen$kind '$act', which does not exist"
            } elseif ($offered[$act] -notcontains $p.name) {
                Problem "$($c.Name) -$($p.name): claims -Xen$kind '$act', which does not offer it"
            }
        }
    }
    foreach ($act in $offered.Keys) {
        foreach ($prop in $offered[$act]) {
            if ($prop -eq "Async") { continue }
            if ($h.parameters.parameter.name -notcontains $prop) {
                Problem "$($c.Name) -${prop}: offered by -Xen$kind '$act' but undocumented"
            }
        }
    }
}
Note "dynamicClaims" $dynClaims

# Every example must parse, and every cmdlet and parameter it names must exist.
# An example that does not run is worse than no example, because people paste
# them.
foreach ($c in $cmdlets) {
    foreach ($ex in @((Get-Help $c.Name).examples.example | Where-Object { $_ })) {
        # A prompt opens each statement; a statement continued over several
        # lines is indented instead. Strip the prompts, keep the indentation,
        # and what is left is the script the reader would paste.
        $code = ($ex.code | Out-String).Trim() -replace "(?m)^PS>[ ]?", ""
        if (-not $code) { continue }
        Note "examples"

        $errors = $null
        $ast = [System.Management.Automation.Language.Parser]::ParseInput($code, [ref]$null, [ref]$errors)
        if ($errors.Count) {
            Problem "$($c.Name): example does not parse - $($errors[0].Message)"
            continue
        }

        $calls = $ast.FindAll(
            { $args[0] -is [System.Management.Automation.Language.CommandAst] }, $true)
        foreach ($call in $calls) {
            $name = $call.GetCommandName()
            if (-not $name) { continue }
            $info = Get-Command $name -ErrorAction SilentlyContinue
            if (-not $info) {
                Problem "$($c.Name): example calls '$name', which is not a cmdlet"
                continue
            }
            $els = @($call.CommandElements)
            for ($i = 0; $i -lt $els.Count; $i++) {
                $el = $els[$i]
                if ($el -isnot [System.Management.Automation.Language.CommandParameterAst]) { continue }
                $pn = $el.ParameterName
                $ok = $info.Parameters.ContainsKey($pn) -or
                      @($info.Parameters.Values | Where-Object { $_.Aliases -contains $pn }).Count -gt 0 -or
                      @($dynTypes | Where-Object { $_.GetProperty($pn) }).Count -gt 0
                if (-not $ok) {
                    Problem "$($c.Name): example passes -$pn to $name, which has no such parameter"
                    continue
                }

                # An enum argument must still name a member. This is what
                # catches an operation being renamed or withdrawn under an
                # example that mentions it by name.
                $real = $info.Parameters[$pn]
                if ($real -and $real.ParameterType.IsEnum) {
                    $arg = $el.Argument
                    if (-not $arg -and $i + 1 -lt $els.Count) { $arg = $els[$i + 1] }
                    $lit = $arg -as [System.Management.Automation.Language.StringConstantExpressionAst]
                    if ($lit) {
                        $members = [Enum]::GetNames($real.ParameterType)
                        if ($members -notcontains $lit.Value) {
                            Problem "$($c.Name): example passes -$pn $($lit.Value) to $name, which is not a member of $($real.ParameterType.Name)"
                        }
                    }
                }
            }
        }
    }
}

Write-Host ""
Write-Host "Generated Get-Help content checked against the cmdlets:"
foreach ($k in $counts.Keys) { Write-Host ("  {0,-15} {1}" -f $k, $counts[$k]) }
Write-Host ""
if ($problems.Count -gt 0) {
    $problems | ForEach-Object { Write-Host "::error::$_" }
    Write-Host "$($problems.Count) disagreement(s) between the generated help and the cmdlets."
    exit 1
}
Write-Host "No disagreements."
exit 0
