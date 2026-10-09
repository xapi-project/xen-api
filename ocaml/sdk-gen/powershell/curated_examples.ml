(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

(* Worked examples for the cmdlets where the generated one-liner does not
   teach enough.

   gen_powershell_help gives every cmdlet an example built from its family's
   idiom, which is right for the bulk of the SDK but cannot show why you would
   reach for a thing. The entries here are the ones that carry an idiom a
   reader would otherwise have to work out: the asynchronous form, an action
   that takes an object as a parameter, an operation whose online and offline
   variants are different actions rather than a flag, a field written from one
   side and read from the other.

   They are written the way examples in a mature PowerShell module are: a
   title saying what the example shows, a prompt opening each statement, an
   indented continuation where one statement runs over several lines, a
   realistic object name rather than a placeholder, and a remark that says why
   the form is what it is rather than restating the command.

   The code is a quoted literal so that the indentation is the indentation the
   reader sees: an ordinary string literal loses it to OCaml's line
   continuation.

   These are appended after the generated example, so each cmdlet reads from
   the simplest form to the most involved.

   KEEPING THEM HONEST

   Hand-written examples rot quietly as the datamodel moves, so nothing here is
   taken on trust:

     - test_gen_powershell checks every entry in this file: that it names a
       cmdlet once, that each statement is written at a prompt, that the
       snippet invokes the cmdlet it is filed under, and that it carries a
       title and an explanation;
     - gen_powershell_help fails to build if an entry names a cmdlet it does
       not generate, so a renamed or withdrawn cmdlet cannot leave a stale
       entry behind;
     - verify-help.ps1 strips the prompts, parses what is left against the
       built module, and fails CI if a cmdlet, a parameter or an enum value it
       names has gone.

   That last check is why no example here shows sample output: a command can
   be verified against the module, invented output cannot, and output is what
   would rot first. Adding an example is a matter of appending to the list;
   the checks apply to it automatically. *)

(* Cmdlet name, then (title, code, explanation) triples. *)
let examples : (string * (string * string * string) list) list =
  [
    ( "Get-XenVM"
    , [
        ( "List the running VMs"
        , {|PS> Get-XenVM | Where-Object { $_.power_state -eq "Running" }|}
        , "Every field of the XenAPI record is a property of the returned \
           object, so the objects are filtered with Where-Object rather than \
           by a cmdlet parameter. The property names are the API's, in their \
           original spelling."
        )
      ; ( "Find the templates"
        , {|PS> Get-XenVM | Where-Object { $_.is_a_template }|}
        , "Templates are VM objects with is_a_template set, not a separate \
           class, so there is no Get-XenTemplate; this is how you list the \
           templates a new VM can be cloned from."
        )
      ; ( "Retrieve one VM and read a field"
        , {|PS> $vm = Get-XenVM -Name "web-01"
PS> $vm.memory_static_max|}
        , "-Name matches on name_label, which is not unique: the cmdlet \
           returns every match. Use -Uuid where you need exactly one object."
        )
      ]
    )
  ; ( "Invoke-XenVM"
    , [
        ( "Start a VM"
        , {|PS> Get-XenVM -Name "web-01" | Invoke-XenVM -XenAction Start|}
        , "The VM operations are actions on one cmdlet rather than a cmdlet \
           each, so the operation is chosen with -XenAction. The VM is piped \
           in; -Ref or -Uuid would do as well."
        )
      ; ( "Shut down a VM cleanly or forcibly"
        , {|PS> $vm = Get-XenVM -Name "web-01"
PS> $vm | Invoke-XenVM -XenAction CleanShutdown
PS> $vm | Invoke-XenVM -XenAction HardShutdown|}
        , "CleanShutdown asks the guest to shut itself down and fails if it \
           will not; HardShutdown is the equivalent of pulling the power. They \
           are separate actions because the choice matters."
        )
      ; ( "Run an operation asynchronously"
        , {|PS> Get-XenVM -Name "web-01" |
    Invoke-XenVM -XenAction Start -Async -PassThru |
    Wait-XenTask -ShowProgress|}
        , "-Async returns as soon as the server has accepted the work and \
           -PassThru emits the Task that represents it, which pipes straight \
           into Wait-XenTask. This is the idiom for any operation that may \
           take a while; without -Async the cmdlet blocks until it finishes."
        )
      ; ( "Snapshot a VM and revert to the snapshot"
        , {|PS> $vm = Get-XenVM -Name "web-01"
PS> $snapshot = $vm |
    Invoke-XenVM -XenAction Snapshot -NewName "before upgrade" -PassThru
PS> $vm | Invoke-XenVM -XenAction Revert -Snapshot $snapshot|}
        , "-PassThru is what makes the snapshot usable: without it the \
           snapshot is taken but nothing is returned to pass to Revert. \
           -Snapshot is one of the parameters this cmdlet adds for the Revert \
           action, so it only appears once -XenAction is Revert."
        )
      ; ( "Clone a template into a new VM"
        , {|PS> $template = Get-XenVM |
    Where-Object { $_.is_a_template } |
    Select-Object -First 1
PS> $vm = $template |
    Invoke-XenVM -XenAction Clone -NewName "web-02" -PassThru
PS> $vm | Invoke-XenVM -XenAction Provision|}
        , "Clone copies the template's definition; Provision then creates the \
           disks it describes. A cloned template is not runnable until it has \
           been provisioned."
        )
      ; ( "Migrate a running VM within the pool"
        , {|PS> $target = Get-XenHost -Name "host-02"
PS> Get-XenVM -Name "web-01" |
    Invoke-XenVM -XenAction PoolMigrate -XenHost $target|}
        , "PoolMigrate moves a running VM between hosts that share its \
           storage. Moving one to a host in another pool is a different \
           action, MigrateSend, because it has to copy the disks too."
        )
      ]
    )
  ; ( "New-XenVM"
    , [
        ( "Create a VM from a table of fields"
        , {|PS> New-XenVM -HashTable @{
        name_label        = "web-03"
        memory_static_max = "2147483648"
        VCPUs_max         = "2"
        VCPUs_at_startup  = "2"
    } -PassThru|}
        , "The keys are the API's field names, so anything the class accepts \
           at creation can be set in one call. In practice most VMs are made \
           by cloning a template rather than built from fields like this."
        )
      ]
    )
  ; ( "Set-XenVM"
    , [
        ( "Rename a VM and set its description"
        , {|PS> Get-XenVM -Name "web-01" |
    Set-XenVM -NameLabel "web-01a" -NameDescription "Front end, rebuilt"|}
        , "Several fields can be set in one synchronous call. Only one field \
           at a time can be set with -Async, because each field is a separate \
           call on the server."
        )
      ; ( "Put a VM into a VM group"
        , {|PS> $group = Get-XenVMGroup -Name "front end"
PS> Get-XenVM -Name "web-01" | Set-XenVM -Groups $group|}
        , "Group membership is written from the VM side and read from the \
           group side, which is the half that is easy to get wrong: there is \
           no Set-XenVMGroup parameter that adds members."
        )
      ]
    )
  ; ( "Get-XenVMGroup"
    , [
        ( "List the members of a VM group"
        , {|PS> (Get-XenVMGroup -Name "front end").VMs|}
        , "The group record holds references to its members. Membership is set \
           from the other side, with Set-XenVM -Groups."
        )
      ]
    )
  ; ( "Get-XenVBD"
    , [
        ( "List a VM's disks, excluding the CD drive"
        , {|PS> $vm = Get-XenVM -Name "web-01"
PS> $vm.VBDs |
    ForEach-Object { Get-XenVBD -Ref $_ } |
    Where-Object { $_.type -eq "Disk" }|}
        , "A VBD is the attachment between a VM and a virtual disk, and a CD \
           drive is a VBD too, hence the filter on type. The VM record holds \
           references, so each one is resolved with -Ref."
        )
      ]
    )
  ; ( "Invoke-XenVBD"
    , [
        ( "Detach and reattach a disk"
        , {|PS> $vbd = Get-XenVBD | Where-Object { $_.type -eq "Disk" } | Select-Object -First 1
PS> $vbd | Invoke-XenVBD -XenAction Unplug
PS> $vbd | Invoke-XenVBD -XenAction Plug|}
        , "Unplug makes the disk no longer live to the guest but leaves it \
           attached to the VM; removing it is Remove-XenVBD. Unplug fails if \
           the guest is using the disk and will not release it - UnplugForce \
           does not ask."
        )
      ]
    )
  ; ( "Invoke-XenVDI"
    , [
        ( "Grow a disk that is not in use"
        , {|PS> Get-XenVDI | Select-Object -First 1 |
    Invoke-XenVDI -XenAction Resize -Size 40GB|}
        , "Resize is for a disk no guest is writing to. A disk can only grow; \
           the size is in bytes, so PowerShell's GB suffix is the readable way \
           to give it."
        )
      ; ( "Grow a disk while the VM is running"
        , {|PS> Get-XenVDI | Select-Object -First 1 |
    Invoke-XenVDI -XenAction ResizeOnline -Size 60GB|}
        , "Resizing a live disk is a different action rather than a flag on \
           the same one, because it tells the storage layer the guest is using \
           the disk while it grows. Not every storage type supports it."
        )
      ]
    )
  ; ( "Invoke-XenHost"
    , [
        ( "Put a host into maintenance mode"
        , {|PS> $h = Get-XenHost -Name "host-02"
PS> $h | Invoke-XenHost -XenAction Disable
PS> $h | Invoke-XenHost -XenAction Evacuate
PS> $h | Invoke-XenHost -XenAction Enable|}
        , "Disable stops new VMs starting on the host, Evacuate migrates the \
           ones already running off it, and Enable puts it back into service. \
           Maintenance mode is those three actions rather than one flag."
        )
      ]
    )
  ; ( "Invoke-XenPool"
    , [
        ( "Rotate the pool secret"
        , {|PS> Get-XenPool | Invoke-XenPool -XenAction RotateSecret|}
        , "Existing sessions survive the rotation, so a script that is already \
           connected carries on without reconnecting."
        )
      ]
    )
  ; ( "Set-XenNetwork"
    , [
        ( "Raise a network's MTU for jumbo frames"
        , {|PS> Get-XenNetwork -Name "storage" | Set-XenNetwork -MTU 9000|}
        , "The MTU belongs to the network rather than to any one interface, \
           because every NIC and switch along the path has to agree on it. It \
           takes effect as VIFs are replugged."
        )
      ]
    )
  ; ( "New-XenVIF"
    , [
        ( "Attach a VM to a network"
        , {|PS> $vm = Get-XenVM -Name "web-01"
PS> $network = Get-XenNetwork -Name "storage"
PS> New-XenVIF -VM $vm -Network $network -Device "1" -PassThru|}
        , "A VIF is the attachment between a VM and a network. -Device is the \
           slot number as the guest sees it and has to be free; leaving the \
           MAC unset lets the server generate one."
        )
      ]
    )
  ; ( "Get-XenSR"
    , [
        ( "Rank storage repositories by free space"
        , {|PS> Get-XenSR |
    Sort-Object { $_.physical_size - $_.physical_utilisation } -Descending |
    Select-Object name_label, physical_size, physical_utilisation|}
        , "Free space is not a field: it is the physical size less what is \
           already used. Both are in bytes."
        )
      ]
    )
  ]

(* Looked up by cmdlet name; [] when the generated example is enough. *)
let for_cmdlet name =
  match List.assoc_opt name examples with Some l -> l | None -> []

let cmdlet_names = List.map fst examples
