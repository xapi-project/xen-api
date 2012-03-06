open Types

let api =
  {
    Interfaces.name = "xenops";
    description = "The Xen domain management API";
    type_decls = [
      { TyDecl.name = "power_state";
	description = "Power state of the VM";
	ty = Type.(Variant(
	  ( "Running", Unit, "VM is running (i.e. a memory exists and vCPUs are being scheduled"),
	  [ "Halted", Unit, "VM is not running (i.e. no memory is being consumed and no vCPUs are being scheduled)";
	    "Suspended", Unit, "VM runtime state has been saved to disk and VM is not running (i.e. no memory is being consumed and no vCPUs are being scheduled)";
	    "Paused", Unit, "VM is paused (i.e. memory is still being consumed but no vCPUs are being scheduled)"
	  ]));
      }; { TyDecl.name = "disk";
	description = "A disk which should be added to a VM";
	ty = Type.(Variant(
	  ( "Local", Basic String, "An already-configured block device in dmoain 0 (intended for testing)" ),
	  [ "VDI", Basic String, "Identifies a VDI controlled by the Storage Manager (of the form SR/VDI)" ]
	))
      }
    ];
    interfaces =
      [
        {
          Interface.name = "Query";
          description = "Query the properties of this service";
	  type_decls = [ { TyDecl.name = "info";
			   description = "Properties of this service";
			   ty = Type.(Struct(
			     ( "name", Basic String, "Human-readable name of this service" ),
			     [ "vendor", Basic String, "Vendor of this service";
			       "version", Basic String, "Version number of this service";
			       "features", Array (Basic String), "A set of feature flags";
			       "instance_id", Basic String, "A unique id for this instance, regenerated over service restart"
			     ]
			   ))
			 }];
          methods = [
	    {
	      Method.name = "query";
	      description = "[query] returns the properties of this service";
	      inputs = [
	      ];
	      outputs = [
		{ Arg.name = "info";
		  ty = Type.Name "info";
		  description = "The properties of this service";
		}
	      ];
	    }
	  ]
	}; {
          Interface.name = "Network";
          description = "Types used for configuring VM networking";
	  type_decls = [ { TyDecl.name = "t";
			   description = "A network to be attached to a VM";
			   ty = Type.(Variant(
			     ("Local", Basic String, "name of an already-configured local switch"),
			     [ "Remote", Basic String, "name of a switch in another domain" ]
			   ));
			 } ];
	  methods = []
	}; {
          Interface.name = "Vm";
          description = "Types used to represent a VM configuration";
	  methods = [];
	  type_decls = [ {
	    TyDecl.name = "video_card";
	    description = "Type of video hardware";
	    ty = Type.(Variant(
	      ("Cirrus", Unit, "Cirrus Logic"),
	      [ "Standard_VGA", Unit, "Standard VGA" ]
	    ));
	  }; {
	    TyDecl.name = "hvm_info";
	    description = "Hardware options specific to HVM VMs";
	    ty = Type.(Struct(
	      ("hap", Basic Boolean, "If true then Hardware Assisted Paging (HAP) will be enabled"),
	      [ "shadow_multiplier", Basic Double, "Allow the amount of shadow memory to be increased beyond the default amount (1.0 means default)";
		"timeoffset", Basic String, "Offset between the host clock (in UTC) and guest time stored in the virtual BIOS";
		"video_mib", Basic Int64, "Amount of video memory (in MiB)";
		"video", Type.Name "video_card", "Type of video hardware";
		"acpi", Basic Boolean, "If true then ACPI will be enabled";
		"serial", Option (Basic String), "";
		"keymap", Option (Basic String), "";
		"vnc_ip", Option (Basic String), "Specify the IP on which the VNC framebuffer will listen (default 127.0.0.1)";
		"pci_emulations", Array (Basic String), "";
		"pci_passthrough", Basic Boolean, "If true then PCI passthrough is enabled";
		"boot_order", Basic String, "BIOS boot order ('cdn' = disk; cdrom; network)";
		"qemu_disk_cmdline", Basic Boolean, "If true then the qemu commandline will be used to configure disks, otherwise xenstore will be used";
		"qemu_stubdom", Basic Boolean, "If true then qemu will be run inside a stub domain";
	      ]
	       
	    ));
	  }; {
	    TyDecl.name = "pv_direct_boot";
	    description = "PV VM boot options without a 'pygrub'-style bootloader";
	    ty = Type.(Struct(
	      ("kernel", Basic String, "Path to the VM kernel image"),
	      ["cmdline", Basic String, "Command-line options to pass to the VM kernel";
	       "ramdisk", Option (Basic String), "Optional initial ramdisk to pass to the VM kernel" ]));
	  }; {
	    TyDecl.name = "pv_indirect_boot";
	    description = "PV VM boot options when using a 'pygrub'-style bootloader";
	    ty = Type.(Struct(
	      ("bootloader", Basic String, "Name of the bootloader to use (e.g. 'pygrub')"),
	      [ "extra_args", Basic String, "Additional arguments to pass to the VM kernel in addition to those from the bootloader";
		"legacy_args", Basic String, "";
		"bootloader_args", Basic String, "Additional arguments to pass to the bootloader itself";
		"devices", Array (Type.Name "disk"), "Ordered list of disks to search for bootloader configuration" ]));
	  }; {
	    TyDecl.name = "pv_boot";
	    description = "Describes how to boot a particular PV VM";
	    ty = Type.(Variant(
	      ("Direct", Type.Name "pv_direct_boot", "Use the given kernel/cmdline/ramdisk"),
	      ["Indirect", Type.Name "pv_indirect_boot", "Determine the kernel/cmdline/ramdisk through a bootloader (e.g. 'pygrub')"]))

	  }; {
	    TyDecl.name = "pv_info";
	    description = "Hardware configuration for a particular PV VM";
	    ty = Type.(Struct(
	      ("boot", Type.Name "pv_boot", "Describe how to acquire a kernel/cmdline/initrd"),
	      ["framebuffer", Basic Boolean, "If true then create a PV framebuffer";
	       "vncterm", Basic Boolean, "If true then export the default serial console over VNC";
	       "vncterm_ip", Option (Basic String), "Specify the IP address to bind the default VNC serial console to"]));
	  }; {
	    TyDecl.name = "builder_info";
	    description = "Describes how to boot a particular VM (HVM or PV)";
	    ty = Type.(Variant(
	      ("HVM", Type.Name "hvm_info", "Boot HVM"),
	      ["PV", Type.Name "pv_info", "Boot PV" ]));
	  }; {
	    TyDecl.name = "id";
	    description = "A unique VM id (or handle)";
	    ty = Type.(Basic String)
	  }; {
	    TyDecl.name = "action";
	    description = "Describes an action to perform after a shutdown/reboot/crash";
	    ty = Type.(Variant(
	      ("Coredump", Unit, "Perform a core dump"),
	      ["Shutdown", Unit, "Shut down the VM and leave it Halted";
	       "Start", Unit, "(Re)Start the VM and leave it Running"
	      ]));
	  }; {
	    TyDecl.name = "weight_and_cap";
	    description = "xen credit1 scheduler weight and cap";
	    ty = Type.(Struct(
	      ("weight", Basic Int64, ""),
	      ["cap", Basic Int64, ""]));
	  }; {
	    TyDecl.name = "scheduler_params";
	    description = "vCPU configuration for the xen scheduler";
	    ty = Type.(Struct(
	      ("priority", Option (Type.Name "weight_and_cap"), "Priority setting to be applied to all vCPUs"),
	      ["affinity", Array(Array (Basic Int64)), "For each vCPU, a set of pCPUs to pin it"]));
	  }; {
	    TyDecl.name = "t";
	    description = "VM configuration";
	    ty = Type.(Struct(
	      ("id", Type.Name "id", "Unique ID for this VM"),
	      ["name", Basic String, "Human-readable (non-unique) name for this VM";
	       "ssidref", Basic Int64, "";
	       "xsdata", Dict(String, Basic String), "Additional key/value pairs to write into xenstore on domain create";
	       "platformdata", Dict(String, Basic String), "Additional key/value pairs to write into the platform/ directory in xenstore on domain create";
	       "bios_strings", Dict(String, Basic String), "Key/value pairs representing the BIOS (e.g. vendor) information to be passed to the VM";
	       "ty", Type.Name "builder_info", "Describes how to boot this VM";
	       "suppress_spurious_page_faults", Basic Boolean, "If true then enable the spurious page faults workaround";
	       "machine_address_size", Option (Basic Int64), "";
	       "memory_static_max", Basic Int64, "Amount of physical RAM to expose to the VM during boot";
	       "memory_dynamic_max", Basic Int64, "Maximum amount of physical host RAM to allocate to the guest (via PoD and ballooning)";
	       "memory_dynamic_min", Basic Int64, "Minimum amount of physical host RAM to allocate to the guest (via PoD and ballooning)";
	       "vcpu_max", Basic Int64, "Number of virtual CPUs (vCPUs) to expose to the VM during boot";
	       "vcpus", Basic Int64, "Number of virtual CPUs (vCPUs) to tell the VM to actually use";
	       "scheduler_params", Type.Name "scheduler_params", "xen scheduler configuration";
	       "on_crash", Array (Type.Name "action"), "actions to perform if this VM crashes";
	       "on_shutdown", Array (Type.Name "action"), "actions to perform if this VM shuts down";
	       "on_reboot", Array (Type.Name "action"), "actions to perform if this VM reboots";
	       "transient", Basic Boolean, "If true this VM will be forgotten if it shuts down (not if it reboots)";
	       "pci_msitranslate", Basic Boolean, "If true then enable MSI translation for passed-through PCI devices";
	       "pci_power_mgmt", Basic Boolean, "If true then enable PCI power management for passed-through PCI devices"
	      ]));
	  }; {
	    TyDecl.name = "console_protocol";
	    description = "Protocol to use for interacting with the VM console";
	    ty = Type.(Variant(
	      ("Rfb", Unit, "Use the Remote FrameBuffer protocol i.e. VNC"),
	      [ "Vt100", Unit, "Treat the console as a VT100 terminal" ]
	    ))
	  }; {
	    TyDecl.name = "console";
	    description = "Console configuration";
	    ty = Type.(Struct(
	      ("protocol", Name "console_protocol", "Protocol to use for interacting with the VM console"),
	      ["port", Basic Int64, "Port number on which the console server is listening"]));
	  }; {
	    TyDecl.name = "state";
	    description = "Run-time state of the VM";
	    ty = Type.(Struct(
	      ("power_state", Name "power_state", "Power state of the VM"),
	      ["domids", Array (Basic Int64), "Domain ids in use by the VM";
	       "consoles", Array (Name "console"), "Consoles attached to the VM";
	       "memory_target", Basic Int64, "Balloon driver memory target";
	       "vcpu_target", Basic Int64, "Actual number of vCPUs in use";
	       "shadow_multiplier_target", Basic Double, "Effective shadow_multiplier in use";
	       "rtc_timeoffset", Basic String, "Difference between the host clock (in UTC) and the VM's BIOS clock";
	       "uncooperative_balloon_driver", Basic Boolean, "If true the balloon driver is not responding properly to commands";
	       "guest_agent", Dict(String, Basic String), "Key/value pairs reported by the guest agent via xenstore";
	       "last_start_time", Basic Double, "Time the VM last (Re)Started"
	      ]
	      ));
	    }
		       ]
	}
      ]
  }
