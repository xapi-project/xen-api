open Types

let api =
  {
    Interfaces.name = "xenops";
    description = "The Xen domain management API";
    type_decls = [
      { TyDecl.name = "disk";
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
	  }
		       ]
	}
      ]
  }
