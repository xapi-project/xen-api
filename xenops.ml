open Types

let api =
  {
    Interfaces.name = "domains";
    title = "Domain manager";
    description = "The Xen domain management service is responsible for all domain management on an XCP host. The API allows clients to register VM configurations with the management service and issue VM lifecycle commands such as: start, shutdown, reboot, suspend, resume and migrate. A simple event interface allows interested clients to notice when significant events have happened, for example a VM reboot or a Virtual Block Device (VBD) unplug.";
    exn_decls = [
      { TyDecl.name = "Internal_error";
        description = "An unexpected internal error occurred. The exception parameter will contain further information.";
        ty = Type.(Basic String)
      }; {
        TyDecl.name = "Already_exists";
        description = "An object could not be created because one already exists with the same primary key.";
        ty = Type.Unit;
      }; {
        TyDecl.name = "Does_not_exist";
        description = "An object of type {1} and reference {2} does not exist";
        ty = Type.(Pair (Basic String, Basic String));
      }; {
        TyDecl.name = "Unimplemented";
        description = "The called function has not been implemented yet.";
        ty = Type.Unit
      }; {
        TyDecl.name = "Domain_not_built";
        description = "This operation cannot be performed on a domain which has not been built by the domain builder";
        ty = Type.Unit
      }; {
        TyDecl.name = "Maximum_vcpus";
        description = "The maximum number of vCPUs is {1}";
        ty = Type.(Basic Int64);
      }; {
        TyDecl.name = "Bad_power_state";
        description = "The operation cannot be performed while the VM is in the {1} power_state; it should be {2} instead";
        ty = Type.(Pair(Name "power_state", Name "power_state"))
      }; {
        TyDecl.name = "Failed_to_achnowledge_shutdown_request";
        description = "The VM failed to acknowledge a request to shut itself down. Perhaps it does not have the VM tools installed?";
        ty = Type.Unit;
      }; {
        TyDecl.name = "Failed_to_shutdown";
        description = "The VM failed to shutdown";
        ty = Type.Unit;
      }; {
        TyDecl.name = "Device_is_connected";
        description = "The device is still connected to a VM";
        ty = Type.Unit;
      }; {
        TyDecl.name = "Device_not_connected";
        description = "The device is not connected to a VM";
        ty = Type.Unit;
      }; {
        TyDecl.name = "Device_detach_rejected";
        description = "The VM rejected a request to hot-unplug the device";
        ty = Type.Unit;
      }; {
        TyDecl.name = "Media_not_ejectable";
        description = "The device contains media which is not ejectable.";
        ty = Type.Unit;
      }; {
        TyDecl.name = "Media_present";
        description = "The device already contains media.";
        ty = Type.Unit;
      }; {
        TyDecl.name = "Media_not_present";
        description = "THe device does not contain media.";
        ty = Type.Unit;
      }; {
        TyDecl.name = "No_bootable_device";
        description = "The VM cannot be started because no device is bootable.";
        ty = Type.Unit;
      }; {
        TyDecl.name = "Bootloader_error";
        description = "The VM bootloader returned an error.";
        ty = Type.(Pair(Basic String, Array(Basic String)));
      }; {
        TyDecl.name = "Ballooning_error";
        description = "The ballooning service returned an error.";
        ty = Type.(Pair(Basic String, Basic String));
      }; {
        TyDecl.name = "No_ballooning_service";
        description = "Memory could not be allocated because no ballooning service is running.";
        ty = Type.Unit;
      }; {
        TyDecl.name = "Not_supported";
        description = "The operation is not supported on this (kind of) VM.";
        ty = Type.Unit;
      }; {
        TyDecl.name = "IO_error";
        description = "A disk I/O error occurred.";
        ty = Type.Unit;
      }; {
        TyDecl.name = "VDI_not_found";
        description = "The storage service could not find a VDI";
        ty = Type.Unit;
      }; {
        TyDecl.name = "Caller_must_pass_file_descriptor";
        description = "This operation requires the caller to pass a file descriptor but none was sent.";
        ty = Type.Unit;
      }; {
        TyDecl.name = "Failed_to_contact_remote_service";
        description = "The remote service ({1}) could not be contacted. Perhaps it is not running?";
        ty = Type.(Basic String);
      }; {
        TyDecl.name = "Hook_failed";
        description = "The hook script failed with the given error";
        ty = Type.(Pair((Pair((Pair(Basic String, Basic String)), Basic String)), Basic String));
      }; {
        TyDecl.name = "Not_enough_memory";
        description = "The host does not have enough memory free to complete this operation";
        ty = Type.(Basic Int64);
      }
    ];
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
          methods = [	    
            {
              Method.name = "add";
              description = "[add t] registers a new VM configuration [t] with the service.";
              inputs = [
                { Arg.name = "t";
                  ty = Type.(Name "t");
                  description = "The VM configuration to register";
                }
              ];
              outputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "A reference to the registered configuration";
                }
              ];
            }; {
              Method.name = "remove";
              description = "[remove id] unregisters a VM configuration [id].";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to unregister.";
                }
              ];
              outputs = []
            }; {
              Method.name = "create";
              description = "[create id] creates the necessary domains to (re)start a given VM";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                }
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {
              Method.name = "build";
              description = "[build id] runs the domain builder to (re)start a given VM";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                }
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {
              Method.name = "create_device_model";
              description = "[create_device_model id] instantiates the virtual hardware (device model) to (re)start a given VM";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                }; {
                  Arg.name = "is_resuming";
                  ty = Type.(Basic Boolean);
                  description = "If true then the VM is being resumed and device model state should be reloaded";
                }
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {
              Method.name = "destroy";
              description = "[destroy id] destroys all domains associated with a given VM";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                }
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {
              Method.name = "pause";
              description = "[pause id] deschedules all vCPUs of a VM, leaving it paused";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                }
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {
              Method.name = "unpause";
              description = "[unpause id] unpauses a VM, allowing all vCPUs to be scheduled";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                }
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {
              Method.name = "set_vcpus";
              description = "[set_vcpus id target] sets the total number of vCPUs which a VM should use to [target]";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                }; {
                  Arg.name = "num_vpcus";
                  ty = Type.(Basic Int64);
                  description = "The target number of vCPUs";
                }
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {
              Method.name = "set_shadow_multiplier";
              description = "[set_shadow_multiplier id] sets the total number of vCPUs which a VM should use to [target]";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                }; {
                  Arg.name = "shadow_multiplier";
                  ty = Type.(Basic Double);
                  description = "The target shadow_multiplier";
                }
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {
              Method.name = "stat";
              description = "[stat id] returns the current VM configuration";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to query.";
                };
              ];
              outputs = [
                { Arg.name = "state";
                  ty = Type.(Option(Pair(Name "t", Name "state")));
                  description = "The current VM runtime state"
                }
              ]
            }; {	      
              Method.name = "list";
              description = "[list id] returns the list of all registered VM configurations";
              inputs = [];
              outputs = [
                { Arg.name = "VMs";
                  ty = Type.(Array(Pair(Name "t", Name "state")));
                  description = "All registered VM configurations"
                }
              ]
            }; {	      
              Method.name = "start";
              description = "[start id] powers-on a VM, leaving it Running";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                }
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {
              Method.name = "shutdown";
              description = "[shutdown id] shuts down and powers-off a VM, leaving it Halted";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                }; {
                  Arg.name = "clean_shutdown_timeout";
                  ty = Type.(Option (Basic Double));
                  description = "If a clean_shutdown_timeout is provided, signal and wait for the guest to shutdown before powering it off";
                }
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {
              Method.name = "reboot";
              description = "[reboot id] reboots a VM, leaving it Running";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                }; {
                  Arg.name = "clean_shutdown_timeout";
                  ty = Type.(Option (Basic Double));
                  description = "If a clean_shutdown_timeout is provided, signal and wait for the guest to shutdown before powering it off";
                }
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {
              Method.name = "resume";
              description = "[resume id disk] resumes a VM from the given [disk], leaving it Running";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                }; {
                  Arg.name = "disk";
                  ty = Type.(Name "disk");
                  description = "The disk from which to load the running memory state";
                }
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {

              Method.name = "s3suspend";
              description = "[s3suspend id] triggers an S3 suspend";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                };
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {
              Method.name = "s3resume";
              description = "[s3resume id] triggers an S3 resume";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                };
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {
              Method.name = "migrate";
              description = "[migrate id url] migrates a running VM image to a remote machine";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                }; {
                  Arg.name = "url";
                  ty = Type.(Basic String);
                  description = "URL of remote xenops service to receive the running VM image";
                }
              ];
              outputs = [
                { Arg.name = "task";
                  ty = Type.(Name "id/Task");
                  description = "A reference to an asynchronous task"
                }
              ]
            }; {
              Method.name = "export_metadata";
              description = "[export_metadata id] returns a whole VM configuration as a string";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the VM to act on.";
                };
              ];
              outputs = [
                { Arg.name = "VM";
                  ty = Type.(Basic String);
                  description = "A whole VM configuration"
                }
              ]
            }; {
              Method.name = "import_metadata";
              description = "[import_metadata config] takes a whole VM configuration and registers it (including all devices)";
              inputs = [
                { Arg.name = "config";
                  ty = Type.(Basic String);
                  description = "The whole VM configuration to import.";
                };
              ];
              outputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "A reference to the registered VM."
                }
              ]
            };


          ];
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
        }; {
          Interface.name = "Pci";
          description = "PCI devices passed-through to the VM";
          type_decls = [
            { TyDecl.name = "id";
              description = "A reference (or handle) to a PCI device";
              ty = Type.(Array (Basic String))
            }; {
              TyDecl.name = "t";
              description = "PCI device configuration";
              ty = Type.(Struct(
                  ("id", Name "id", "Reference to this PCI device configuration"),
                  ["position", Basic Int64, "Position on the VM's virtual PCI bus";
                   "domain", Basic Int64, "Physical PCI device domain";
                   "bus", Basic Int64, "Physical PCI device bus";
                   "dev", Basic Int64, "Physical PCI device dev";
                   "fn", Basic Int64, "Physical PCI device function";
                   "msitranslate", Option (Basic Boolean), "Override the VM-wide MSI translation setting for this device";
                   "power_mgmt", Option (Basic Boolean), "Override the VM-wide PCI power management setting for this device";
                  ]));
            }; {
              TyDecl.name = "state";
              description = "Run-time state of this PCI device";
              ty = Type.(Struct(
                  ("plugged", Basic Boolean, "If true this device is currently attached to the VM"), []));
            }
          ];
          methods = [];
        }; {
          Interface.name = "Vbd";
          description = "Virtual Block Device configuration";
          methods = [];
          type_decls = [
            { TyDecl.name = "mode";
              description = "Indicates whether this block device will be exposed read/only or read/write";
              ty = Type.(Variant(
                  ("ReadOnly", Unit, "Expose as read/only"),
                  ["ReadWrite", Unit, "Expose as read/write"]));
            }; {
              TyDecl.name = "ty";
              description = "Indicates whether this block device will be exposed as a fixed disk or removable CDROM";
              ty = Type.(Variant(
                  ("CDROM", Unit, "Expose as a removable CDROM"),
                  ["Disk", Unit, "Expose as a fixed disk"]))
            }; {
              TyDecl.name = "id";
              description = "A reference (or handle) to this VBD configuration";
              ty = Type.(Array(Basic String));
            }; {
              TyDecl.name = "qos_class";
              description = "Disk QoS scheduling class";
              ty = Type.(Variant(
                  ("Highest", Unit, ""),
                  ["High", Unit, "";
                   "Normal", Unit, "";
                   "Low", Unit, "";
                   "Lowest", Unit, "";
                   "Other", Basic Int64, ""]));
            }; {
              TyDecl.name = "qos_scheduler";
              description = "Choice of Disk QoS scheduler";
              ty = Type.(Variant(
                  ("RealTime", Name "qos_class", ""),
                  ["Idle", Unit, "";
                   "BestEffort", Name "qos_class", ""]));
            }; {
              TyDecl.name = "qos";
              description = "VBD QoS configuration";
              ty = Type.(Variant(
                  ("Ionice", Name "qos_scheduler", ""), []));
            }; {
              TyDecl.name = "t";
              description = "VBD configuration";
              ty = Type.(Struct(
                  ("id", Name "id", "Unique ID for this VBD configuration"),
                  ["position", Option (Basic Int64), "Position this disk should be exposed at on the VM's disk controller";
                   "mode", Name "mode", "Read/write or Read/only";
                   "backend", Option (Name "disk"), "Actual disk contents (None means empty)";
                   "ty", Name "ty", "Fixed disk or removable CDROM";
                   "unpluggable", Basic Boolean, "If true then the device can be hot-unplugged from the VM";
                   "extra_backend_keys", Dict(String, Basic String), "Key/value pairs to be written to the disk backend in xenstore";
                   "extra_private_keys", Dict(String, Basic String), "Key/value pairs to be written to the per-disk private directory in xenstore";
                   "qos", Option(Name "qos"), "Optional QoS configuration"
                  ]));
            }; {
              TyDecl.name = "state";
              description = "Run-time state of the VBD";
              ty = Type.(Struct(
                  ("id", Name "id", "Unique ID for the corresponding VBD"),
                  ["qos_target", Option(Name "qos"), "Disk QoS parameters in effect (if any)";
                   "media_present", Basic Boolean, "If true then media is present in this drive"]))
            }
          ]
        }; {
          Interface.name = "Vif";
          description = "Virtual (Network) InterFace configuration";
          methods = [];
          type_decls = [
            { TyDecl.name = "id";
              description = "A reference (or handle) to identify this VIF configuration";
              ty = Type.(Array(Basic String));
            }; {
              TyDecl.name = "rate";
              description = "QoS settings for this VIF";
              ty = Type.(Struct(
                  ("kbytes_per_sec", Basic Int64, "kBytes/sec maximum rate to allow"),
                  ["timeslice_us", Basic Int64, "Interval in microseconds to impose the maximum rate"]
                ));
            }; {
              TyDecl.name = "t";
              description = "a VIF configuration";
              ty = Type.(Struct(
                  ("id", Name "id", "A reference to uniquely identify this VIF configuration"),
                  ["position", Basic Int64, "Position this VIF will be exposed at on the VM's bus";
                   "mac", Basic String, "MAC address";
                   "carrier", Basic Boolean, "If true then signal that a carrier is present";
                   "mtu", Basic Int64, "Maximum Transfer Unit (MTU)";
                   "rate", Option(Name "rate"), "Optional QoS setting";
                   "backend", Name "t/Network", "XXX";
                   "other_config", Dict(String, Basic String), "";
                   "extra_private_keys", Dict(String, Basic String), ""
                  ]));
            }; {
              TyDecl.name = "state";
              description = "Run-time state of this VIF";
              ty = Type.(Struct(
                  ("plugged", Basic Boolean, "If true then this VIF is currently attached to the VM"),
                  ["kthread_pid", Basic Int64, "";
                   "media_present", Basic Boolean, "If true then media is present in this VIF"
                  ]));
            }
          ]
        }; {
          Interface.name = "Metadata";
          description = "Encapsulates a whole VM configuration (VM, VBDs, VIFs, PCIs)";
          type_decls = [
            { TyDecl.name = "t";
              description = "A whole VM configuration (e.g. for export or import)";
              ty = Type.(Struct(
                  ("vm", Name "t/Vm", ""),
                  ["vbds", Array(Name "t/Vbd"), "";
                   "vifs", Array(Name "t/Vif"), "";
                   "pcis", Array(Name "t/Pci"), "";
                   "domains", Option (Basic String), "Opaque data describing per-domain state"]));
            }
          ];
          methods = [];
        }; {
          Interface.name = "Task";
          description = "Describes asynchronous tasks";
          type_decls = [
            { TyDecl.name = "id";
              description = "A reference (or handle) to a specific task";
              ty = Type.(Basic String)
            }; {
              TyDecl.name = "result";
              description = "The result of a task which has stopped executing";
              ty = Type.(Variant(
                  ("Pending", Basic Double, "Task is still running and is the given fraction complete"),
                  ["Completed", Basic Double, "Task has completed successfully after the given number of seconds";
                   "Failed", Basic String, "XXX Task has failed with the given error"]));
            }; {
              TyDecl.name = "t";
              description = "An asynchronous task";
              ty = Type.(Struct(
                  ("id", Name "id", "A reference (or handle) to a specific task"),
                  ["result", Name "result", "Current progress or result";
                   "subtasks", Dict(String, Basic String), "Diagnostic information about 'subtasks'"]));
            };
          ];
          methods = [

            {
              Method.name = "stat";
              description = "[stat task] returns the current state of [task].";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the task to query";
                }
              ];
              outputs = [
                { Arg.name = "t";
                  ty = Type.(Name "t");
                  description = "The current state of the task";
                }
              ];
            }; {
              Method.name = "cancel";
              description = "[cancel task] requests that [task] be immediately cancelled.";
              inputs = [
                { Arg.name = "id";
                  ty = Type.(Name "id");
                  description = "The reference of the task to cancel";
                }
              ];
              outputs = []
            }
          ]
        }; {
          Interface.name = "Dynamic";
          description = "Dynamically-typed values";
          type_decls = [
            { TyDecl.name = "id";
              description = "A reference (or handle) to a specific object";
              ty = Type.(Variant(
                  ("Vm", Name "id/Vm", "A VM reference"),
                  ["Vbd", Name "id/Vbd", "A VBD reference";
                   "Vif", Name "id/Vif", "A VIF reference";
                   "Pci", Name "id/Pci", "A PCI reference";
                   "Task", Name "id/Task", "A Task reference";
                   "Barrier", Basic Int64, "A barrier inserted into an event stream"
                  ]));
            }; {
              TyDecl.name = "t";
              description = "A dynamically-typed event";
              ty = Type.(Variant(
                  ("Vm_t", Pair(Name "id/Vm", Option (Pair (Name "t/Vm", Name "state/Vm"))), ""),
                  [
                    "Vbd_t", Pair(Name "id/Vbd", Option (Pair (Name "t/Vbd", Name "state/Vbd"))), "";
                    "Vif_t", Pair(Name "id/Vif", Option (Pair (Name "t/Vif", Name "state/Vif"))), "";
                    "Pci_t", Pair(Name "id/Pci", Option (Pair (Name "t/Pci", Name "state/Pci"))), "";
                    "Task_t", Pair(Name "id/Task", Option (Name "t/Task")), ""
                  ]));
            }
          ];
          methods = [];
        }
      ]
  }
