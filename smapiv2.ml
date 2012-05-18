open Types

let api =
  let vdi_info =
    Type.(Struct(
      ( "vdi", Name "vdi", "The unique id of this VDI" ),
      [ "sr", Name "sr", "The SR containing this VDI";
	"content_id", Name "content_id", "The unique id of the VDI contents. If two VDIs have the same content_id then they must have the same data inside";
	"name_label", Basic String, "Human-readable name of the VDI";
	"name_description", Basic String, "Human-readable description of the VDI";
	"ty", Basic String, "Used by a toolstack to remember why a VDI was created";
	"metadata_of_pool", Basic String, "In the special case of a pool metadata containing VDI, this is the pool reference";
	"is_a_snapshot", Basic Boolean, "True if the VDI is a snapshot of another VDI";
	"snapshot_time", Basic String, "If is_a_snapshot is true then this is the time the snapshot was created";
	"snapshot_of", Basic String, "If is_a_snapshot is true then this is the VDI which was snapshotted";
	"read_only", Basic Boolean, "If true then this VDI is stored on read-only media";
	"virtual_size", Basic Int64, "Size of the VDI from the perspective of a VM (in bytes)";
	"physical_utilisation", Basic Int64, "Amount of space currently being consumed on the physical storage media";
      ]
    )) in
  let sr = {
    Arg.name = "sr";
    ty = Type.(Basic String);
    description = "The Storage Repository to operate within";
  } in
  let vdi = {
    Arg.name = "vdi";
    ty = Type.(Basic String);
    description = "The Virtual Disk Image to operate on";
  } in
  let attach_info =
	  Type.(Struct(
		  ( "params", Basic String, "The xenstore backend params key"),
		  [ "xenstore_data", Dict(String, Basic String), "Additional xenstore backend device keys" ]
	  )) in
  let vdi_info' = {
    Arg.name = "vdi_info";
(*    ty = vdi_info; *)
    ty = Type.Name "vdi_info";
    description = "The Virtual Disk Image properties";
  } in
  let params = {
    Arg.name = "params";
    ty = Type.(Dict(String, Basic String));
    description = "Additional key/value pairs";
  } in
  {
    Interfaces.name = "storage";
    title = "Storage Manager";
    description = "The Storage Manager (SM) is responsible for all storage operations on an XCP host. It organises Virtual Disk Images (VDIs) into homogenous collections known as Storage Repositories (SRs) where each collection is stored in a specific format (e.g. .vhd files on NFS, raw LUN on a iSCSI/FC storage array). The Storage Manager API (SMAPI) provides a simple abstract interface which allows the toolstack to create, destroy, snapshot, clone, resize etc VDIs within SRs";
    exn_decls = [];
    type_decls = [
		{
			TyDecl.name = "vdi";
			description = "Primary key for a Virtual Disk Image (e.g. path on a storage system)";
			ty = Type.(Basic String);
		}; {
			TyDecl.name = "sr";
			description = "Primary key for a Storage Repository (e.g. a UUID)";
			ty = Type.(Basic String);
		}; {
			TyDecl.name = "content_id";
			description = "Identifies the contents (i.e. bytes with) a VDI. Two VDIs with the same content_id must have the same content.";
			ty = Type.(Basic String);
		}; {
			TyDecl.name = "vdi_info";
			description = "All per-VDI properties";
			ty = vdi_info
		};
	];
    interfaces =
      [
	{
	  Interface.name = "VDI";
	  description = "Operations which operate on Virtual Disk Images";
	  type_decls = [];
	  methods = [
	    {
	      Method.name = "create";
	      description = "[create task sr vdi_info params] creates a new VDI in [sr] using [vdi_info]. Some fields in the [vdi_info] may be modified (e.g. rounded up), so the function returns the vdi_info which was used.";
	      inputs = [
		sr;
		vdi_info';
		params;
	      ];
	      outputs = [
		{ Arg.name = "new_vdi";
		  ty = vdi_info;
		  description = "The created Virtual Disk Image";
		}
	      ];
	    }; {
	      Method.name = "snapshot";
	      description = "[snapshot task sr vdi vdi_info params] creates a new VDI which is a snapshot of [vdi] in [sr]";
	      inputs = [
		sr;
		vdi;
		vdi_info';
		params;
	      ];
	      outputs = [
		{ Arg.name = "new_vdi";
		  ty = vdi_info;
		  description = "[snapshot task sr vdi vdi_info params] creates a new VDI which is a snapshot of [vdi] in [sr]";
		}
	      ];
	    }; {
	      Method.name = "clone";
	      description = "[clone task sr vdi vdi_info params] creates a new VDI which is a clone of [vdi] in [sr]";
	      inputs = [
		sr;
		vdi;
		vdi_info';
		params;
	      ];
	      outputs = [
		{ Arg.name = "new_vdi";
		  ty = vdi_info;
		  description = "[clone task sr vdi vdi_info params] creates a new VDI which is a clone of [vdi] in [sr]";
		}
	      ];
	    }; {
	      Method.name = "destroy";
	      description = "[destroy task sr vdi] removes [vdi] from [sr]";
	      inputs = [
		sr;
		vdi;
	      ];
	      outputs = [
	      ];
	    }; {
	      Method.name = "attach";
	      description = "[attach task dp sr vdi read_write] returns the [params] for a given [vdi] in [sr] which can be written to if (but not necessarily only if) [read_write] is true";
	      inputs = [
		{ Arg.name = "dp";
		  ty = Type.(Basic String);
		  description = "DataPath to attach this VDI for";
		};
		sr;
		vdi;
		{ Arg.name = "read_write";
		  ty = Type.(Basic Boolean);
		  description = "If true then the DataPath will be used read/write, false otherwise";
		}
	      ];
	      outputs = [
		{ Arg.name = "params";
		  ty = Type.(Basic String);
		  description = "xenstore backend params key";
		}
	      ];
	    }; {
	      Method.name = "activate";
	      description = "[activate task dp sr vdi] signals the desire to immediately use [vdi]. This client must have called [attach] on the [vdi] first.";
	      inputs = [
		{ Arg.name = "dp";
		  ty = Type.(Basic String);
		  description = "DataPath to attach this VDI for";
		};
		sr;
		vdi;
	      ];
	      outputs = [
	      ];
	    }; {
	      Method.name = "deactivate";
	      description = "[deactivate task dp sr vdi] signals that this client has stopped reading (and writing) [vdi].";
	      inputs = [
		{ Arg.name = "dp";
		  ty = Type.(Basic String);
		  description = "DataPath to deactivate";
		};
		sr;
		vdi;
	      ];
	      outputs = [
	      ];
	    }; {
	      Method.name = "detach";
	      description = "[detach task dp sr vdi] signals that this client no-longer needs the [params] to be valid.";
	      inputs = [
		{ Arg.name = "dp";
		  ty = Type.(Basic String);
		  description = "DataPath to detach";
		};
		sr;
		vdi;
	      ];
	      outputs = [
	      ];
	    }; {
	      Method.name = "copy";
	      description = "[copy task sr vdi url sr2] copies the data from [vdi] into a remote system [url]'s [sr2]";
	      inputs = [
		sr;
		vdi;
		{ Arg.name = "url";
		  ty = Type.(Basic String);
		  description = "URL which identifies a remote system";
		};
		{ sr with Arg.name = "dest" };
	      ];
	      outputs = [
		{ vdi with Arg.name = "new_vdi" }
	      ];
	    }; {
	      Method.name = "get_url";
	      description = "[get_url task sr vdi] returns a URL suitable for accessing disk data directly.";
	      inputs = [
		sr;
		vdi
	      ];
	      outputs = [
		{ Arg.name = "url";
		  ty = Type.(Basic String);
		  description = "URL which represents this VDI";
		}
	      ];
	    }; {
	      Method.name = "get_by_name";
	      description = "[get_by_name task sr name] returns the vdi within [sr] with [name]";
	      inputs = [
		sr;
		{ Arg.name = "name";
		  ty = Type.(Basic String);
		  description = "Name of the VDI to return";
		};
	      ];
	      outputs = [
		vdi
	      ];
	    }; {
	      Method.name = "set_content_id";
	      description = "[set_content_id task sr vdi content_id] tells the storage backend that a VDI has an updated [content_id]";
	      inputs = [
		sr;
		vdi;
		{ Arg.name = "content_id";
		  ty = Type.(Basic String);
		  description = "New value of the VDI content_id field";
		}
	      ];
	      outputs = [
	      ];
	    }; {
	      Method.name = "compose";
	      description = "[compose task sr vdi1 vdi2] layers the updates from [vdi2] onto [vdi1], modifying [vdi2]";
	      inputs = [
		sr;
		{ vdi with Arg.name = "vdi1" };
		{ vdi with Arg.name = "vdi2" };
	      ];
	      outputs = [
	      ];
	    }
	      
	  ]
	}; {
	  Interface.name = "SR";
	  description = "Operations which act on Storage Repositories";
	  type_decls = [];
	  methods = [
	    {
	      Method.name = "attach";
	      description = "[attach task sr]: attaches the SR";
	      inputs = [
		sr;
		{ Arg.name = "device_config";
		  ty = Type.(Dict(String, Basic String));
		  description = "Host-local SR configuration (e.g. address information)";
		};
	      ];
	      outputs = [
	      ];
	    }; {
	      Method.name = "detach";
	      description = "[detach task sr]: detaches the SR, first detaching and/or deactivating any active VDIs. This may fail with Sr_not_attached, or any error from VDI.detach or VDI.deactivate.";
	      inputs = [
		sr;
	      ];
	      outputs = [
	      ];
	    }; {
	      Method.name = "destroy";
	      description = "[destroy sr]: destroys (i.e. makes unattachable and unprobeable) the [sr], first detaching and/or deactivating any active VDIs. This may fail with Sr_not_attached, or any error from VDI.detach or VDI.deactivate.";
	      inputs = [
		sr;
	      ];
	      outputs = [
	      ];
	    }; {
	      Method.name = "reset";
	      description = "[reset task sr]: declares that the SR has been completely reset, e.g. by rebooting the VM hosting the SR backend.";
	      inputs = [
		sr;
	      ];
	      outputs = [
	      ];
	    }; {
	      Method.name = "scan";
	      description = "[scan task sr] returns a list of VDIs contained within an attached SR";
	      inputs = [
		sr;
	      ];
	      outputs = [
			  {
				  Arg.name = "vdis";
				  ty = Type.(Array (Name "vdi_info"));
				  description = "List of all the visible VDIs in the SR";
			  }
	      ];
	    }
	  ]
	}; {
	  Interface.name = "DP";
	  description = "Operations which act on DataPaths";
	  type_decls = [];
	  methods = [
	    {
	      Method.name = "create";
	      description = "[create task id]: creates and returns a dp";
	      inputs = [
		{ Arg.name = "id";
		  ty = Type.(Basic String);
		  description = "Human-readable DataPath name, for logging and diagnostics";
		}
	      ];
	      outputs = [
		{ Arg.name = "id";
		  ty = Type.(Basic String);
		  description = "Abstract DataPath identifier";
		}
	      ];
	    }; {
	      Method.name = "destroy";
	      description = "[destroy task id]: frees any resources associated with [id] and destroys it. This will typically do any needed VDI.detach, VDI.deactivate cleanup.";
	      inputs = [
		{ Arg.name = "id";
		  ty = Type.(Basic String);
		  description = "Abstract DataPath identifier";
		}; {
		  Arg.name = "allow_leak";
		  ty = Type.(Basic Boolean);
		  description = "If true then a failure will be logged but the call will not fail";
		}
	      ];
	      outputs = [
	      ];
	    }; {
	      Method.name = "diagnostics";
	      description = "[diagnostics ()]: returns a printable set of diagnostic information, typically including lists of all registered datapaths and their allocated resources.";
	      inputs = [
	      ];
	      outputs = [
		{ Arg.name = "diagnostics";
		  ty = Type.(Basic String);
		  description = "A string containing loggable human-readable diagnostics information";
		}
	      ];
	    }
	  ]
	}
      ]
  }
