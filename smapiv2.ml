open Types

let api =
  let vdi_info_decl =
    Type.(Struct(
        ( "vdi", Name "vdi", "The unique id of this VDI. This must be unique with a Storage Repository, but may not be unique between Storage Respositories." ),
        [ "content_id", Name "content_id", "A repersentation of the VDI contents such that if two VDIs have the same content_id then they must have the same data inside";
          "name_label", Basic String, "Human-readable name of the VDI";
          "name_description", Basic String, "Human-readable description of the VDI";
          "is_a_snapshot", Basic Boolean, "True if the VDI is a snapshot of another VDI";
          "snapshot_time", Basic String, "If is_a_snapshot is true then this is the time the snapshot was created";
          "snapshot_of", Basic String, "If is_a_snapshot is true then this is the VDI which was snapshotted";
          "read_only", Basic Boolean, "If true then this VDI is stored on read-only media";
          "virtual_size", Basic Int64, "Size of the VDI from the perspective of a VM (in bytes)";
          "physical_utilisation", Basic Int64, "Amount of space currently being consumed on the physical storage medium";
        ]
      )) in
  let vdi_info = Type.Name "vdi_info" in
  let sr = {
    Arg.name = "sr";
    ty = Type.(Basic String);
    description = "The Storage Repository";
  } in
  let vdi = {
    Arg.name = "vdi";
    ty = Type.(Basic String);
    description = "The Virtual Disk Image";
  } in
  let attach_info_decl =
    Type.(Struct(
        ( "uri", Basic String, String.concat "" [
          "A URI which can be opened and used for I/O. For example this could ";
          "reference a local block device, a remote NFS share, iSCSI LUN or ";
          "RBD volume.";
        ]),
        [ "extra_headers", Dict(String, Basic String), String.concat "" [
          "Additional HTTP headers which will be provided when opening the URI. ";
          "For example these could be used to pass along an authorization token."
          ]
        ]
      )) in
  let attach_info = Type.Name "attach_info" in
  let vdi_info' = {
    Arg.name = "vdi_info";
    (*    ty = vdi_info; *)
    ty = Type.Name "vdi_info";
    description = "The Virtual Disk Image properties";
  } in
  let params = {
    Arg.name = "params";
    ty = Type.(Dict(String, Basic String));
    description = "Additional key/value pairs with an implementation-specific meaning. Consult the documentation provided by your vendor.";
  } in
  {
    Interfaces.name = "storage";
    title = "Storage Manager";
    description =
      String.concat "" [
        "The xapi toolstack delegates all storage control-plane functions to ";
        "the Storage Manager (SM). The SM organises Virtual Disk Images (VDIs) ";
        "into collections known as Storage Repositories (SRs). The Storage ";
        "Manager API (SMAPI) provides a simple abstract interface which allows ";
        "the toolstack to create, destroy, snapshot, clone, resize etc VDIs ";
        "within SRs";
      ];
    exn_decls = [
      {
        TyDecl.name = "Sr_not_attached";
        description = "An SR must be attached in order to access VDIs";
        ty = Type.(Basic String)
      }; {
        TyDecl.name = "Vdi_does_not_exist";
        description = "The specified VDI could not be found in the SR";
        ty = Type.(Basic String)
      }; {
        TyDecl.name = "Illegal_transition";
        description = "The requested VDI state transition is invalid";
        ty = Type.(Pair(Basic String, Basic String))
      }; {
        TyDecl.name = "Backend_error";
        description = "A backend-specific error occurred";
        ty = Type.(Pair(Basic String, Array (Basic String)));
      }; {
        TyDecl.name = "Unimplemented";
        description = "The operation has not been implemented";
        ty = Type.(Basic String);
      }; {
        TyDecl.name = "Does_not_exist";
        description = "The object does not exist";
        ty = Type.(Pair(Basic String, Basic String));
      }; {
        TyDecl.name = "Cancelled";
        description = "The task has been asynchronously cancelled";
        ty = Type.(Basic String);
      }; {
        TyDecl.name = "Redirect";
        description = "The request should be resent to this address";
        ty = Type.(Basic String);
      }; {
        TyDecl.name = "Sr_attached";
        description = "The operation cannot be performed because the SR is still attached";
        ty = Type.(Basic String)
      }
    ];
    type_decls = [
      {
        TyDecl.name = "vdi";
        description = String.concat "" [
          "Primary key for a Virtual Disk Image. This can be any string which ";
          "is meaningful to the implementation. For example this could be an ";
          "NFS filename, an LVM LV name or even a URI.";
          ];
        ty = Type.(Basic String);
      }; {
        TyDecl.name = "sr";
        description = String.concat "" [
          "Primary key for a specific Storage Repository. This can be any ";
          "string which is meaningful to the implementation. For example this ";
          "could be an NFS directory name, an LVM VG name or even a URI.";
        ];
        ty = Type.(Basic String);
      }; {
        TyDecl.name = "content_id";
        description = String.concat "" [
          "Identifies the data contained within a VDI. This can be any string ";
          "provided that, if two VDIs have the same content_id, they definitely ";
          "have the same contents. This implication is one-way: if two VDIs ";
          "have different content_ids then this does not imply they have ";
          "different contents.";
        ];
        ty = Type.(Basic String);
      }; {
        TyDecl.name = "vdi_info";
        description = String.concat "" [
          "A set of properties associated with a VDI. These properties can ";
          "change dynamically and can be queried by the VDI.stat call.";
        ];
        ty = vdi_info_decl
      }; {
        TyDecl.name = "attach_info";
        description = "Configuration for blkback";
        ty = attach_info_decl
      }; {
        TyDecl.name = "query_result";
        description = "properties of this implementation";
        ty = Type.(Struct( ("driver", Basic String, "driver, used in the XenAPI as SR.type"), [
            "name", Basic String, "short name";
            "description", Basic String, "description";
            "vendor", Basic String, "entity (e.g. company, project, group) which produced this implementation";
            "copyright", Basic String, "copyright";
            "version", Basic String, "version";
            "required_api_version", Basic String, "minimum required API version";
            "features", Array (Basic String), "features supported by this plugin";
            "configuration", Dict(String, Basic String), "key/description pairs describing required device_config parameters"
          ]))
      }

    ];
    interfaces =
      [
        {
          Interface.name = "Query";
          description = String.concat "" [
            "Discover properties of this implementation. Every implementation ";
            "must support the query interface or it will not be recognised as ";
            "a storage manager by xapi.";
          ];
          type_decls = [
          ];
          methods = [
            {
              Method.name = "query";
              description = String.concat "" [
                "Query this implementation and return its properties. This is ";
                "called by xapi to determine whether it is compatible with xapi ";
                "and to discover the supported features."
              ];
              inputs = [];
              outputs = [
                {
                  Arg.name = "query_result";
                  ty = Type.Name "query_result";
                  description = "The properies of this implementation"
                }
              ]
            }; {
              Method.name = "diagnostics";
              description = String.concat "" [
                "Returns a printable set of backend diagnostic information.";
                "Implementations are encouraged to include any data which will ";
                "be useful to diagnose problems. Note this data should not ";
                "include personally-identifiable data as it is intended to be ";
                "automatically included in bug reports.";
              ];
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
        };

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
              ];
              outputs = [
                { Arg.name = "new_vdi";
                  ty = vdi_info;
                  description = "The created Virtual Disk Image";
                }
              ];
            }; {
              Method.name = "snapshot";
              description = "[snapshot task sr vdi_info] creates a new VDI which is a snapshot of [vdi_info.vdi] in [sr]";
              inputs = [
                sr;
              ];
              outputs = [
                { Arg.name = "new_vdi";
                  ty = vdi_info;
                  description = "[snapshot task sr vdi_info params] creates a new VDI which is a snapshot of [vdi_info.vdi] in [sr]";
                }
              ];
            }; {
              Method.name = "clone";
              description = "[clone task sr vdi_info] creates a new VDI which is a clone of [vdi_info.vdi] in [sr]";
              inputs = [
                sr;
              ];
              outputs = [
                { Arg.name = "new_vdi";
                  ty = vdi_info;
                  description = "[clone task sr vdi_info params] creates a new VDI which is a clone of [vdi_info.vdi] in [sr]";
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
              Method.name = "stat";
              description = "[stat task sr vdi] returns metadata associated with VDI [vdi] in SR [sr].";
              inputs = [
                sr;
                vdi;
              ];
              outputs = [
                { Arg.name = "vdi_info";
                  ty = vdi_info;
                  description = "VDI metadata";
                }
              ];
            };
             {
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
                { Arg.name = "device";
                  ty = attach_info;
                  description = "backend device configuration";
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
            };
          ]
        }; {
          Interface.name = "SR";
          description = "Operations which act on Storage Repositories";
          type_decls = [];
          methods = [
            {
              Method.name = "ls";
              description = "[ls dbg]: returns a list of attached SRs";
              inputs = [];
              outputs = [
                {
                  Arg.name = "srs";
                  ty = Type.(Array (Basic String));
                  description = "The attached SRs"
                }
              ]
            };
            {
              Method.name = "create";
              description = "[create task sr device_config physical_size]: creates a fresh SR";
              inputs = [
                sr;
                { Arg.name = "device_config";
                  ty = Type.(Dict(String, Basic String));
                  description = "Host-local SR configuration (e.g. address information)";
                };
                { Arg.name = "physical_size";
                  ty = Type.(Basic Int64);
                  description = "Requested maximum size of the SR (bytes)"
                }
              ];
              outputs = []
            };
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
            };
             {
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
        }
      ]
  }
