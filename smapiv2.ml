open Types

let api =
  let vdi_info_decl =
    Type.(Struct(
        ( "vdi", Name "vdi", String.concat " " [
          "A primary key for this Virtual Disk Image (VDI). This must be unique";
          "within the enclosing Storage Repository (SR). A typical value would";
          "be a filename or an LVM volume name."
          ]),
        [ "name", Basic String, String.concat " " [
          "Short, human-readable label for the VDI. Names are commonly used by";
          "when displaying short lists of VDIs.";
          ];
          "description", Basic String, String.concat " " [
            "Longer, human-readable description of the VDI. Descriptions are";
            "generally only displayed by clients when the user is examining";
            "VDIs individually.";
          ];
          "read_write", Basic Boolean, String.concat " " [
            "True means the VDI may be written to, false means the VDI is";
            "read-only. Some storage media is read-only so all volumes are";
            "read-only; for example .iso disk images on an NFS share. Some";
            "VDIs are created read-only; for example because they are snapshots";
            "of some other VDI.";
          ];
          "virtual_size", Basic Int64, String.concat " " [
            "Size of the VDI from the perspective of a VM (in bytes)";
          ];
          "uri", Array (Basic String), String.concat "" [
            "A list of URIs which can be opened and used for I/O. A URI could ";
            "reference a local block device, a remote NFS share, iSCSI LUN or ";
            "RBD volume. In cases where the data may be accessed over several ";
            "protocols, he list should be sorted into descending order of ";
            "desirability. Xapi will open the most desirable URI for which it has ";
            "an available datapath driver.";
          ]
        ]
      )) in
  let vdi_info = Type.Name "vdi" in
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
        TyDecl.name = "vdi_info";
        description = String.concat "" [
          "A set of properties associated with a VDI. These properties can ";
          "change dynamically and can be queried by the VDI.stat call.";
        ];
        ty = vdi_info_decl
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
          Interface.name = "Driver";
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
                {
                  Arg.name = "name_label";
                  ty = Basic String;
                  description = "A human-readable name to associate with the new disk. This name is intended to be short, to be a good summary of the disk."
                };
                {
                  Arg.name = "name_description";
                  ty = Basic String;
                  description = "A human-readable description to associate with the new disk. This can be arbitrarily long, up to the general string size limit."
                };
                {
                  Arg.name = "size";
                  ty = Basic Int64;
                  description = "A minimum size (in bytes) for the disk. Depending on the characteristics of the implementation this may be rounded up to (for example) the nearest convenient block size. The created disk will not be smaller than this size.";
                };
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
              Method.name = "resize";
              description = "[resize task sr vdi new_size] enlarges [vdi] to be at least [new_size].";
              inputs = [
                sr;
                vdi;
                { Arg.name = "new_size";
                  ty = Basic Int64;
                  description = "New disk size"
                }
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
              Method.name = "create";
              description = "[create task sr device_config physical_size]: creates a fresh SR";
              inputs = [
                sr;
                (*
                { Arg.name = "device_config";
                  ty = Type.(Dict(String, Basic String));
                  description = "Host-local SR configuration (e.g. address information)";
                };
                *)
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
              description = String.concat " " [
                "[scan sr] returns a paginated list of VDIs";
                "contained within an attached SR.";
              ];
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
