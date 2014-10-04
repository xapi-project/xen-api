open Types

let api =
  let volume_decl =
    Type.(Struct(
        ( "key", Name "key", String.concat " " [
          "A primary key for this volume. The key must be unique";
          "within the enclosing Storage Repository (SR). A typical value would";
          "be a filename or an LVM volume name."
          ]),
        [ "name", Basic String, String.concat " " [
          "Short, human-readable label for the volume. Names are commonly used by";
          "when displaying short lists of volumes.";
          ];
          "description", Basic String, String.concat " " [
            "Longer, human-readable description of the volume. Descriptions are";
            "generally only displayed by clients when the user is examining";
            "volumes individually.";
          ];
          "read_write", Basic Boolean, String.concat " " [
            "True means the VDI may be written to, false means the volume is";
            "read-only. Some storage media is read-only so all volumes are";
            "read-only; for example .iso disk images on an NFS share. Some";
            "volume are created read-only; for example because they are snapshots";
            "of some other VDI.";
          ];
          "virtual_size", Basic Int64, String.concat " " [
            "Size of the volume from the perspective of a VM (in bytes)";
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
  let volume = Type.Name "volume" in
  let sr = {
    Arg.name = "sr";
    ty = Type.(Basic String);
    description = "The Storage Repository";
  } in
  let key = {
    Arg.name = "key";
    ty = Type.(Basic String);
    description = "The volume key";
  } in
  {
    Interfaces.name = "control";
    title = "The storage control-plane";
    description =
      String.concat "" [
        "The xapi toolstack delegates all storage control-plane functions to ";
        "\"drivers\", also known as \"Storage Manager plugins\". These drivers";
        "allow the toolstack to create/destroy/snapshot/clone volumes which";
        "are organised into groups called Storage Repositories (SR). Volumes";
        "have a set of URIs which can be used by the storage data-plane to";
        "read and write the disk data.";
      ];
    exn_decls = [
      {
        TyDecl.name = "Sr_not_attached";
        description = "An SR must be attached in order to access volumes";
        ty = Type.(Basic String)
      }; {
        TyDecl.name = "SR_does_not_exist";
        description = "The specified SR could not be found";
        ty = Type.(Basic String)
      }; {
        TyDecl.name = "Volume_does_not_exist";
        description = "The specified volume could not be found in the SR";
        ty = Type.(Basic String)
      }; {
        TyDecl.name = "Unimplemented";
        description = "The operation has not been implemented";
        ty = Type.(Basic String);
      }; {
        TyDecl.name = "Cancelled";
        description = "The task has been asynchronously cancelled";
        ty = Type.(Basic String);
      };
    ];
    type_decls = [
      {
        TyDecl.name = "key";
        description = String.concat " " [
          "Primary key for a volume. This can be any string which";
          "is meaningful to the implementation. For example this could be an";
          "NFS filename, an LVM LV name or even a URI. This string is";
          "abstract."
          ];
        ty = Type.(Basic String);
      }; {
        TyDecl.name = "sr";
        description = String.concat "" [
          "Primary key for a specific Storage Repository. This can be any ";
          "string which is meaningful to the implementation. For example this ";
          "could be an NFS directory name, an LVM VG name or even a URI.";
          "This string is abstract.";
        ];
        ty = Type.(Basic String);
      }; {
        TyDecl.name = "volume";
        description = String.concat "" [
          "A set of properties associated with a volume. These properties can ";
          "change dynamically and can be queried by the Volume.stat call.";
        ];
        ty = volume_decl
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
            "a storage driver by xapi.";
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
          Interface.name = "Volume";
          description = "Operations which operate on volumes (also known as Virtual Disk Images)";
          type_decls = [];
          methods = [
            {
              Method.name = "create";
              description = String.concat " " [
                "[create sr name description size] creates a new volume in [sr]";
                "with [name] and [description]. The volume will have size";
                ">= [size] i.e. it is always permissable for an implementation";
                "to round-up the volume to the nearest convenient block size";
              ];
              inputs = [
                sr;
                {
                  Arg.name = "name";
                  ty = Basic String;
                  description = String.concat " " [
                    "A human-readable name to associate with the new disk. This";
                    "name is intended to be short, to be a good summary of the";
                    "disk."
                  ]
                };
                {
                  Arg.name = "description";
                  ty = Basic String;
                  description = String.concat " " [
                    "A human-readable description to associate with the new";
                    "disk. This can be arbitrarily long, up to the general";
                    "string size limit."
                  ]
                };
                {
                  Arg.name = "size";
                  ty = Basic Int64;
                  description = String.concat " " [
                    "A minimum size (in bytes) for the disk. Depending on the";
                    "characteristics of the implementation this may be rounded";
                    "up to (for example) the nearest convenient block size. The";
                    "created disk will not be smaller than this size.";
                  ]
                };
              ];
              outputs = [
                { Arg.name = "volume";
                  ty = volume;
                  description = "Properties of the created volume";
                }
              ];
            }; {
              Method.name = "snapshot";
              description = String.concat " " [
                "[snapshot sr volume] creates a new volue which is a ";
                "snapshot of [volume] in [sr]. Snapshots should never be";
                "written to; they are intended for backup/restore only.";
              ];
              inputs = [
                sr;
                key;
              ];
              outputs = [
                { Arg.name = "volume";
                  ty = volume;
                  description = "Properties of the created volume";
                }
              ];
            }; {
              Method.name = "clone";
              description = String.concat " " [
                "[clone sr volume] creates a new volume which is a writable";
                "clone of [volume] in [sr].";
              ];
              inputs = [
                sr;
                key;
              ];
              outputs = [
                { Arg.name = "volume";
                  ty = volume;
                  description = "Properties of the created volume";
                }
              ];
            }; {
              Method.name = "destroy";
              description = "[destroy sr volume] removes [volume] from [sr]";
              inputs = [
                sr;
                key;
              ];
              outputs = [
              ];
            }; {
              Method.name = "resize";
              description = String.concat " " [
                "[resize sr volume new_size] enlarges [volume] to be at least";
                "[new_size].";
              ];
              inputs = [
                sr;
                key;
                { Arg.name = "new_size";
                  ty = Basic Int64;
                  description = "New disk size"
                }
            ];
              outputs = [
              ];
            }; {
              Method.name = "stat";
              description = String.concat " " [
                "[stat sr volume] returns metadata associated with [volume].";
              ];
              inputs = [
                sr;
                key;
              ];
              outputs = [
                { Arg.name = "volume";
                  ty = volume;
                  description = "Volume metadata";
                }
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
                "[scan sr] returns a paginated list of volumes";
                "contained within an attached SR.";
              ];
              inputs = [
                sr;
              ];
              outputs = [
                {
                  Arg.name = "volumes";
                  ty = Type.(Array (Name "volume"));
                  description = "List of all the visible volumes in the SR";
                }
              ];
            }
          ]
        }
      ]
  }
