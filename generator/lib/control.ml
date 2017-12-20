open Types
open Type

let api =
  let health_decl =
    Type.(Variant(
      ("Healthy", Basic String, "Storage is fully available"), [
       "Recovering", Basic String, "Storage is busy recovering, e.g. rebuilding mirrors.";
      ]
    )) in
  let sr_stat_decl =
    Type.(Struct(
        ( "sr", Name "sr", String.concat " " [
          "The URI identifying this volume. A typical value would be a";
          "file:// URI pointing to a directory or block device.";
          ]),
        [ "uuid", Option (Basic String), String.concat " " [
              "Uuid that uniquely identifies this SR, if one is available. ";
              "For SRs that are created by SR.create, this should be the ";
              "value passed into that call, if it is possible to persist ";
              "it."];
          "name", Basic String, String.concat " " [
          "Short, human-readable label for the SR.";
          ];
          "description", Basic String, String.concat " " [
            "Longer, human-readable description of the SR. Descriptions are";
            "generally only displayed by clients when the user is examining";
            "SRs in detail.";
          ];
          "free_space", Basic Int64, String.concat " " [
            "Number of bytes free on the backing storage (in bytes)";
          ];
          "total_space", Basic Int64, String.concat " " [
            "Total physical size of the backing storage (in bytes)";
          ];
          "datasources", Array (Basic String), String.concat " " [
            "URIs naming datasources: time-varying quantities representing anything";
            "from disk access latency to free space. The entities named by these URIs";
            "are self-describing.";
          ];
          "clustered", Basic Boolean, String.concat " " [
            "Indicates whether the SR uses clustered local storage.";
          ];
          "health", Name "health", String.concat " " [
            "The health status of the SR.";
          ];
        ]
      )) in
  let volume_decl =
    Type.(Struct(
        ( "key", Name "key", String.concat " " [
          "A primary key for this volume. The key must be unique";
          "within the enclosing Storage Repository (SR). A typical value would";
          "be a filename or an LVM volume name."
          ]),
        [ "uuid", Option (Basic String), String.concat " " [
          "A uuid (or guid) for the volume, if one is available.";
          "If a storage system has a built-in notion of a guid, then it";
          "will be returned here.";
          ];
          "name", Basic String, String.concat " " [
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
          "sharable", Basic Boolean, String.concat " " [
            "Indicates whether the VDI can be attached by";
            "multiple hosts at once.";
            "This is used for example by the HA statefile and XAPI redo log."
          ];
          "virtual_size", Basic Int64, String.concat " " [
            "Size of the volume from the perspective of a VM (in bytes)";
          ];
          "physical_utilisation", Basic Int64, String.concat " " [
            "Amount of space currently used on the backing storage (in bytes)";
          ];
          "uri", Array (Basic String), String.concat " " [
            "A list of URIs which can be opened and used for I/O. A URI could ";
            "reference a local block device, a remote NFS share, iSCSI LUN or ";
            "RBD volume. In cases where the data may be accessed over several ";
            "protocols, he list should be sorted into descending order of ";
            "desirability. Xapi will open the most desirable URI for which it has ";
            "an available datapath plugin.";
          ];
          "keys", Dict(String, Basic String), String.concat " " [
            "A lit of key=value pairs which have been stored in the Volume ";
            "metadata. These should not be interpreted by the Volume plugin.";
          ];
        ]
      )) in
  let volume = Type.Name "volume" in
  let sr_stat = Type.Name "sr_stat" in
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
  let uri = {
    Arg.name = "uri";
    ty = Type.(Basic String);
    description = "The Storage Repository URI";
  } in
  let uuid = {
    Arg.name = "uuid";
    ty = Type.(Basic String);
    description = "A uuid to associate with the SR."
  } in
  {
    Interfaces.name = "volume";
    title = "The Volume plugin interface";
    description =
      String.concat " " [
        "The xapi toolstack delegates all storage control-plane functions to ";
        "\"Volume plugins\".These plugins";
        "allow the toolstack to create/destroy/snapshot/clone volumes which";
        "are organised into groups called Storage Repositories (SR). Volumes";
        "have a set of URIs which can be used by the \"Datapath plugins\"";
        "to connect the disk data to VMs.";
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
      }; {
        TyDecl.name = "Activated_on_another_host";
        description = "The Volume is already active on another host";
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
        description = String.concat " " [
          "Primary key for a specific Storage Repository. This can be any ";
          "string which is meaningful to the implementation. For example this ";
          "could be an NFS directory name, an LVM VG name or even a URI.";
          "This string is abstract.";
        ];
        ty = Type.(Basic String);
      }; {
        TyDecl.name = "health";
        description = "The health status of an SR.";
        ty = health_decl;
      }; {
        TyDecl.name = "sr_stat";
        description = String.concat " " [
          "A set of high-level properties associated with an SR. These";
          "properties can change dynamically and can be queried by a";
          "SR.stat call.";
        ];
        ty = sr_stat_decl
      }; {
        TyDecl.name = "volume";
        description = String.concat " " [
          "A set of properties associated with a volume. These properties can ";
          "change dynamically and can be queried by the Volume.stat call.";
        ];
        ty = volume_decl
      }

    ];
    interfaces =
      [
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
                {
                  Arg.name = "sharable";
                  ty = Basic Boolean;
                  description = String.concat " " [
                    "Indicates whether the VDI can be attached by";
                    "multiple hosts at once.";
                    "This is used for example by the HA statefile and XAPI redo log."
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
                "Note the name and description are copied but any extra";
                "metadata associated by [set] is not copied.";
                "This can raise Activated_on_another_host(host_installation_uuid)";
                "if the VDI is already active on another host and snapshots";
                "can only be taken on the host that has the VDI active (if any).";
                "XAPI will take care of redirecting the request to the proper host"
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
                "clone of [volume] in [sr]. Note the name and description are";
                "copied but any extra metadata associated by [set] is not copied.";
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
              Method.name = "set_name";
              description = String.concat " " [
                "[set_name sr volume new_name] changes the name of [volume]";
              ];
              inputs = [
                sr;
                key;
                { Arg.name = "new_name";
                  ty = Basic String;
                  description = "New name"
                }
            ];
              outputs = [
              ];
            }; {
              Method.name = "set_description";
              description = String.concat " " [
                "[set_description sr volume new_description] changes the description of [volume]";
              ];
              inputs = [
                sr;
                key;
                { Arg.name = "new_description";
                  ty = Basic String;
                  description = "New description"
                }
            ];
              outputs = [
              ];
            }; {
              Method.name = "set";
              description = String.concat " " [
                "[set sr volume key value] associates [key] with [value] in the metadata of [volume]";
                "Note these keys and values are not interpreted by the plugin; they are intended for";
                "the higher-level software only.";
              ];
              inputs = [
                sr;
                key;
                { Arg.name = "k";
                  ty = Basic String;
                  description = "Key"
                }; {
                  Arg.name = "v";
                  ty = Basic String;
                  description = "Value"
                }
            ];
              outputs = [
              ];
            }; {
              Method.name = "unset";
              description = String.concat " " [
                "[unset sr volume key] removes [key] and any value associated with it from the metadata of [volume]";
                "Note these keys and values are not interpreted by the plugin; they are intended for";
                "the higher-level software only.";
              ];
              inputs = [
                sr;
                key;
                { Arg.name = "k";
                  ty = Basic String;
                  description = "Key"
                }
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
              Method.name = "probe";
              description = "[probe uri]: looks for existing SRs on the storage device";
              inputs = [
                uri;
              ];
              outputs = [
               { Arg.name = "result";
                 ty = 
                   Type.Struct (
                     ("srs", Type.(Array sr_stat), "SRs found on this storage device"), [
                      "uris", Type.(Array (Basic String)), "Other possible URIs which may contain SRs"
                   ]);
                 description = "Contents of the storage device";
               }
              ];
            };
            {
              Method.name = "create";
              description = "[create uuid uri name description configuration]: creates a fresh SR";
              inputs = [
                uuid;
                uri;
                { Arg.name = "name";
                  ty = Type.(Basic String);
                  description = "Human-readable name for the SR";
                }; {
                  Arg.name = "description";
                  ty = Type.(Basic String);
                  description = "Human-readable description for the SR";
                }; {
                  Arg.name = "configuration";
                  ty = Type.(Dict(String, Basic String));
                  description = String.concat " " [
                    "Plugin-specific configuration which describes where and";
                    "how to create the storage repository. This may include";
                    "the physical block device name, a remote NFS server and";
                    "path or an RBD storage pool.";
                  ];
                };
              ];
              outputs = [uri]
            };
            {
              Method.name = "attach";
              description = String.concat " "[
                "[attach uri]: attaches the SR to the local host. Once an SR";
                "is attached then volumes may be manipulated.";
              ];
              inputs = [
                uri;
              ];
              outputs = [
                sr;
              ];
            }; {
              Method.name = "detach";
              description = String.concat " " [
                "[detach sr]: detaches the SR, clearing up any associated";
                "resources. Once the SR is detached then volumes may not be";
                "manipulated.";
              ];
              inputs = [
                sr;
              ];
              outputs = [
              ];
            }; {
              Method.name = "destroy";
              description = String.concat " "[
                "[destroy sr]: destroys the [sr] and deletes any volumes";
                "associated with it. Note that an SR must be attached to be";
                "destroyed; otherwise Sr_not_attached is thrown.";
              ];
              inputs = [
                sr;
              ];
              outputs = [
              ];
            }; {
              Method.name = "stat";
              description = String.concat " " [
                "[stat sr] returns summary metadata associated with [sr]. Note this call does not return details of sub-volumes, see SR.ls.";
              ];
              inputs = [
                sr;
              ];
              outputs = [
                { Arg.name = "sr";
                  ty = sr_stat;
                  description = "SR metadata";
                }
              ];
            }; {
              Method.name = "set_name";
              description = String.concat " " [
                "[set_name sr new_name] changes the name of [sr]";
              ];
              inputs = [
                sr;
                { Arg.name = "new_name";
                  ty = Basic String;
                  description = "The new name of the SR"
                }
            ];
              outputs = [
              ];
            }; {
              Method.name = "set_description";
              description = String.concat " " [
                "[set_description sr new_description] changes the description of [sr]";
              ];
              inputs = [
                sr;
                { Arg.name = "new_description";
                  ty = Basic String;
                  description = "The new description for the SR";
                }
              ];
              outputs = [
              ];
            }; {
              Method.name = "ls";
              description = String.concat " " [
                "[ls sr] returns a list of volumes";
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
