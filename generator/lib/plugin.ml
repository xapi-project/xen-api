open Types

let api =
  {
    Interfaces.name = "plugin";
    title = "The Plugin interface";
    description =
      String.concat " " [
        "The xapi toolstack expects all plugins to support a basic query";
        "interface.";
      ];
    exn_decls = [
      {
        TyDecl.name = "Unimplemented";
        description = "The operation has not been implemented";
        ty = Type.(Basic String);
      }
    ];
    type_decls = [
      {
        TyDecl.name = "query_result";
        description = "properties of this implementation";
        ty = Type.(Struct( ("plugin", Basic String, "plugin name, used in the XenAPI as SR.type"), [
            "name", Basic String, "short name";
            "description", Basic String, "description";
            "vendor", Basic String, "entity (e.g. company, project, group) which produced this implementation";
            "copyright", Basic String, "copyright";
            "version", Basic String, "version";
            "required_api_version", Basic String, "minimum required API version";
            "features", Array (Basic String), "features supported by this plugin";
            "configuration", Dict(String, Basic String), "key/description pairs describing required device_config parameters";
            "required_cluster_stack", Array (Basic String), "the plugin requires one of these cluster stacks to be active";
          ]))
      }
    ];
    interfaces =
      [
        {
          Interface.name = "Plugin";
          description = String.concat " " [
            "Discover properties of this implementation. Every implementation ";
            "must support the query interface or it will not be recognised as ";
            "a storage plugin by xapi.";
          ];
          type_decls = [
          ];
          methods = [
            {
              Method.name = "query";
              description = String.concat " " [
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
              description = String.concat " " [
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
      ]
  }
