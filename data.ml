open Types

let api =
  {
    Interfaces.name = "data";
    title = "Memory manager";
    description = "The memory management service (also known as \"squeezed\") shares host memory among running VMs via their balloon drivers. Each VM has an associated memory range (minimum and maximum amount of allocated memory) and the manager attempts to share the host memory fairly.";
    exn_decls = [
    ];
    type_decls = [
      {
        TyDecl.name = "reservation_id";
        description = "A reference (or handle) to a memory reservation";
        ty = Type.(Basic String);
      }
    ];
    interfaces = [
      {
        Interface.name = "Memory";
        description = "Create and manipulate VM memory reservations";
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
            Method.name = "reserve";
            description = "[reserve kib] reserves [kib] of memory for some undisclosed purpose and returns a reservation id";
            inputs = [
            ];
            outputs = [
              { Arg.name = "reservation_id";
                ty = Type.Name "reservation_id";
                description = "A reference to a reservation which can be transferred to a VM";
              }
            ];
          }
        ]
      }
    ];
  }
