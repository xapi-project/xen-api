open Types

let api =
  {
    Interfaces.name = "memory";
    title = "Memory manager";
    description = "The memory management service (also known as \"squeezed\") shares host memory among running VMs via their balloon drivers. Each VM has an associated memory range (minimum and maximum amount of allocated memory) and the manager attempts to share the host memory fairly.";
    exn_decls = [
    ];
    type_decls = [
      {
        TyDecl.name = "reservation_id";
        description = "A reference (or handle) to a memory reservation";
        ty = Type.(Basic String);
      }; {
        TyDecl.name = "domid";
        description = "A domain id";
        ty = Type.(Basic Int64);
      }; {
        TyDecl.name = "kib";
        description = "An amount of memory in KiB";
        ty = Type.(Basic Int64);
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
              { Arg.name = "amount";
                ty = Type.Name "kib";
                description = "Amount of memory to reserve on the host";
              }
            ];
            outputs = [
              { Arg.name = "reservation_id";
                ty = Type.Name "reservation_id";
                description = "A reference to a reservation which can be transferred to a VM";
              }
            ];
          }; {
            Method.name = "reserve_range";
            description = "[reserve_range min_kib max_kib] reserves as much as possible between [min_kib] and [max_kib] of memory for some undisclosed purpose and returns a reservation id";
            inputs = [
              { Arg.name = "min";
                ty = Type.Name "kib";
                description = "Minimum amount of memory to reserve on the host";
              }; {
                Arg.name = "max";
                ty = Type.Name "kib";
                description = "Maximum amount of memory to reserve on the host";
              }
            ];
            outputs = [
              { Arg.name = "reservation_id";
                ty = Type.Name "reservation_id";
                description = "A reference to a reservation which can be transferred to a VM";
              }
            ];
          }; {
            Method.name = "transfer_to_domain";
            description = "[transfer_to_domain reservation_id domid] transfers the memory reserved by [reservation_id] to domain [domid]";
            inputs = [
              { Arg.name = "reservation_id";
                ty = Type.Name "reservation_id";
                description = "A reference to a memory reservation";
              }; {
                Arg.name = "domid";
                ty = Type.Name "domid";
                description = "Domain id to transfer the reservation to";
              }
            ];
            outputs = []
          }; {
            Method.name = "delete";
            description = "[delete reservation_id] deletes the reservation identified by [reservation_id]";
            inputs = [
              { Arg.name = "reservation_id";
                ty = Type.Name "reservation_id";
                description = "A reference to a memory reservation";
              }
            ];
            outputs = [];
          }; {
            Method.name = "balance";
            description = "Rebalance memory allocations between domains";
            inputs = [];
            outputs = [];
          }
        ]
      }
    ];
  }
