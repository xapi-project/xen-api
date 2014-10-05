open Types

let api =
  {
    Interfaces.name = "Storage Datapath plugin";
    title = "The Storage Datapath plugin interface";
    description = String.concat " " [
      "The Datapath plugin takes a URI which points to virtual disk data and";
      "chooses a Xen datapath implementation: driver domain, blkback implementation";
      "and caching strategy."
    ];
    exn_decls = [
    ];
    type_decls = [
    ];
    interfaces = [
      {
        Interface.name = "Datapath";
        description = String.concat " " [
          "..."
        ];
        type_decls = [ ];
        methods = [
          {
            Method.name = "attach";
            description = "[attach]";
            inputs = [
            ];
            outputs = [
            ];
          }; {
            Method.name = "activate";
            description = "[activate]";
            inputs = [
            ];
            outputs = [
            ];
          }; {
            Method.name = "deactivate";
            description = "[deactivate]";
            inputs = [
            ];
            outputs = [
            ];
          }; {
            Method.name = "detach";
            description = "[detach]";
            inputs = [
            ];
            outputs = [
            ];
          }
        ]
      }
    ];
  }
