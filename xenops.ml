open Types

let api =
  {
    Interfaces.name = "xenops";
    description = "The Xen domain management API";
    type_decls = [
      { TyDecl.name = "disk";
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
	  type_decls = [ { TyDecl.name = "video_card";
			   description = "Type of video hardware";
			   ty = Type.(Variant(
			     ("Cirrus", Unit, "Cirrus Logic"),
			     [ "Standard_VGA", Unit, "Standard VGA" ]
			   ));
			 }

		       ];
	  methods = []
	}
      ]
    }
