open Types

let api =
  {
    Interfaces.name = "xenops";
    description = "The Xen domain management API";
    type_decls = [];
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
	}
      ]
    }
