(* data source *)

(** This is used both for updating the DSs and for creating them *)
type ds = { ds_name: string;
	    ds_description: string;
	    ds_value: Rrd.ds_value_type;
	    ds_type: Rrd.ds_type;
	    ds_default: bool;
	    ds_min: float;
	    ds_max: float;
	    ds_units: string;
	    ds_pdp_transform_function: float -> float;
}

let ds_make ~name ~description ~value ~ty ~default ?(min=neg_infinity) ?(max=infinity) ?(units="") ?(transform=(fun x -> x)) () =
  { ds_name=name;
    ds_description=description;
    ds_value=value;
    ds_type=ty;
    ds_default=default;
    ds_min=min;
    ds_max=max;
    ds_units=units;
    ds_pdp_transform_function=transform;
  }
