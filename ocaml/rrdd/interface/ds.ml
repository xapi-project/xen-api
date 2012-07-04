(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)
(** Data source
 * @group Performance Monitoring
 *)

(** This is used both for updating the DSs and for creating them *)
type ds = {
	ds_name : string;
	ds_description : string;
	ds_value : Rrd.ds_value_type;
	ds_type : Rrd.ds_type;
	ds_default : bool;
	ds_min : float;
	ds_max : float;
	ds_units : string;
	ds_pdp_transform_function : float -> float;
}

let ds_make ~name ~description ~value ~ty ~default ~units
		?(min = neg_infinity) ?(max = infinity) ?(transform = (fun x -> x)) () = {
	ds_name = name;
	ds_description = description;
	ds_value = value;
	ds_type = ty;
	ds_default = default;
	ds_min = min;
	ds_max = max;
	ds_units = units;
	ds_pdp_transform_function = transform;
}
