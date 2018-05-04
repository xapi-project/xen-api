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

type t = {
	name : string;
	description : string;
	enabled : bool;
	standard : bool;
	min : float;
	max : float;
	units : string
} [@@deriving rpcty]

let to_key_value_map ds = [
	"name_label", ds.name;
	"name_description", ds.description;
	"enabled", string_of_bool ds.enabled;
	"standard", string_of_bool ds.standard;
	"min", string_of_float ds.min;
	"max", string_of_float ds.max;
	"units", ds.units;
]
