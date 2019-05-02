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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

let string fmt = Printf.ksprintf (fun msg -> `String msg) fmt

let ds_value = function
  | Rrd.VT_Float x ->
    [ "value",      string "%.2f" x
    ; "value_type", string "float"
    ]
  | Rrd.VT_Int64 n ->
    [ "value",      string "%Ld" n
    ; "value_type", string "int64"
    ]
  | Rrd.VT_Unknown ->
    failwith "to_json: Impossible to represent VT_Unknown"

let ds_type x =
  "type",  match x with
  | Rrd.Gauge     -> string "gauge"
  | Rrd.Absolute  -> string "absolute"
  | Rrd.Derive    -> string "derive"

let ds_owner x =
  "owner", match x with
  | Rrd.VM vm   -> string "vm %s" vm
  | Rrd.Host    -> string "host"
  | Rrd.SR sr   -> string "sr %s" sr

let bool b    = string "%b" b (* Should use `Bool b *)
let float x   = string "%.2f" x
let record xs = `O xs

let description = function
  | ""  -> []
  | str -> ["description", string "%s" str]

let ds_to_json (owner, ds) =
  ds.Ds.ds_name, record @@ List.concat
    [ description ds.Ds.ds_description
    ; [ ds_owner owner ]
    ; ds_value ds.Ds.ds_value
    ; [ ds_type ds.Ds.ds_type ]
    ; [ "default" , bool ds.Ds.ds_default
      ; "units"   , string "%s" ds.Ds.ds_units
      ; "min"     , float ds.Ds.ds_min
      ; "max"     , float ds.Ds.ds_max
      ]
    ]

let dss_to_json ~header timestamp dss =
  let payload = record
      [ "timestamp", `Float (Int64.to_float timestamp)
      ; "datasources", record @@ List.map ds_to_json dss
      ] in
  let buf    = Buffer.create 2048 in
  let out    = Buffer.create 2048 in
  let ()     = Ezjsonm.to_buffer buf payload in
  let json   = Buffer.contents buf in
  let digest = Digest.string json |> Digest.to_hex in
  Buffer.add_string out @@
  Printf.sprintf "%s%08x\n%s\n" header (Buffer.length buf) digest;
  Buffer.add_string out json;
  Buffer.add_char out '\n';
  Buffer.contents out

let metadata_to_json (dss: (Rrd.ds_owner * Ds.ds) list) =
  let json = record [ "datasources", record @@ List.map ds_to_json dss ] in
  let buf  = Buffer.create 2048 in
  let ()   = Ezjsonm.to_buffer buf json in
  Buffer.contents buf

let json_of_dss          = dss_to_json
let json_metadata_of_dss = metadata_to_json

