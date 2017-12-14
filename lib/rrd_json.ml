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

let json_of_ds ?(owner=Rrd.Host) ?(rshift=4) ds buf =
  let open Ds in
  let add_string str = 
    for _=0 to rshift-1 do Buffer.add_char buf ' ' done; 
    Buffer.add_string buf str in
  let json_line_string ?(last=false) n v = add_string (Printf.sprintf "  \"%s\": \"%s\"%s\n" n v (if last then "" else ","))
  and json_line_int64  ?(last=false) n v = add_string (Printf.sprintf "  \"%s\": \"%Ld\"%s\n" n v (if last then "" else ","))
  and json_line_float ?(last=false) n v  = add_string (Printf.sprintf "  \"%s\": \"%.2f\"%s\n" n v (if last then "" else ","))
  and json_line_bool  ?(last=false) n v = add_string (Printf.sprintf "\"%s\":\"%b\"%s" n v (if last then "" else ",")) in
  begin
    add_string (Printf.sprintf "\"%s\": {\n" ds.ds_name);
    if ds.ds_description != "" then (json_line_string "description" ds.ds_description);
    json_line_string "owner" (match owner with | Rrd.Host -> "host" | Rrd.VM vm -> "vm " ^ vm | Rrd.SR sr -> "sr " ^ sr);
    (match ds.ds_value with 
     | Rrd.VT_Int64 i -> json_line_int64 "value" i; json_line_string "value_type" "int64"
     | Rrd.VT_Float f -> json_line_float "value" f; json_line_string "value_type" "float"
     | Rrd.VT_Unknown  -> failwith "to_json: Impossible to represent VT_Unknown type");
    json_line_string "type" (match ds.ds_type with
        | Rrd.Gauge -> "gauge"
        | Rrd.Absolute -> "absolute"
        | Rrd.Derive -> "derive");
    json_line_bool "default" ds.ds_default;
    json_line_string "units" ds.ds_units;
    json_line_float "min" ds.ds_min;
    json_line_float ~last:true "max" ds.ds_max;

    add_string "},\n"; 
    (* begin *)
    (* 	Printf.printf "====== json_of_ds ======\n%!"; *)
    (* 	Printf.printf "%s%!" (Buffer.contents buf); *)
    (* 	Printf.printf "========================\n%!"; *)
    (* end; *)
  end

let json_of_dss ~header timestamp (dss : (Rrd.ds_owner * Ds.ds) list) =
  let buf = Buffer.create 100 in
  List.iter (fun (owner, ds) -> json_of_ds ~owner ds buf) dss;
  let dss = Buffer.contents buf in
  let payload =
    Printf.sprintf "{\n  \"timestamp\": %Ld,\n  \"datasources\": {\n%s\n  }\n}" timestamp
      (if String.length dss > 0 then (String.sub dss 0 (String.length dss - 2)) else "")
  in
  Printf.sprintf "%s%08x\n%s\n%s\n"
    header
    (String.length payload)
    (Digest.to_hex (Digest.string payload))
    payload

(** Write the JSON metadata of datasource [ds] (i.e. not the value itself) to
 * buffer [buf]. [owner] is Host unless specified. *)
let json_metadata_of_ds ?(owner=Rrd.Host) ds buf =
  let open Ds in
  let add_string str = Buffer.add_string buf str in
  let json_line_string ?(last=false) n v = add_string (Printf.sprintf "\"%s\":\"%s\"%s" n v (if last then "" else ","))
  and json_line_float  ?(last=false) n v = add_string (Printf.sprintf "\"%s\":\"%.2f\"%s" n v (if last then "" else ","))
  and json_line_bool  ?(last=false) n v = add_string (Printf.sprintf "\"%s\":\"%b\"%s" n v (if last then "" else ",")) in
  begin
    add_string (Printf.sprintf "\"%s\":{" ds.ds_name);
    if ds.ds_description != "" then (json_line_string "description" ds.ds_description);
    json_line_string "owner" (match owner with
        | Rrd.Host -> "host"
        | Rrd.VM vm -> "vm " ^ vm
        | Rrd.SR sr -> "sr " ^ sr);
    json_line_string "value_type" (match ds.ds_value with
        | Rrd.VT_Int64 _ -> "int64"
        | Rrd.VT_Float _ -> "float"
        | Rrd.VT_Unknown -> failwith "to_json: Impossible to represent VT_Unknown type");
    json_line_string "type" (match ds.ds_type with
        | Rrd.Gauge -> "gauge"
        | Rrd.Absolute -> "absolute"
        | Rrd.Derive -> "derive");
    json_line_bool "default" ds.ds_default;
    json_line_string "units" ds.ds_units;
    json_line_float "min" ds.ds_min;
    json_line_float ~last:true "max" ds.ds_max;
    add_string "}"
  end

(** Return a string containing the JSON metadata of a list of (ds * ds_owner)
  * tuples [dss]. The string will not contain the values of the datasources. *)
let json_metadata_of_dss (dss : (Rrd.ds_owner * Ds.ds) list) =
  let buf = Buffer.create 100 in
  Buffer.add_string buf "{\"datasources\":{";
  let rec add_dss = function
    | [] -> ()
    | (owner, ds) :: [] -> json_metadata_of_ds ~owner ds buf
    | (owner, ds) :: rest ->
      json_metadata_of_ds ~owner ds buf;
      Buffer.add_char buf ',';
      add_dss rest
  in
  add_dss dss;
  Buffer.add_string buf "}}";
  Buffer.contents buf
