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

open Rrd_protocol

let (|>) a b = b a

let default_header = "DATASOURCES\n"
let header_bytes = String.length default_header
let length_start = header_bytes
let length_bytes = 8 (* hex length of payload *)
let checksum_start = header_bytes + length_bytes + 1 (* newline *)
let checksum_bytes = 32 (* hex length of checksum *)
let payload_start = header_bytes + length_bytes + checksum_bytes + 2 (* 2 newlines *)

(* Possible types for values in datasources. *)
type value_type = Float | Int64

(* Converts string to datasource value type. *)
let val_ty_of_string (s : string) : value_type =
  match String.lowercase s with
  | "float" -> Float
  | "int64" -> Int64
  | _ -> raise Invalid_payload

(* Converts an RPC value to a typed datasource value. *)
let ds_value_of_rpc ~(ty : value_type) ~(rpc : Rpc.t) : Rrd.ds_value_type =
  match ty with
  | Float -> Rrd.VT_Float (Rpc.float_of_rpc rpc)
  | Int64 -> Rrd.VT_Int64 (Rpc.int64_of_rpc rpc)

(* A function that converts a JSON type into a datasource type, assigning
 * default values appropriately. *)
let ds_of_rpc ((name, rpc) : (string * Rpc.t)) : (Rrd.ds_owner * Ds.ds) =
  try
    let kvs = Rrd_rpc.dict_of_rpc ~rpc in
    let description = Rrd_rpc.assoc_opt ~key:"description" ~default:"" kvs in
    let units = Rrd_rpc.assoc_opt ~key:"units" ~default:"" kvs in
    let ty =
      Rrd_rpc.ds_ty_of_string
        (Rrd_rpc.assoc_opt ~key:"type" ~default:"absolute" kvs)
    in
    let val_ty =
      val_ty_of_string
        (Rrd_rpc.assoc_opt ~key:"value_type" ~default:"float" kvs)
    in
    let value =
      let value_rpc = List.assoc "value" kvs in
      ds_value_of_rpc ~ty:val_ty ~rpc:value_rpc
    in
    let min =
      float_of_string (Rrd_rpc.assoc_opt ~key:"min" ~default:"-infinity" kvs) in
    let max =
      float_of_string (Rrd_rpc.assoc_opt ~key:"max" ~default:"infinity" kvs) in
    let owner =
      Rrd_rpc.owner_of_string
        (Rrd_rpc.assoc_opt ~key:"owner" ~default:"host" kvs) in
    let default =
      bool_of_string (Rrd_rpc.assoc_opt ~key:"default" ~default:"false" kvs) in
    let ds =
      Ds.ds_make ~name ~description ~units ~ty ~value ~min ~max ~default () in
    owner, ds
  with e -> raise e

(* A function that parses the payload written by a plugin into the payload
 * type. *)
let parse_payload ~(json : string) : payload =
  try
    let rpc = Jsonrpc.of_string json in
    let kvs = Rrd_rpc.dict_of_rpc ~rpc in
    let timestamp = Rpc.int64_of_rpc (List.assoc "timestamp" kvs) in
    let datasource_rpcs = Rrd_rpc.dict_of_rpc ~rpc:(List.assoc "datasources" kvs) in
    {timestamp; datasources = List.map ds_of_rpc datasource_rpcs}
  with _ -> raise Invalid_payload

let make_payload_reader () =
  let last_checksum = ref "" in
  (fun cs ->
     let header = Cstruct.copy cs 0 header_bytes in
     if header <> default_header then
       raise Invalid_header_string;
     let length =
       let length_str = "0x" ^ (Cstruct.copy cs length_start length_bytes) in
       try int_of_string length_str with _ -> raise Invalid_length
     in
     let checksum = Cstruct.copy cs checksum_start checksum_bytes in
     let payload_string = Cstruct.copy cs payload_start length in
     if payload_string |> Digest.string |> Digest.to_hex <> checksum then
       raise Invalid_checksum;
     if checksum = !last_checksum
     then raise No_update
     else last_checksum := checksum;
     parse_payload ~json:payload_string)

let write_payload alloc_cstruct payload =
  let json =
    Rrd_json.json_of_dss ~header:default_header payload.timestamp payload.datasources
  in
  let length = String.length json in
  let cs = alloc_cstruct length in
  Cstruct.blit_from_string json 0 cs 0 length

let make_payload_writer () = write_payload

let protocol = {
  make_payload_reader;
  make_payload_writer;
}
