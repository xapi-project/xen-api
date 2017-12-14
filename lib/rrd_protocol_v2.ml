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

open Crc
open Rrd_protocol

(* Field sizes. *)
let default_header = "DATASOURCES"

let header_bytes = String.length default_header

let data_crc_bytes = 4

let metadata_crc_bytes = 4

let datasource_count_bytes = 4

let timestamp_bytes = 8

let datasource_value_bytes = 8

let metadata_length_bytes = 4

let get_total_bytes datasource_count metadata_length =
  header_bytes +
  data_crc_bytes +
  metadata_crc_bytes +
  datasource_count_bytes +
  timestamp_bytes +
  (datasource_value_bytes * datasource_count) +
  metadata_length_bytes +
  metadata_length

(* Field start points. *)
let header_start = 0

let data_crc_start = header_start + header_bytes

let metadata_crc_start = data_crc_start + data_crc_bytes

let datasource_count_start = metadata_crc_start + metadata_crc_bytes

let timestamp_start = datasource_count_start + datasource_count_bytes

let datasource_value_start = timestamp_start + timestamp_bytes

let get_metadata_length_start datasource_count =
  datasource_value_start + (datasource_count * datasource_value_bytes)

let get_metadata_start datasource_count =
  (get_metadata_length_start datasource_count) + metadata_length_bytes

(* Reading fields from cstructs. *)
module Read = struct
  let header cs =
    let header = Bytes.create header_bytes in
    Cstruct.blit_to_bytes cs header_start header 0 header_bytes;
    header

  let data_crc cs =
    Cstruct.BE.get_uint32 cs data_crc_start

  let metadata_crc cs =
    Cstruct.BE.get_uint32 cs metadata_crc_start

  let datasource_count cs =
    Int32.to_int (Cstruct.BE.get_uint32 cs datasource_count_start)

  let timestamp cs =
    Cstruct.BE.get_uint64 cs timestamp_start

  let datasource_values cs cached_datasources =
    let rec aux start acc = function
      | [] -> acc
      | (owner, cached_datasource) :: rest -> begin
          (* Replace the cached datasource's value with the value read
             				 * from the cstruct. *)
          let value = match cached_datasource.Ds.ds_value with
            | Rrd.VT_Float _ ->
              Rrd.VT_Float (Int64.float_of_bits (Cstruct.BE.get_uint64 cs start))
            | Rrd.VT_Int64 _ ->
              Rrd.VT_Int64 (Cstruct.BE.get_uint64 cs start)
            | Rrd.VT_Unknown -> failwith "unknown datasource type"
          in
          aux (start + datasource_value_bytes)
            ((owner, {cached_datasource with Ds.ds_value = value}) :: acc)
            rest
        end
    in
    List.rev (aux datasource_value_start [] cached_datasources)

  let metadata_length cs datasource_count =
    Int32.to_int
      (Cstruct.BE.get_uint32 cs (get_metadata_length_start datasource_count))

  let metadata cs datasource_count metadata_length =
    let metadata = Bytes.create metadata_length in
    Cstruct.blit_to_bytes
      cs (get_metadata_start datasource_count)
      metadata 0 metadata_length;
    metadata
end

(* Writing fields to cstructs. *)
module Write = struct
  let header cs =
    Cstruct.blit_from_string default_header 0 cs header_start header_bytes

  let data_crc cs value =
    Cstruct.BE.set_uint32 cs data_crc_start value

  let metadata_crc cs value =
    Cstruct.BE.set_uint32 cs metadata_crc_start value

  let datasource_count cs value =
    Cstruct.BE.set_uint32 cs datasource_count_start (Int32.of_int value)

  let timestamp cs value =
    Cstruct.BE.set_uint64 cs timestamp_start value

  let datasource_values cs values =
    let rec aux start = function
      | [] -> ()
      | value :: rest -> begin
          let to_write = match value with
            | Rrd.VT_Float f -> Int64.bits_of_float f
            | Rrd.VT_Int64 i -> i
            | Rrd.VT_Unknown -> failwith "unknown datasource type"
          in
          Cstruct.BE.set_uint64 cs start to_write;
          aux (start + datasource_value_bytes) rest
        end
    in
    aux datasource_value_start values

  let metadata_length cs value datasource_count =
    Cstruct.BE.set_uint32 cs
      (get_metadata_length_start datasource_count) (Int32.of_int value)

  let metadata cs value datasource_count =
    let metadata_length = String.length value in
    Cstruct.blit_from_string value 0 cs
      (get_metadata_start datasource_count)
      metadata_length
end

let default_value_of_string (s : string) : Rrd.ds_value_type =
  match s with
  | "float" -> Rrd.VT_Float 0.0
  | "int64" -> Rrd.VT_Int64 0L
  | _ -> raise Invalid_payload

(* WARNING! This creates datasources from datasource metadata, hence the
 * values will be meaningless. The types however, will be correct. *)
let uninitialised_ds_of_rpc ((name, rpc) : (string * Rpc.t))
  : (Rrd.ds_owner * Ds.ds) =
  let kvs = Rrd_rpc.dict_of_rpc ~rpc in
  let description = Rrd_rpc.assoc_opt ~key:"description" ~default:"" kvs in
  let units = Rrd_rpc.assoc_opt ~key:"units" ~default:"" kvs in
  let ty =
    Rrd_rpc.ds_ty_of_string
      (Rrd_rpc.assoc_opt ~key:"type" ~default:"absolute" kvs)
  in
  let value =
    default_value_of_string (Rpc.string_of_rpc (List.assoc "value_type" kvs)) in
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

let parse_metadata metadata =
  try
    let rpc = Jsonrpc.of_string metadata in
    let kvs = Rrd_rpc.dict_of_rpc ~rpc in
    let datasource_rpcs = Rrd_rpc.dict_of_rpc ~rpc:(List.assoc "datasources" kvs) in
    List.map uninitialised_ds_of_rpc datasource_rpcs
  with _ -> raise Invalid_payload

let make_payload_reader () =
  let last_data_crc = ref 0l in
  let last_metadata_crc = ref 0l in
  let cached_datasources : (Rrd.ds_owner * Ds.ds) list ref = ref [] in
  (fun cs ->
     (* Check the header string is present and correct. *)
     let header = Read.header cs in
     if not (header = default_header) then
       raise Invalid_header_string;
     (* Check that the data CRC has changed. Since the CRC'd data
        		 * includes the timestamp, this should change with every update. *)
     let data_crc = Read.data_crc cs in
     if data_crc = !last_data_crc then raise No_update;
     let metadata_crc = Read.metadata_crc cs in
     let datasource_count = Read.datasource_count cs in
     let timestamp = Read.timestamp cs in
     (* Check the data crc is correct. *)
     let data_crc_calculated =
       Crc32.cstruct
         (Cstruct.sub cs timestamp_start
            (timestamp_bytes + datasource_count * datasource_value_bytes))
     in
     if not (data_crc = data_crc_calculated)
     then raise Invalid_checksum
     else last_data_crc := data_crc;
     (* Read the datasource values. *)
     let datasources =
       if metadata_crc = !last_metadata_crc then begin
         (* Metadata hasn't changed, so just read the datasources values. *)
         Read.datasource_values cs !cached_datasources
       end else begin
         (* Metadata has changed - we need to read it to find the types of the
            				 * datasources, then go back and read the values themselves. *)
         let metadata_length = Read.metadata_length cs datasource_count in
         let metadata = Read.metadata cs datasource_count metadata_length in
         (* Check the metadata checksum is correct. *)
         if not (metadata_crc = Crc32.string metadata 0 metadata_length)
         then raise Invalid_checksum;
         (* If all is OK, cache the metadata checksum and read the values
            				 * based on this new metadata. *)
         last_metadata_crc := metadata_crc;
         Read.datasource_values cs (parse_metadata metadata)
       end
     in
     cached_datasources := datasources;
     {
       timestamp = timestamp;
       datasources = datasources;
     })

let write_payload alloc_cstruct payload =
  let metadata = Rrd_json.json_metadata_of_dss payload.datasources in
  let datasource_count = List.length payload.datasources in
  let metadata_length = String.length metadata in
  let total_bytes = get_total_bytes datasource_count metadata_length in
  let cs = alloc_cstruct total_bytes in
  (* Write header. *)
  Write.header cs;
  (* Write metadata checksum. *)
  Write.metadata_crc cs (Crc32.string metadata 0 metadata_length);
  (* Write number of datasources. *)
  Write.datasource_count cs datasource_count;
  (* Write timestamp. *)
  Write.timestamp cs payload.timestamp;
  (* Write datasource values. *)
  Write.datasource_values cs
    (List.map (fun (_, ds) -> ds.Ds.ds_value) payload.datasources);
  (* Write the metadata. *)
  Write.metadata cs metadata datasource_count;
  (* Write the metadata length. *)
  Write.metadata_length cs metadata_length datasource_count;
  (* Write the data checksum. *)
  let data_crc = Crc32.cstruct
      (Cstruct.sub cs timestamp_start
         (timestamp_bytes + datasource_count * datasource_value_bytes))
  in
  Write.data_crc cs data_crc

let make_payload_writer () = write_payload

let protocol = {
  make_payload_reader;
  make_payload_writer;
}
