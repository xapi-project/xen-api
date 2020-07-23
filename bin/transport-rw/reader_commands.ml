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

let string_of_data_source owner ds =
  let owner_string = match owner with
    | Rrd.Host -> "Host"
    | Rrd.SR sr -> "SR " ^ sr
    | Rrd.VM vm -> "VM " ^ vm
  in
  let value_string = match ds.Ds.ds_value with
    | Rrd.VT_Float f -> Printf.sprintf "float %f" f
    | Rrd.VT_Int64 i -> Printf.sprintf "int64 %Ld" i
    | Rrd.VT_Unknown -> Printf.sprintf "unknown"
  in
  let type_string = match ds.Ds.ds_type with
    | Rrd.Absolute -> "absolute"
    | Rrd.Gauge -> "gauge"
    | Rrd.Derive -> "derive"
  in
  Printf.sprintf
    "owner: %s\nname: %s\ntype: %s\nvalue: %s\nunits: %s"
    owner_string ds.Ds.ds_name type_string value_string ds.Ds.ds_units

let interpret_payload payload =
  print_endline "------------ Metadata ------------";
  Printf.printf "timestamp = %Ld\n%!" payload.timestamp;
  print_endline "---------- Data sources ----------";
  List.iter
    (fun (owner, ds) ->
       print_endline (string_of_data_source owner ds);
       print_endline "----------")
    payload.datasources

let main_loop reader interval =
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle (fun _ -> reader.Rrd_reader.cleanup (); exit 0));
  try
    while true do
      let payload = reader.Rrd_reader.read_payload () in
      interpret_payload payload;
      Thread.delay interval
    done
  with e ->
    reader.Rrd_reader.cleanup ();
    raise e

let protocol_of_string = function
  | "v1" -> Rrd_protocol_v1.protocol
  | "v2" -> Rrd_protocol_v2.protocol
  | _ -> failwith "Unknown protocol"


let read_file once path protocol =
  let protocol = protocol_of_string protocol in
  let reader = Rrd_reader.FileReader.create path protocol in
  if once then begin
    reader.Rrd_reader.read_payload () |> interpret_payload;
    reader.Rrd_reader.cleanup ()
  end else
    main_loop reader 5.0

let read_page domid grantref protocol =
  let protocol = protocol_of_string protocol in
  let reader =
    Rrd_reader.PageReader.create
      {Rrd_reader.frontend_domid = domid; shared_page_refs = [grantref]}
      protocol
  in
  main_loop reader 5.0
