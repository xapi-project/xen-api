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

let now () = Int64.of_float (Unix.gettimeofday ())

let get_extra_data_sources_flag =
  let counter = ref 0 in
  (fun () ->
     let result = match !counter / 8 with
       | 0 -> false
       | _ -> true
     in
     if !counter >= 15 then counter := 0 else incr counter;
     result)

let generate_time_data_source () =
  let current_time = now () in
  Rrd.Host,
  Ds.ds_make ~name:"current_time"
    ~description:"The current time"
    ~value:(Rrd.VT_Int64 current_time) ~ty:(Rrd.Gauge)
    ~default:true ~units:"seconds" ()

let generate_random_int_data_source () =
  Rrd.SR "my_sr",
  Ds.ds_make ~name:"random_int"
    ~description:"A random integer"
    ~value:(Rrd.VT_Int64 (Random.int64 256L)) ~ty:(Rrd.Absolute)
    ~default:true ~units:"things" ()

let generate_random_float_data_source () =
  Rrd.VM "my_vm",
  Ds.ds_make ~name:"random_float"
    ~description:"A random float"
    ~value:(Rrd.VT_Float (Random.float 1.0)) ~ty:(Rrd.Absolute)
    ~default:true ~units:"bits of things" ()

let generate_data_sources () =
  let include_extra_data_sources = get_extra_data_sources_flag () in
  [generate_time_data_source ()] @
  if include_extra_data_sources
  then [
    generate_random_int_data_source ();
    generate_random_float_data_source ();
  ]
  else []

let generate_payload () = {
  timestamp = now ();
  datasources = generate_data_sources ();
}

let main_loop writer interval =
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle (fun _ -> writer.Rrd_writer.cleanup (); exit 0));
  try
    while true do
      writer.Rrd_writer.write_payload (generate_payload ());
      Thread.delay interval
    done
  with e ->
    writer.Rrd_writer.cleanup ();
    raise e

let protocol_of_string = function
  | "v1" -> Rrd_protocol_v1.protocol
  | "v2" -> Rrd_protocol_v2.protocol
  | _ -> failwith "Unknown protocol"

let write_file path protocol =
  Random.self_init ();
  let protocol = protocol_of_string protocol in
  let _, writer =
    Rrd_writer.FileWriter.create
      {Rrd_writer.path = path; shared_page_count = 1}
      protocol
  in
  Printf.printf "Writing to file %s\n%!" path;
  main_loop writer 5.0

let write_page domid protocol =
  Random.self_init ();
  let protocol = protocol_of_string protocol in
  let refs, writer =
    Rrd_writer.PageWriter.create
      {Rrd_writer.backend_domid = domid; shared_page_count = 1}
      protocol
  in
  Printf.printf
    "Writing to pages [%s] shared with domain %d\n%!"
    (List.map string_of_int refs |> String.concat "; ")
    domid;
  main_loop writer 5.0
