(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

(* Check the VDI.copy support for copying disk deltas *)

open Sparse_encoding
open Client
open Quicktest_common


let write_to_vdi ~session_id ~vdi f =
  let task_id = Client.Task.create ~rpc:!rpc ~session_id
      ~label:"quicktest importing VDI contents"
      ~description:"" in
  (*let uri = Printf.sprintf "/import_raw_vdi?session_id=%s&vdi=%s&task_id=%si&chunked=true"
    (Ref.string_of session_id) (Ref.string_of vdi) (Ref.string_of task_id) in *)
  let req = Http.Request.make ~version:"1.0"
      ~user_agent:"quicktest"
      ~query:[
        "session_id", Ref.string_of session_id;
        "vdi", Ref.string_of vdi;
        "task_id", Ref.string_of task_id;
        "chunked", "true"
      ]
      Http.Put "/import_raw_vdi" in
  http req (fun (_, fd) -> f fd);
  while Client.Task.get_status ~rpc:!rpc ~session_id ~self:task_id = `pending do
    Unix.sleep 1
  done;
  let t = Client.Task.get_record ~rpc:!rpc ~session_id ~self:task_id in
  Client.Task.destroy ~rpc:!rpc ~session_id ~self:task_id;
  if t.API.task_status <> `success
  then raise (Api_errors.Server_error(List.hd t.API.task_error_info, List.tl t.API.task_error_info))

let read_from_vdi ~session_id ~vdi f =
  let uri = Printf.sprintf "/export_raw_vdi?session_id=%s&vdi=%s"
      (Ref.string_of session_id) (Ref.string_of vdi) in
  let req = Http.Request.make ~version:"1.0"
      ~user_agent:"quicktest"
      Http.Get uri in
  http req (fun (_, fd) -> f fd)

(* Simpler alternative to Xapi_vdi.wait_for_vbds_to_be_unplugged_and_destroyed,
   using sleep instead of the event system. *)
let wait_for_no_vbds_then_destroy ~rpc ~session_id self =
  let wait_for_no_vbds () =
    let start = Mtime_clock.counter () in
    let over () = (Mtime_clock.count start |> Mtime.Span.to_s) > 4.0 in
    while not (over ()) && Client.VDI.get_VBDs ~rpc ~session_id ~self <> [] do Unix.sleepf 0.1 done
  in
  wait_for_no_vbds ();
  Client.VDI.destroy ~rpc ~session_id ~self

let start session_id sr =
  let t = make_test "Check VDI.copy delta handling" 1 in
  start t;

  (* Create a 4 MiB disk on src_sr *)
  let original =
    Client.VDI.create ~rpc:!rpc ~session_id ~name_label:"quicktest original"
      ~name_description:"Used by the VDI.copy test"
      ~sR:sr ~virtual_size:Int64.(mul (mul 4L 1024L) 1024L)
      ~_type:`user ~sharable:false ~read_only:false
      ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[] in

  debug t "Created a 4MiB test disk";

  (* Upload 1 MiB of 'a' characters: on a .vhd this will consume
     half of a 2 MiB block *)
  write_to_vdi ~session_id ~vdi:original
    (fun fd ->
       let data = String.make (1024 * 1024) 'a' in
       let chunk = { Chunk.start = 0L; data } in
       Chunk.marshal fd chunk;
       let final = { Chunk.start = 0L; data = "" } in
       Chunk.marshal fd final;
    );

  debug t "Uploaded 1MiB of 'a's";

  (* Snapshot the disk *)
  let snapshot =
    Client.VDI.snapshot ~rpc:!rpc ~session_id ~vdi:original ~driver_params:[] in

  debug t "Snapshotted VDI";

  (* Upload a single sector of 'b' characters: on a .vhd this will
     be represented as a block with an almost-empty bitmap. *)
  write_to_vdi ~session_id ~vdi:original
    (fun fd ->
       let data = String.make 512 'b' in
       let chunk = { Chunk.start = 0L; data } in
       Chunk.marshal fd chunk;
       let final = { Chunk.start = 0L; data = "" } in
       Chunk.marshal fd final;
    );

  debug t "Uploaded 1 sector of 'b's";

  (* Back up the original snapshot *)

  let snapshot_backup =
    Client.VDI.copy ~rpc:!rpc ~session_id ~vdi:snapshot ~sr
      ~base_vdi:Ref.null ~into_vdi:Ref.null in

  debug t "Created backup of snapshot full image";

  (* Back up the differences between the snapshot and the current *)
  let delta_backup =
    Client.VDI.copy ~rpc:!rpc ~session_id ~vdi:original ~sr
      ~base_vdi:snapshot ~into_vdi:Ref.null in

  debug t "Created backup of deltas";

  (* Consolidate the differences into the snapshot backup *)
  let (_: API.ref_VDI) =
    Client.VDI.copy ~rpc:!rpc ~session_id ~vdi:delta_backup ~sr:Ref.null
      ~base_vdi:Ref.null ~into_vdi:snapshot_backup in

  debug t "Consolidated deltas into full image";

  (* Download the consolidated image, check the contents match what
     we've written to the original *)
  read_from_vdi ~session_id ~vdi:snapshot_backup
    (fun fd ->
       let a = Stdext.Unixext.really_read_string fd 512 in
       for i = 0 to String.length a - 1 do
         if a.[i] <> 'b' then begin
           let msg = Printf.sprintf "VDI offset %d has %c: expected %c" i a.[i] 'b' in
           failed t msg;
           failwith msg;
         end
       done;
       debug t "First sector is full of 'b's";
       let b = Stdext.Unixext.really_read_string fd (1024 * 1024 - 512) in
       for i = 0 to String.length b - 1 do
         if b.[i] <> 'a' then begin
           let msg = Printf.sprintf "VDI offset %d has %c: expected %c" i b.[i] 'a' in
           failed t msg;
           failwith msg
         end;
       done;
       debug t "1MiB - 1 sector is full of 'a's";
    );

  debug t "Destroying VDI (cleanup)";
  List.iter (wait_for_no_vbds_then_destroy ~rpc:!rpc ~session_id) [ original; snapshot; snapshot_backup; delta_backup ];

  success t

