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

open Storage_interface
open Storage_client

(* Principles:
   1. we don't delete or manipulate VDIs we didn't create
   2. we create VDIs with non-clashing names
   3. we always clean up (as best we can) after every test.
*)

(* We assume that no-one else has made VDIs with this name prefix: *)
let safe_prefix = Printf.sprintf "storage_test.%d" (Unix.getpid ())
let dbg = safe_prefix

open OUnit

(* Names which are likely to cause problems *)
let names = [
  "simple"; (* start with an easy one *)
  "";
  ".";
  "..";
  "/";
  "!";
  String.make 128 '0';
]

(* For each VDI we check that:
   1. it shows up in a SR.scan
   2. attach RO, activate, deactivate, detach works
   3. attach RW, activate, deactivate, detach works
*)
let test_name sr n () =
  let vdi_info = {
    default_vdi_info with
      name_label = safe_prefix ^ "." ^ n;
      virtual_size = 1000000000L;
  } in
  let name_exists name =
    let all = Client.SR.scan ~dbg ~sr in
    List.fold_left (fun acc vdi_info -> acc || (vdi_info.name_label = name)) false all in
  assert(not (name_exists vdi_info.name_label));
  let vdi = Client.VDI.create ~dbg ~sr ~vdi_info in
  assert(name_exists vdi_info.name_label);
  (* Check the disk has size >= the amount we requested *)
  assert(vdi.virtual_size >= vdi_info.virtual_size);

  List.iter
    (fun read_write ->
      let _ = Client.VDI.attach ~dbg ~dp:dbg ~sr ~vdi:vdi.vdi ~read_write in
      Client.VDI.activate ~dbg ~dp:dbg ~sr ~vdi:vdi.vdi;
      Client.VDI.deactivate ~dbg ~dp:dbg ~sr ~vdi:vdi.vdi;
      Client.VDI.detach ~dbg ~dp:dbg ~sr ~vdi:vdi.vdi;
    ) [ true; false ];

  Client.VDI.destroy ~dbg ~sr ~vdi:vdi.vdi;
  assert(not (name_exists vdi_info.name_label))

let vdi_name_suite sr = "vdi_name_suite" >::: (List.map (fun n -> n >:: test_name sr n) names)

open Cmdliner

let start verbose queue sr = match queue, sr with
  | Some queue, Some sr ->
    Storage_interface.queue_name := queue;
    Xcp_client.use_switch := true;

    let suite = "storage" >:::
      [
        vdi_name_suite sr;
      ] in
    let (_: test_result list) = run_test_tt ~verbose suite in
    ()
  | _, _ ->
    Printf.fprintf stderr "Please supply both a queue name and an SR\n%!";
    ()

let cmd =
  let doc = "Storage component test" in
  let man = [
    `S "DESCRIPTION";
    `P "Test a storage implementation via the SMAPI.";
    `S "USAGE";
    `P "$(tname) <queue name> <sr name>";
    `P "-- test the service listening on <queue name> using the existing attached SR <sr name>.";
  ] in
  let verbose =
    let doc = "Print verbose output" in
    Arg.(value & flag & info ["verbose"; "v"] ~doc) in
  let queue =
    let doc = "The queue name where the storage implementation is listening." in
    Arg.(value & pos 0 (some string) None & info [] ~doc) in
  let sr =
    let doc = "The attached SR." in
    Arg.(value & pos 1 (some string) None & info [] ~doc) in

  Term.(pure start $ verbose $ queue $ sr),
  Term.info "test" ~doc ~man

let () = match Term.eval cmd with
 | `Error _ -> exit 1
 | _ -> exit 0

