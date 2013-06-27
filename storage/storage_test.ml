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

open OUnit

(* Names which are likely to cause problems *)
let names = [
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
let test_name n () = ()

let vdi_name_suite = "vdi_name_suite" >::: (List.map (fun n -> n >:: test_name n) names)

open Cmdliner

let start verbose queue sr = match queue, sr with
  | Some queue, Some sr ->
    let suite = "storage" >:::
      [
        vdi_name_suite;
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

