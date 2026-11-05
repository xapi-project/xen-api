(*
 * Copyright (C) 2026 Cloud Software Group
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

module D = Debug.Make (struct let name = __MODULE__ end)

let page_size = 65536

(* The helper's own budget for a read is 4 s; the rest is for its start and
   exit *)
let timeout = Mtime.Span.(5 * s)

let run_helper args =
  match
    Helpers.call_script ~log_output:On_failure ~timeout
      !Xapi_globs.gpu_partition_helper
      args
  with
  | stdout ->
      Ok Gpu_partition_probe.{exit= Some 0; stdout}
  | exception Unix.Unix_error ((ENOENT | ENOTDIR), "access", _) ->
      Error Gpu_partition_probe.Helper_absent
  | exception Unix.Unix_error (_, "access", _) ->
      Error Helper_not_executable
  | exception Forkhelpers.Spawn_internal_error (_, stdout, WEXITED n) ->
      Ok {exit= Some n; stdout}
  | exception
      Forkhelpers.Spawn_internal_error (_, stdout, (WSIGNALED _ | WSTOPPED _))
    ->
      Ok {exit= None; stdout}
  | exception Forkhelpers.Subprocess_timeout ->
      Error Caller_timeout

(* Always passed; without a lock file the helper takes no lock *)
let no_lock_args = ["--lock-timeout=0"]

let read_card ~run ~lock_args bdf =
  let request = Gpu_partition_probe.request ~page_size bdf in
  Gpu_partition_probe.decode request
    (run (Gpu_partition_probe.helper_args request @ lock_args))

let cards_bound_to_nvidia ~__context ~host =
  Xapi_pgpu.nvidia_cards ~__context ~host
  |> List.filter (fun (_, pci) -> pci.API.pCI_driver_name = "nvidia")

let read_cards ~__context ~read_card cards =
  let read (self, pci) =
    let bdf = pci.API.pCI_pci_id in
    (* Any exception, even one before the helper runs, costs only this GPU;
       the warning names it *)
    let answer =
      try (read_card bdf).Gpu_partition_probe.probe
      with e ->
        D.log_backtrace e ;
        D.warn "%s: GPU %s: %s" __FUNCTION__ bdf (Printexc.to_string e) ;
        Unreadable {mode= None; reason= Xapi Helper_crashed}
    in
    let axes = Gpu_partition_probe.axes answer in
    Xapi_pgpu.update_partition_mode ~__context ~self axes ;
    D.info "%s: GPU %s: partition_mode %s%s" __FUNCTION__ bdf
      (API.partition_mode_to_string
         (Db.PGPU.get_partition_mode ~__context ~self)
      )
      ( match answer with
      | Unreadable {reason; _} ->
          ", unreadable: " ^ Gpu_partition_probe.string_of_reason reason
      | Partitions _ | None_carved _ | Not_applicable ->
          ""
      )
  in
  List.iter read cards

let read_cards_at_start ~__context ~host ~read_card =
  let cards = cards_bound_to_nvidia ~__context ~host in
  List.iter
    (fun (self, _) -> Xapi_pgpu.update_partition_mode ~__context ~self [None])
    cards ;
  read_cards ~__context ~read_card cards
