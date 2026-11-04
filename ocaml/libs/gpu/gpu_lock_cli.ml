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

(* The operator's command for the host GPU lock: "status" lists who holds it
   and creates nothing; "ensure" makes it usable where the toolstack uses it:
   a lock object exists and its key is not off. "ensure" reads the key once,
   before it waits for the holders. *)

(* How long "ensure" waits for older holders to end: the longest chain
   measured, a VF setup that waits 30 s for the lock and then enables VFs for
   about 16 s, plus a margin *)
let ensure_timeout = Mtime.Span.(60 * s)

let () =
  Debug.set_facility Syslog.Local5 ;
  (* the calls below work whether or not the lock is engaged *)
  let lock = Gpu_lock.create ~engaged:(fun () -> true) () in
  let fail text = prerr_endline text ; exit 1 in
  let not_in_use = "the GPU lock is not in use on this host" in
  match Sys.argv with
  | [|_; "status"|] ->
      Gpu_lock.Gate.holders lock
      |> Result.fold ~ok:(List.iter print_endline) ~error:(fun why ->
          match Gpu_lock.Gate.read_key lock ~timeout:Mtime.Span.zero with
          | Ok None ->
              prerr_endline not_in_use
          | Ok (Some (Pending | On | Off)) | Error _ ->
              fail ("cannot list the GPU lock's holders: " ^ why)
      )
  | [|_; "ensure"|] -> (
    match Gpu_lock.Gate.read_key lock ~timeout:Mtime.Span.zero with
    | Ok None ->
        fail not_in_use
    | Ok (Some Off) ->
        fail "the GPU lock's key is off on this host"
    (* An Error: held or unusable; ensure waits for the holders, or says why *)
    | Ok (Some (Pending | On)) | Error _ ->
        Gpu_lock.Gate.ensure lock ~timeout:ensure_timeout
        |> Result.iter_error (fun r -> fail (Gpu_lock.describe r))
  )
  | _ ->
      prerr_endline "usage: gpu-lock-cli status | ensure" ;
      exit 2
