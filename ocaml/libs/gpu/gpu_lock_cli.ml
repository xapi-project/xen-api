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

(* How long "ensure" waits for older holders to end: the longest legitimate
   chain, a VF setup that waits 30 s for the lock and then enables VFs for
   about 16 s, plus a margin *)
let ensure_timeout = Mtime.Span.(60 * s)

(* The operator's command for the host GPU lock: "status" lists who holds it,
   without locking or creating anything; "ensure" makes it usable where the
   toolstack uses it: a lock object exists and its key is not off. *)
let () =
  let lock = Gpu_lock.create ~engaged:(fun () -> true) () in
  let fail text = prerr_endline text ; exit 1 in
  match Sys.argv with
  | [|_; "status"|] ->
      Gpu_lock.Switch.holders lock
      |> Result.fold ~ok:(List.iter print_endline) ~error:(fun why ->
          fail ("cannot list the GPU lock's holders: " ^ why)
      )
  | [|_; "ensure"|] -> (
    match Gpu_lock.Switch.read_key lock with
    | Ok (None | Some Off) ->
        fail "the GPU lock is not in use on this host"
    | Ok (Some (Pending | On)) | Error _ ->
        Gpu_lock.Switch.ensure lock ~timeout:ensure_timeout
        |> Result.iter_error (fun r -> fail (Gpu_lock.describe r))
  )
  | _ ->
      prerr_endline "usage: gpu-lock-cli status | ensure" ;
      exit 2
