(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

open Stdext.Listext

module D=Debug.Make(struct let name="xapi_pool_license" end)
open D

(* Compare two date options, where None is always greater than (Some _) *)
let compare_dates (a: Stdext.Date.iso8601 option) (b: Stdext.Date.iso8601 option) =
  match a, b with
  | None, None -> 0
  | None, Some _ -> 1
  | Some _, None -> -1
  | Some a', Some b' -> compare a' b'

let get_lowest_edition_with_expiry ~__context ~hosts ~edition_to_int =
  let all_editions_with_expiry =
    List.map
      (fun host ->
         Db.Host.get_edition ~__context ~self:host,
         License_check.get_expiry_date ~__context ~host
      ) hosts
  in
  let pool_edition, _ =
    List.filter_map (fun (edition, _) ->
        if List.mem_assoc edition edition_to_int then
          Some (edition, List.assoc edition edition_to_int)
        else
          None
      ) all_editions_with_expiry
    |> List.sort (fun a b -> compare (snd a) (snd b))
    |> List.hd
  in

  (* Get the earliest expiry date of a list of hosts, given a pool edition.
     	 * Only the expiry dates of the hosts that match the edition are taken into account. *)
  let pool_expiry =
    List.filter_map
      (fun (edition, expiry) -> if edition = pool_edition then Some expiry else None)
      all_editions_with_expiry
    |> List.sort compare_dates
    |> List.hd
  in
  pool_edition, pool_expiry

(* Separate this logic out from Xapi_pool.apply_edition for testing purposes. *)
let apply_edition_with_rollback ~__context ~hosts ~edition ~apply_fn =
  (* Snapshot the current state of the pool in case we need to roll back;
     	 * list the hosts against the edition we're upgrading them *from*. *)
  let pool_license_state =
    List.map
      (fun host -> (host, Db.Host.get_edition ~__context ~self:host))
      hosts
  in
  (* This list will be added to as hosts have the new edition applied. *)
  let to_rollback = ref [] in
  try
    List.iter
      (fun (host, old_edition) ->
         apply_fn ~__context ~host ~edition;
         to_rollback := (host, old_edition) :: !to_rollback)
      pool_license_state
  with e ->
    error
      "Caught %s while trying to upgrade pool to edition %s - attempting rollback"
      (Printexc.to_string e) edition;
    (* Best-effort attempt to roll everything back. *)
    List.iter
      (fun (host, old_edition) ->
         try apply_fn ~__context ~host ~edition:old_edition with _ -> ())
      !to_rollback;
    (* Raise the original exception. *)
    raise e
