(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(** Module that defines API functions for Secret objects
 * @group XenAPI functions
*)

module D = Debug.Make (struct let name = "xapi_secret" end)

open D

let introduce ~__context ~uuid ~value ~other_config =
  let ref = Ref.make () in
  Db.Secret.create ~__context ~ref ~uuid ~value ~other_config ;
  ref

let create ~__context ~value ~other_config =
  let uuid = Uuidx.to_string (Uuidx.make ()) in
  let ref = introduce ~__context ~uuid ~value ~other_config in
  ref

let destroy ~__context ~self = Db.Secret.destroy ~__context ~self

(* Delete the passwords references in a string2string map *)
let clean_out_passwds ~__context strmap =
  let delete_secret uuid =
    try
      let s = Db.Secret.get_by_uuid ~__context ~uuid in
      Db.Secret.destroy ~__context ~self:s
    with _ -> ()
  in
  let check_key (k, _) = String.ends_with ~suffix:"password_secret" k in
  let secrets = List.map snd (List.filter check_key strmap) in
  List.iter delete_secret secrets

let copy ~__context ~secret =
  let uuid = Uuidx.(to_string (make ())) in
  let value = Db.Secret.get_value ~__context ~self:secret in
  let other_config = Db.Secret.get_other_config ~__context ~self:secret in
  let ref = introduce ~__context ~uuid ~value ~other_config in
  ref

(* Modify a ((string * string) list) by duplicating all the passwords found in
 * it *)
let duplicate_passwds ~__context strmap =
  let check_key k = String.ends_with ~suffix:"password_secret" k in
  let possibly_duplicate (k, v) =
    if check_key k then
      let sr = Db.Secret.get_by_uuid ~__context ~uuid:v in
      let v = Db.Secret.get_value ~__context ~self:sr in
      let new_sr = create ~__context ~value:v ~other_config:[] in
      let new_uuid = Db.Secret.get_uuid ~__context ~self:new_sr in
      (k, new_uuid)
    else
      (k, v)
  in
  List.map possibly_duplicate strmap

let move_passwds_to_secrets ~__context strmap =
  let maybe_move (k, value) =
    if String.ends_with ~suffix:"password" k then (
      let new_k = k ^ "_secret" in
      warn
        "Replacing deprecated %s with %s, please avoid using %s in \
         device-config!"
        k new_k k ;
      let new_sr = create ~__context ~value ~other_config:[] in
      let new_uuid = Db.Secret.get_uuid ~__context ~self:new_sr in
      (new_k, new_uuid)
    ) else
      (k, value)
  in
  List.rev_map maybe_move strmap
