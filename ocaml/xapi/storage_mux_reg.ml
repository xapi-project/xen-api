(*
 * Copyright (c) Cloud Software Group
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

(** This module contains the code for registering storage plugins (SMAPIv1 and SMAPIv3)
and multiplexing between them according to the sr type *)

module D = Debug.Make (struct let name = __MODULE__ end)

open D

type processor = Rpc.call -> Rpc.response

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

open Storage_interface

let s_of_sr = Storage_interface.Sr.string_of

type plugin = {
    processor: processor
  ; backend_domain: string
  ; query_result: query_result
  ; features: Smint.Feature.t list
}

let plugins : (sr, plugin) Hashtbl.t = Hashtbl.create 10

let m = Mutex.create ()

let debug_printer rpc call =
  (* debug "Rpc.call = %s" (Xmlrpc.string_of_call call); *)
  let result = rpc call in
  (* debug "Rpc.response = %s" (Xmlrpc.string_of_response result); *)
  result

let register sr rpc d info =
  with_lock m (fun () ->
      let features =
        Smint.Feature.parse_capability_int64 info.Storage_interface.features
      in
      Hashtbl.replace plugins sr
        {
          processor= debug_printer rpc
        ; backend_domain= d
        ; query_result= info
        ; features
        } ;
      debug "register SR %s (currently-registered = [ %s ])" (s_of_sr sr)
        (String.concat ", "
           (Hashtbl.fold (fun sr _ acc -> s_of_sr sr :: acc) plugins [])
        )
  )

let unregister sr =
  with_lock m (fun () ->
      Hashtbl.remove plugins sr ;
      debug "unregister SR %s (currently-registered = [ %s ])" (s_of_sr sr)
        (String.concat ", "
           (Hashtbl.fold (fun sr _ acc -> s_of_sr sr :: acc) plugins [])
        )
  )

(* This function is entirely unused, but I am not sure if it should be
   deleted or not *)
let query_result_of_sr sr =
  with_lock m (fun () ->
      Option.map (fun x -> x.query_result) (Hashtbl.find_opt plugins sr)
  )

let sr_has_capability sr capability =
  with_lock m (fun () ->
      match Hashtbl.find_opt plugins sr with
      | Some x ->
          Smint.Feature.has_capability capability x.features
      | None ->
          false
  )

(* This is the policy: *)
let of_sr sr =
  with_lock m (fun () ->
      match Hashtbl.find_opt plugins sr with
      | Some x ->
          x.processor
      | None ->
          error "No storage plugin for SR: %s (currently-registered = [ %s ])"
            (s_of_sr sr)
            (String.concat ", "
               (Hashtbl.fold (fun sr _ acc -> s_of_sr sr :: acc) plugins [])
            ) ;
          raise (Storage_error (No_storage_plugin_for_sr (s_of_sr sr)))
  )

let smapi_version_of_sr sr =
  with_lock m (fun () ->
      match Hashtbl.find_opt plugins sr with
      | Some x ->
          x.query_result.smapi_version
      | None ->
          error "No storage plugin for SR: %s (currently-registered = [ %s ])"
            (s_of_sr sr)
            (String.concat ", "
               (Hashtbl.fold (fun sr _ acc -> s_of_sr sr :: acc) plugins [])
            ) ;
          raise (Storage_error (No_storage_plugin_for_sr (s_of_sr sr)))
  )

type 'a sm_result = SMSuccess of 'a | SMFailure of exn

let string_of_sm_result f = function
  | SMSuccess x ->
      Printf.sprintf "Success: %s" (f x)
  | SMFailure e ->
      Printf.sprintf "Failure: %s" (Printexc.to_string e)

let success = function SMSuccess _ -> true | _ -> false

let multicast f =
  Hashtbl.fold
    (fun sr plugin acc ->
      (sr, try SMSuccess (f sr plugin.processor) with e -> SMFailure e) :: acc
    )
    plugins []
