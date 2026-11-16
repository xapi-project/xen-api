(*
 * Copyright (C) Cloud Software Group
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

type t = Performance | Functional | Audit | Boundary [@@deriving ord]

let all = [Performance; Functional; Audit; Boundary]

let to_string = function
  | Performance ->
      Constants.observer_purpose_performance
  | Functional ->
      Constants.observer_purpose_functional
  | Audit ->
      Constants.observer_purpose_audit
  | Boundary ->
      Constants.observer_purpose_boundary

let of_string s = List.find_opt (fun p -> String.equal (to_string p) s) all

let assert_valid_purpose purposes =
  match List.find_opt (fun p -> Option.is_none (of_string p)) purposes with
  | Some p ->
      raise Api_errors.(Server_error (invalid_value, ["purpose"; p]))
  | None ->
      ()

(* Deliberately NOT modelled on Xapi_observer_components.is_component_enabled,
   which opens a task and reads every Observer record on each call, and turns
   any exception into [false]. This is a pure function over records the
   caller has already fetched: it cannot touch the database and cannot fail. *)
let is_enabled ~observers ~purpose =
  let name = to_string purpose in
  List.exists
    (fun (enabled, purposes) ->
      enabled && (purposes = [] || List.mem name purposes)
    )
    observers

let observers_of_records records =
  List.map (fun (_, r) -> API.(r.observer_enabled, r.observer_purpose)) records
