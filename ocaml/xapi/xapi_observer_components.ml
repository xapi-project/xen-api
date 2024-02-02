(*
 * Copyright (C) 2023 Cloud Software Group
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

type t = Xapi | Xenopsd | Xapi_clusterd | SMApi [@@deriving ord]

exception Unsupported_Component of string

let all = [Xapi; Xenopsd; Xapi_clusterd; SMApi]

let to_string = function
  | Xapi ->
      "xapi"
  | Xenopsd ->
      "xenopsd"
  | Xapi_clusterd ->
      "xapi-clusterd"
  | SMApi ->
      "smapi"

let of_string = function
  | "xapi" ->
      Xapi
  | "xenopsd" ->
      Xenopsd
  | "xapi-clusterd" ->
      Xapi_clusterd
  | "smapi" ->
      SMApi
  | c ->
      raise (Unsupported_Component c)

(* We start up the observer for clusterd only if clusterd has been enabled
   otherwise we initialise clusterd separately in cluster_host so that
   there is no need to restart xapi in order for clusterd to be observed.
   This does mean that observer will always be enabled for clusterd. *)
let startup_components () =
  List.filter
    (function Xapi_clusterd -> !Xapi_clustering.Daemon.enabled | _ -> true)
    all

let assert_valid_components components =
  try List.iter (fun c -> ignore @@ of_string c) components
  with Unsupported_Component component ->
    raise Api_errors.(Server_error (invalid_value, ["component"; component]))

let observed_components_of components =
  match components with [] -> startup_components () | components -> components

let is_component_enabled ~component =
  try
    Server_helpers.exec_with_new_task
      (Printf.sprintf "check if component %s is enabled " (to_string component))
      (fun __context ->
        try
          let observers = Db.Observer.get_all ~__context in
          List.exists
            (fun observer ->
              Db.Observer.get_enabled ~__context ~self:observer
              && Db.Observer.get_components ~__context ~self:observer
                 |> List.map of_string
                 |> observed_components_of
                 |> List.mem component
            )
            observers
        with _ -> false
      )
  with _ -> false

let is_smapi_enabled () = is_component_enabled ~component:SMApi

let ( // ) = Filename.concat

let dir_name_of_component component =
  Xapi_globs.observer_config_dir // to_string component // "enabled"

let env_vars_of_component ~component ~traceparent =
  let dir_name_value = Filename.quote (dir_name_of_component component) in
  Array.concat
    [
      Forkhelpers.default_path_env_pair
    ; Env_record.to_string_array
        ([Env_record.pair ("OBSERVER_CONFIG_DIR", dir_name_value)]
        @
        match traceparent with
        | None ->
            []
        | Some traceparent ->
            [Env_record.pair ("TRACEPARENT", traceparent)]
        )
    ]
