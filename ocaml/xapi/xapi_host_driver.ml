(*
   Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
 *)

module D = Debug.Make (struct let name = __MODULE__ end)

open D
module Unixext = Xapi_stdext_unix.Unixext

(*
module DriverMap = Map.Make (String)
module DriverSet = Set.Make (String)
*)
module T = Xapi_host_driver_tool

let invalid_value field value =
  raise Api_errors.(Server_error (invalid_value, [field; value]))

let internal_error fmt =
  Printf.ksprintf
    (fun msg ->
      error "%s" msg ;
      raise Api_errors.(Server_error (internal_error, [msg]))
    )
    fmt

let drivertool args =
  let path = !Xapi_globs.driver_tool in
  try
    let stdout, _stderr = Forkhelpers.execute_command_get_output path args in
    debug "%s: executed %s %s" __FUNCTION__ path (String.concat " " args) ;
    stdout
  with e ->
    internal_error "%s: failed to run %s %s: %s" __FUNCTION__ path
      (String.concat " " args) (Printexc.to_string e)

module Variant = struct
  let create ~__context ~name ~version ~driver ~hw_present ~priority ~dev_status
      =
    info "%s: %s" __FUNCTION__ name ;
    let ref = Ref.make () in
    let uuid = Uuidx.to_string (Uuidx.make ()) in
    Db.Driver_variant.create ~__context ~ref ~driver ~uuid ~name ~version
      ~hardware_present:hw_present ~priority ~status:dev_status ;
    ref

  let destroy ~__context ~self =
    debug "Destroying driver variant %s" (Ref.string_of self) ;
    Db.Driver_variant.destroy ~__context ~self

  let select ~__context ~self =
    debug "%s: %s" __FUNCTION__ (Ref.string_of self) ;
    let drv = Db.Driver_variant.get_driver ~__context ~self in
    let d = Db.Host_driver.get_record ~__context ~self:drv in
    let v = Db.Driver_variant.get_record ~__context ~self in
    let stdout =
      drivertool ["select"; d.API.host_driver_name; v.API.driver_variant_name]
    in
    info "%s: %s" __FUNCTION__ stdout ;
    Db.Host_driver.set_selected_variant ~__context ~self:drv ~value:self
end

let create ~__context ~host ~name ~friendly_name ~_type ~description ~info:inf
    ~active_variant ~selected_variant =
  D.info "%s: %s" __FUNCTION__ name ;
  let ref = Ref.make () in
  let uuid = Uuidx.to_string (Uuidx.make ()) in
  Db.Host_driver.create ~__context ~ref ~uuid ~host ~name ~friendly_name ~_type
    ~description ~info:inf ~active_variant ~selected_variant ;
  ref

(** destroy driver and recursively its variants, too *)
let destroy ~__context ~self =
  D.debug "Destroying driver %s" (Ref.string_of self) ;
  let variants = Db.Host_driver.get_variants ~__context ~self in
  variants |> List.iter (fun self -> Variant.destroy ~__context ~self) ;
  Db.Host_driver.destroy ~__context ~self

(** Runs on the host where the driver is installed *)
let select ~__context ~self ~variant =
  let drv = Ref.string_of self in
  let var = Ref.string_of variant in
  let variants = Db.Host_driver.get_variants ~__context ~self in
  match List.mem variant variants with
  | true ->
      D.debug "%s selecting driver %s variant %s" __FUNCTION__ drv var ;
      let d = Db.Host_driver.get_record ~__context ~self in
      let v = Db.Driver_variant.get_record ~__context ~self:variant in
      let stdout =
        drivertool ["select"; d.API.host_driver_name; v.API.driver_variant_name]
      in
      info "%s: %s" __FUNCTION__ stdout ;
      Db.Host_driver.set_selected_variant ~__context ~self ~value:variant
  | false ->
      error "%s variant %s does not belong to driver %s" __FUNCTION__ var drv ;
      invalid_value "variant" var

(** Runs on the host where the driver is installed *)
let deselect ~__context ~self =
  D.debug "%s  driver %s" __FUNCTION__ (Ref.string_of self) ;
  let d = Db.Host_driver.get_record ~__context ~self in
  let stdout = drivertool ["deselect"; d.API.host_driver_name] in
  info "%s: %s" __FUNCTION__ stdout ;
  Db.Host_driver.set_active_variant ~__context ~self ~value:Ref.null ;
  Db.Host_driver.set_selected_variant ~__context ~self ~value:Ref.null

(** remove all host driver entries for this host *)
let reset ~__context ~host =
  D.debug "%s" __FUNCTION__ ;
  let open Xapi_database.Db_filter_types in
  let expr = Eq (Field "host", Literal (Ref.string_of host)) in
  let drivers = Db.Host_driver.get_refs_where ~__context ~expr in
  drivers |> List.iter (fun self -> destroy ~__context ~self)

(** Runs on [host] *)
let scan ~__context ~host =
  T.Mock.install () ;
  let null = Ref.null in
  reset ~__context ~host ;
  drivertool ["list"]
  |> T.parse
  |> List.iter @@ fun (_name, driver) ->
     let driver_ref =
       create ~__context ~host ~name:driver.T.name ~friendly_name:driver.T.name
         ~info:driver.T.info ~active_variant:null ~selected_variant:null
         ~description:driver.T.descr ~_type:driver.T.ty
     in
     driver.T.variants
     |> List.iter @@ fun (name, v) ->
        let var_ref =
          Variant.create ~__context ~name ~version:v.T.version
            ~driver:driver_ref ~hw_present:v.T.hw_present ~priority:v.T.priority
            ~dev_status:v.T.dev_status
        in
        ( match driver.T.selected with
        | Some v when v = name ->
            Db.Host_driver.set_selected_variant ~__context ~self:driver_ref
              ~value:var_ref
        | _ ->
            ()
        ) ;
        match driver.T.active with
        | Some v when v = name ->
            Db.Host_driver.set_active_variant ~__context ~self:driver_ref
              ~value:var_ref
        | _ ->
            ()

(** Runs on [host] *)
let rescan ~__context ~host = debug "%s" __FUNCTION__ ; scan ~__context ~host
