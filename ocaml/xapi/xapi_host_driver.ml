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
module Tool = Xapi_host_driver_tool

let invalid_value field value =
  raise Api_errors.(Server_error (invalid_value, [field; value]))

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
    debug "%s: destroying driver variant %s" __FUNCTION__ (Ref.string_of self) ;
    Db.Driver_variant.destroy ~__context ~self

  (** create' is like create but updates an exisiting entry if it
      exists. This avoids entries becoming stale *)
  let create' ~__context ~name ~version ~driver ~hw_present ~priority
      ~dev_status =
    let open Xapi_database.Db_filter_types in
    (* driver and name identify a variant uniquely *)
    let driver' = Eq (Field "driver", Literal (Ref.string_of driver)) in
    let name' = Eq (Field "name", Literal name) in
    let expr = And (driver', name') in
    match Db.Driver_variant.get_refs_where ~__context ~expr with
    | [] ->
        create ~__context ~name ~version ~driver ~hw_present ~priority
          ~dev_status
    | [self] ->
        debug "%s: updating existing entry for variant %s" __FUNCTION__ name ;
        Db.Driver_variant.set_version ~__context ~self ~value:version ;
        Db.Driver_variant.set_priority ~__context ~self ~value:priority ;
        Db.Driver_variant.set_status ~__context ~self ~value:dev_status ;
        Db.Driver_variant.set_hardware_present ~__context ~self
          ~value:hw_present ;
        self
    | variants ->
        warn "%s: multiple entries for %s found; recreating one" __FUNCTION__
          name ;
        variants |> List.iter (fun self -> destroy ~__context ~self) ;
        create ~__context ~name ~version ~driver ~hw_present ~priority
          ~dev_status

  let select ~__context ~self =
    debug "%s: %s" __FUNCTION__ (Ref.string_of self) ;
    let drv = Db.Driver_variant.get_driver ~__context ~self in
    let d = Db.Host_driver.get_record ~__context ~self:drv in
    let v = Db.Driver_variant.get_record ~__context ~self in
    let stdout =
      Tool.call ["select"; d.API.host_driver_name; v.API.driver_variant_name]
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

(** create' is like create except it checks if an entry exists and
    modifies it. This avoids ref/UUIDs becoming stale *)
let create' ~__context ~host ~name ~friendly_name ~_type ~description ~info:inf
    ~active_variant ~selected_variant =
  let null = Ref.null in
  let open Xapi_database.Db_filter_types in
  let host' = Eq (Field "host", Literal (Ref.string_of host)) in
  let name' = Eq (Field "name", Literal name) in
  let expr = And (host', name') in
  match Db.Host_driver.get_refs_where ~__context ~expr with
  | [] ->
      (* no such entry exists - create it *)
      create ~__context ~host ~name ~friendly_name ~info:inf ~active_variant
        ~selected_variant ~description ~_type
  | [self] ->
      (* one existing entry - update it *)
      info "%s: updating host driver %s" __FUNCTION__ name ;
      Db.Host_driver.set_friendly_name ~__context ~self ~value:name ;
      Db.Host_driver.set_info ~__context ~self ~value:inf ;
      Db.Host_driver.set_active_variant ~__context ~self ~value:active_variant ;
      Db.Host_driver.set_selected_variant ~__context ~self
        ~value:selected_variant ;
      Db.Host_driver.set_description ~__context ~self ~value:description ;
      Db.Host_driver.set_type ~__context ~self ~value:_type ;
      self
  | drivers ->
      warn "%s: more than one entry for driver %s; destroying them" __FUNCTION__
        name ;
      drivers |> List.iter (fun self -> destroy ~__context ~self) ;
      create ~__context ~host ~name ~friendly_name ~info:inf
        ~active_variant:null ~selected_variant:null ~description ~_type

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
        Tool.call ["select"; d.API.host_driver_name; v.API.driver_variant_name]
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
  let stdout = Tool.call ["deselect"; d.API.host_driver_name] in
  info "%s: %s" __FUNCTION__ stdout ;
  Db.Host_driver.set_active_variant ~__context ~self ~value:Ref.null ;
  Db.Host_driver.set_selected_variant ~__context ~self ~value:Ref.null

(** remove all host driver entries  that are not in [except]. We exepect
    any list to be short *)
let remove ~__context ~host ~except =
  D.debug "%s" __FUNCTION__ ;
  let open Xapi_database.Db_filter_types in
  let expr = Eq (Field "host", Literal (Ref.string_of host)) in
  Db.Host_driver.get_refs_where ~__context ~expr
  |> List.filter (fun driver -> not @@ List.mem driver except)
  |> List.iter (fun self -> destroy ~__context ~self)

(** Runs on [host]. We update or create an entry for each driver
    reported by drivertool and remove any extra driver that is in xapi. *)
let scan ~__context ~host =
  Tool.Mock.install () ;
  let null = Ref.null in
  let drivers (* on this host *) =
    Tool.call ["list"]
    |> Tool.parse
    |> List.map @@ fun (_name, driver) ->
       let driver_ref =
         create' ~__context ~host ~name:driver.Tool.name
           ~friendly_name:driver.Tool.name ~info:driver.Tool.info
           ~active_variant:null ~selected_variant:null
           ~description:driver.Tool.descr ~_type:driver.Tool.ty
       in
       (driver.Tool.variants
       |> List.iter @@ fun (name, v) ->
          let var_ref =
            Variant.create' ~__context ~name ~version:v.Tool.version
              ~driver:driver_ref ~hw_present:v.Tool.hw_present
              ~priority:v.Tool.priority ~dev_status:v.Tool.dev_status
          in
          ( match driver.Tool.selected with
          | Some v when v = name ->
              Db.Host_driver.set_selected_variant ~__context ~self:driver_ref
                ~value:var_ref
          | _ ->
              ()
          ) ;
          match driver.Tool.active with
          | Some v when v = name ->
              Db.Host_driver.set_active_variant ~__context ~self:driver_ref
                ~value:var_ref
          | _ ->
              ()
       ) ;
       driver_ref
  in
  remove ~__context ~host ~except:drivers

(** Runs on [host] *)
let rescan ~__context ~host = debug "%s" __FUNCTION__ ; scan ~__context ~host
