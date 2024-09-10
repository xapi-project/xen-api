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
module DriverMap = Map.Make (String)
module DriverSet = Set.Make (String)

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

let selection_mutex = Mutex.create ()

let ( // ) = Filename.concat

let invalid_value field value =
  raise Api_errors.(Server_error (invalid_value, [field; value]))

let internal_error fmt =
  Printf.ksprintf
    (fun msg ->
      error "%s" msg ;
      raise Api_errors.(Server_error (internal_error, [msg]))
    )
    fmt

let create ~__context ~host ~name ~versions ~active_version ~selected_version =
  info "%s: %s" __FUNCTION__ name ;
  let ref = Ref.make () in
  let uuid = Uuidx.to_string (Uuidx.make ()) in
  Db.Host_driver.create ~__context ~ref ~uuid ~host ~name ~versions
    ~active_version ~selected_version

let destroy ~__context ~self =
  D.debug "Destroying driver %s" (Ref.string_of self) ;
  Db.Host_driver.destroy ~__context ~self

(*
* `/lib/modules/<kernel-version>/updates/` is searched by the kernel for
  drivers - this is `selected_dir`.
* The hierarchy is expected to contain symbolic links to the file
  actually containing the driver in
  `/lib/modules/<kernel-version>/xenserver/<driver>/<version>/<name>.ko` -
  the base directory here is `all_versions_dir`. *)
type driver_paths = {selected_dir: string; all_versions_dir: string}

let get_drivers_base_path ~__context ~host =
  let linux_version =
    List.assoc "linux" (Db.Host.get_software_version ~__context ~self:host)
  in
  let base_dir = "/lib/modules" // linux_version in
  {
    selected_dir= base_dir // "updates"
  ; all_versions_dir= base_dir // "xenserver"
  }

(* To prevent potential conflicts in generating dependencies and initrd, allow
   only one reload_driver_selection to run per host *)
let reload_driver_selection ~__context ~host ~select =
  with_lock selection_mutex (fun () ->
      let linux_version =
        List.assoc "linux" (Db.Host.get_software_version ~__context ~self:host)
      in
      try
        let _, _ =
          Forkhelpers.execute_command_get_output !Xapi_globs.depmod
            [
              "-ae"
            ; "-F"
            ; Format.sprintf "/boot/System.map-%s" linux_version
            ; linux_version
            ]
        in
        let _, _ =
          Forkhelpers.execute_command_get_output !Xapi_globs.dracut
            [
              "-f"
            ; Format.sprintf "/boot/initrd-%s.img" linux_version
            ; linux_version
            ]
        in
        (* udev event only needs to be triggered and waited for on selection, since
           it can't unload a driver *)
        if select then
          let _, _ =
            Forkhelpers.execute_command_get_output !Xapi_globs.udevadm
              ["trigger"; "--attr-nomatch=driver"]
          in
          let _, _ =
            Forkhelpers.execute_command_get_output !Xapi_globs.udevadm
              ["settle"; "-t"; "30"]
          in
          ()
      with Forkhelpers.Spawn_internal_error (err, _, _) ->
        internal_error "Failed to reload driver selection (%s): %s" __FUNCTION__
          err
  )

(* Versions are compared in segments separated by punctuation characters.
   If both segments are integers, integer comparison is used, if at least
   one of the segments is not an integer, lexicographic comparison is used.
   Any segment compares larger against an empty segment.
   Thus, "1.2.3" > "1.2";
         "1.2.3" > "1.2."
         "1.2.3" < "1.2.fix"
         "1.2.3" < "1.2.3fix"
*)
module Version = struct
  let compare a b =
    let split str = Str.(split_delim (regexp "[.,:;+_-]") str) in

    List.compare
      (fun a b ->
        Stdlib.(
          match (int_of_string_opt a, int_of_string_opt b) with
          | Some a, Some b ->
              Int.compare a b
          | _, _ ->
              String.compare a b
        )
      )
      (split a) (split b)
end

let get_driver_versions driver_name selected_dir all_versions_dir =
  let selected_version =
    try
      let driver_realpath =
        Unix.readlink (selected_dir // (driver_name ^ ".ko"))
      in
      Filename.(basename (dirname driver_realpath))
    with _ ->
      D.debug "Driver '%s' has no selected version" driver_name ;
      ""
  in
  let active_version =
    try
      Result.value ~default:""
        (Xapi_host_driver_helpers.get_version
           ("/sys/module" // driver_name // "notes/note.xenserver")
        )
    with _ ->
      D.debug "Driver '%s' has no active version" driver_name ;
      ""
  in
  let driver_versions =
    let arr = Sys.readdir (all_versions_dir // driver_name) in
    Array.sort Version.compare arr ;
    Array.to_list arr
  in
  (selected_version, active_version, driver_versions)

let update_drivers_db ~__context ~host ~driver_paths db_drivers
    filesystem_drivers =
  try
    let db_drivers_set, db_drivers_map =
      db_drivers
      |> List.fold_left
           (fun (set, map) (ref, driver) ->
             ( DriverSet.add driver.API.host_driver_name set
             , DriverMap.add driver.API.host_driver_name (ref, driver) map
             )
           )
           (DriverSet.empty, DriverMap.empty)
    in
    let filesystem_drivers =
      Array.fold_left
        (fun set name -> DriverSet.add name set)
        DriverSet.empty filesystem_drivers
    in
    (* Modify drivers that were in the DB before and are still on the
       filesystem in-place to keep references from outside intact *)
    let updated_drivers =
      filesystem_drivers
      |> DriverSet.elements
      |> List.filter_map (fun driver_name ->
             let record = DriverMap.find_opt driver_name db_drivers_map in
             Option.bind record (fun (ref, record) ->
                 let selected_version, active_version, driver_versions =
                   get_driver_versions driver_name driver_paths.selected_dir
                     driver_paths.all_versions_dir
                 in
                 let selected_changed =
                   record.host_driver_selected_version <> selected_version
                 in
                 let active_changed =
                   record.host_driver_active_version <> active_version
                 in
                 let versions_changed =
                   record.host_driver_versions <> driver_versions
                 in
                 if selected_changed then
                   Db.Host_driver.set_selected_version ~__context ~self:ref
                     ~value:selected_version ;
                 if active_changed then
                   Db.Host_driver.set_active_version ~__context ~self:ref
                     ~value:active_version ;
                 if versions_changed then
                   Db.Host_driver.set_versions ~__context ~self:ref
                     ~value:driver_versions ;
                 if selected_changed || active_changed || versions_changed then
                   Some driver_name
                 else
                   None
             )
         )
    in
    (* Remove drivers that are no longer in the filesystem from the DB *)
    let removed_drivers =
      DriverSet.diff db_drivers_set filesystem_drivers
      |> DriverSet.elements
      |> List.map (fun driver_name ->
             let self, _ = DriverMap.find driver_name db_drivers_map in
             destroy ~__context ~self ; driver_name
         )
    in
    (* Add the new drivers *)
    let added_drivers =
      DriverSet.diff filesystem_drivers db_drivers_set
      |> DriverSet.elements
      |> List.map (fun driver_name ->
             let selected_version, active_version, driver_versions =
               get_driver_versions driver_name driver_paths.selected_dir
                 driver_paths.all_versions_dir
             in
             create ~__context ~host ~name:driver_name ~versions:driver_versions
               ~active_version ~selected_version ;
             driver_name
         )
    in
    [
      ("updated_drivers", updated_drivers)
    ; ("removed_drivers", removed_drivers)
    ; ("added_drivers", added_drivers)
    ]
    |> List.filter (fun (_, ds) -> ds <> [])
  with e ->
    D.debug "Failed to parse drivers on host %s: %s - %s" (Ref.string_of host)
      (Printexc.to_string e)
      (Printexc.get_backtrace ()) ;
    []

(* Scans the multi-version drivers filesystem hierarchy, updating the drivers
   already in the DB if necessary, removing the ones that have been deleted
   from the filesystem, and adding new ones *)
let discover ~__context ~host =
  try
    let driver_paths = get_drivers_base_path ~__context ~host in
    let db_drivers =
      let query =
        Printf.sprintf "(field \"host\"=\"%s\")" (Ref.string_of host)
      in
      Db.Host_driver.get_all_records_where ~__context ~expr:query
    in
    let filesystem_drivers = Sys.readdir driver_paths.all_versions_dir in
    update_drivers_db ~__context ~host ~driver_paths db_drivers
      filesystem_drivers
  with
  | Sys_error e ->
      D.debug "Failed to parse drivers on host %s: %s" (Ref.string_of host) e ;
      []
  | e ->
      D.debug "Failed to parse drivers on host %s: %s - %s" (Ref.string_of host)
        (Printexc.to_string e)
        (Printexc.get_backtrace ()) ;
      []

(* Runs on the necessary host *)
let select ~__context ~self ~version =
  let versions = Db.Host_driver.get_versions ~__context ~self in
  match List.find_opt (String.equal version) versions with
  | Some _ -> (
      let host = Helpers.get_localhost ~__context in
      let db_driver = Db.Host_driver.get_record ~__context ~self in
      let driver_paths = get_drivers_base_path ~__context ~host in
      let srcfile =
        driver_paths.all_versions_dir
        // db_driver.host_driver_name
        // version
        // (db_driver.host_driver_name ^ ".ko")
      in
      let destfile =
        driver_paths.selected_dir // (db_driver.host_driver_name ^ ".ko")
      in
      try
        if Sys.file_exists destfile then
          Sys.remove destfile ;
        Unix.symlink srcfile destfile ;
        reload_driver_selection ~__context ~host ~select:true ;

        (* Update the state of this driver to verify if it has been activated
         *)
        let db_drivers = [(self, db_driver)] in
        let filesystem_drivers = Array.of_list [db_driver.host_driver_name] in
        let _ =
          update_drivers_db ~__context ~host ~driver_paths db_drivers
            filesystem_drivers
        in
        ()
      with e ->
        internal_error "Couldn't select the version %s of driver %s: %s - %s"
          version
          (Db.Host_driver.get_uuid ~__context ~self)
          (Printexc.to_string e)
          (Printexc.get_backtrace ())
    )
  | None ->
      invalid_value "version" version

(* Runs on the necessary host *)
let deselect ~__context ~self =
  let host = Helpers.get_localhost ~__context in
  let driver_name = Db.Host_driver.get_name ~__context ~self in
  let driver_paths = get_drivers_base_path ~__context ~host in

  let driverfile = driver_paths.selected_dir // (driver_name ^ ".ko") in
  try
    if Sys.file_exists driverfile then
      Sys.remove driverfile ;
    reload_driver_selection ~__context ~host ~select:false ;

    Db.Host_driver.set_active_version ~__context ~self ~value:"" ;
    Db.Host_driver.set_selected_version ~__context ~self ~value:""
  with Sys_error e ->
    internal_error "Couldn't deselect the driver %s: %s - %s"
      (Db.Host_driver.get_uuid ~__context ~self)
      e
      (Printexc.get_backtrace ())
