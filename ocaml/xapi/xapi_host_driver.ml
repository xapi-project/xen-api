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

let update_drivers_db ~__context ~host ~driver_paths db_drivers
    filesystem_drivers =
  failwith "TODO"

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
