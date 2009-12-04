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
(**
 * @group Storage
 *)
 
module D=Debug.Debugger(struct let name="xapi" end)
open D

(** Enumerate the registered plugins and recreate db records for them *)
let resync_plugins ~__context =
  let existing = Db.SM.get_records_where ~__context ~expr:Db_filter_types.True in
  let drivers = Sm.supported_drivers () in
  (* CA-24517: preserve SM.other_config keys across xapi restart for HP *)
  let other_config_tbl : (string, (string * string) list) Hashtbl.t = Hashtbl.create 10 in

  (* remove all existing db records for sm plugins *)
  List.iter (fun (x, y) ->
	       debug "Deleting storage plugin from database: %s" y.API.sM_type;
	       Hashtbl.replace other_config_tbl y.API.sM_type y.API.sM_other_config;
	       Db.SM.destroy ~__context ~self:x) existing;

  (* make new db records for discovered sm plugins *)
  List.iter (fun driver ->
	       debug "Registering new driver with database: %s" driver;
	       let r = Ref.make () and u = Uuid.string_of_uuid (Uuid.make_uuid ()) in
	       let info = Sm.info_of_driver driver in
	       Db.SM.create ~__context ~ref:r ~uuid:u ~_type:driver
		 ~name_label:info.Smint.sr_driver_name
		 ~name_description:info.Smint.sr_driver_description
		 ~vendor:info.Smint.sr_driver_vendor
		 ~copyright:info.Smint.sr_driver_copyright
		 ~version:info.Smint.sr_driver_version
		 ~required_api_version:info.Smint.sr_driver_required_api_version 
		 ~capabilities:info.Smint.sr_driver_text_capabilities
		 ~configuration:info.Smint.sr_driver_configuration
		 ~other_config:(try Hashtbl.find other_config_tbl driver with _ -> [])
		 ~driver_filename:(Sm_exec.cmd_name info.Smint.sr_driver_filename)
	    ) drivers

