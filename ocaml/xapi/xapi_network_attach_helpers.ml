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
 
module D=Debug.Debugger(struct let name="xapi" end)
open D

let assert_network_has_no_vifs_in_use_on_me ~__context ~host ~network =
  (* Check if there are any active VIFs on VMs resident on me *)
  let vifs = Db.Network.get_VIFs ~__context ~self:network in
  List.iter (fun self ->
	       if Db.VIF.get_currently_attached ~__context ~self then
		 begin
		   let vm = Db.VIF.get_VM ~__context ~self in 
		   let resident_on = Db.VM.get_resident_on ~__context ~self:vm in
		   if resident_on=host then
		     let powerstate = Db.VM.get_power_state ~__context ~self:vm in
		     if powerstate=`Running || powerstate=`Paused then
		       raise (Api_errors.Server_error(Api_errors.vif_in_use, [ Ref.string_of network; Ref.string_of self ]))
		 end)
    vifs

(* nice triple negative ;) *)
let assert_pif_disallow_unplug_not_set ~__context pif =
  if (Db.PIF.get_disallow_unplug ~__context ~self:pif) then
    raise (Api_errors.Server_error(Api_errors.pif_does_not_allow_unplug, [ Ref.string_of pif ]))    

let assert_can_attach_network_on_host ~__context ~self ~host =
  let pifs = Db.Network.get_PIFs ~__context ~self in
  (* There really should be only one local PIF by construction *)
  let local_pifs = List.filter (fun self -> Db.PIF.get_host ~__context ~self = host) pifs in
  (* Plugging a bond slave is not allowed *)
  let assert_no_slave pif =
    if Db.PIF.get_bond_slave_of ~__context ~self:pif <> Ref.null then
      raise (Api_errors.Server_error (Api_errors.cannot_plug_bond_slave, [Ref.string_of self]))
  in
  List.iter assert_no_slave local_pifs;
  local_pifs

