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
open Xapi_vif_helpers
module D = Debug.Debugger(struct let name="xapi" end)
open D

let assert_operation_valid ~__context ~self ~(op:API.vif_operations) = 
  assert_operation_valid ~__context ~self ~op

let update_allowed_operations ~__context ~self : unit =
  update_allowed_operations ~__context ~self


let plug ~__context ~self =
	Xapi_xenops.vif_plug ~__context ~self

let unplug ~__context ~self =
	Xapi_xenops.vif_unplug ~__context ~self false

let unplug_force ~__context ~self =
	Xapi_xenops.vif_unplug ~__context ~self true

let create  ~__context ~device ~network ~vM
           ~mAC ~mTU ~other_config ~qos_algorithm_type ~qos_algorithm_params : API.ref_VIF =
  create ~__context ~device ~network ~vM ~currently_attached:false
    ~mAC ~mTU ~other_config ~qos_algorithm_type ~qos_algorithm_params

let destroy  ~__context ~self = destroy ~__context ~self

(* This function moves a dom0 vif device from one bridge to another, without involving the guest,
 * so it also works on guests that do not support hot(un)plug of VIFs. *)
let move ~__context ~network vif =
	debug "Moving VIF %s to network %s" (Db.VIF.get_uuid ~__context ~self:vif)
		(Db.Network.get_uuid ~__context ~self:network);
	let vif_rec = Db.VIF.get_record ~__context ~self:vif in
	let suspended = Db.VM.get_power_state ~__context ~self:vif_rec.API.vIF_VM = `Suspended in
	let attached = vif_rec.API.vIF_currently_attached && not suspended in
	Db.VIF.set_network ~__context ~self:vif ~value:network;
	if attached then Xapi_xenops.vif_move ~__context ~self:vif network

