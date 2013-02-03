(*
 * Copyright (C) Citrix Systems Inc.
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

let u x = raise (Storage_interface.Unimplemented x)

type context = unit

module UPDATES = struct
  let get ctx ~dbg ~from ~timeout =
    (* block forever *)
    while true do Thread.delay 5. done;
    [], ""
end

module Query = struct
  let query ctx ~dbg = u "Query.query"
  let diagnostics ctx ~dbg = u "Query.diagnostics"
end

module DP = struct
  let create ctx ~dbg ~id = u "DP.create"
  let destroy ctx ~dbg ~dp ~allow_leak = u "DP.destroy"
  let attach_info ctx ~dbg ~sr ~vdi ~dp = u "DP.attach_info"
  let diagnostics ctx () = u "DP.diagnostics"
  let stat_vdi ctx ~dbg ~sr ~vdi () = u "DP.stat_vdi"
end

module SR = struct
  let create ctx ~dbg ~sr ~device_config ~physical_size = u "SR.create"
  let attach ctx ~dbg ~sr ~device_config = u "SR.attach"
  let detach ctx ~dbg ~sr = u "SR.detach"
  let reset ctx ~dbg ~sr = u "SR.reset"
  let destroy ctx ~dbg ~sr = u "SR.destroy"
  let scan ctx ~dbg ~sr = u "SR.scan"
  let list ctx ~dbg = u "SR.list"
end

module VDI = struct
  let create ctx ~dbg ~sr ~vdi_info = u "VDI.create"
  let snapshot ctx ~dbg ~sr ~vdi_info = u "VDI.snapshot"
  let clone ctx ~dbg ~sr ~vdi_info = u "VDI.clone"
  let destroy ctx ~dbg ~sr ~vdi = u "VDI.destroy"
  let stat ctx ~dbg ~sr ~vdi = u "VDI.stat"
  let set_persistent ctx ~dbg ~sr ~vdi ~persistent = u "VDI.set_persistent"
  let epoch_begin ctx ~dbg ~sr ~vdi = ()
  let attach ctx ~dbg ~dp ~sr ~vdi ~read_write = u "VDI.attach"
  let activate ctx ~dbg ~dp ~sr ~vdi = u "VDI.activate"
  let deactivate ctx ~dbg ~dp ~sr ~vdi = u "VDI.deactivate"
  let detach ctx ~dbg ~dp ~sr ~vdi = u "VDI.detach"
  let epoch_end ctx ~dbg ~sr ~vdi = ()
  let get_url ctx ~dbg ~sr ~vdi = u "VDI.get_url"
  let similar_content ctx ~dbg ~sr ~vdi = u "VDI.similar_content"
  let get_by_name ctx ~dbg ~sr ~name = u "VDI.get_by_name"
  let set_content_id ctx ~dbg ~sr ~vdi ~content_id = u "VDI.set_content_id"
  let compose ctx ~dbg ~sr ~vdi1 ~vdi2 = u "VDI.compose"
  let add_to_sm_config ctx ~dbg ~sr ~vdi ~key ~value = u "VDI.add_to_sm_config"
  let remove_from_sm_config ctx ~dbg ~sr ~vdi ~key = u "VDI.remove_from_sm_config"
end

let get_by_name ctx ~dbg ~name = u "get_by_name"

module DATA = struct
  let copy_into ctx ~dbg ~sr ~vdi ~url ~dest ~dest_vdi = u "DATA.copy_into"
  let copy ctx ~dbg ~sr ~vdi ~dp ~url ~dest = u "DATA.copy"

  module MIRROR = struct
		(** [start task sr vdi url sr2] creates a VDI in remote [url]'s [sr2] and writes
			data synchronously. It returns the id of the VDI.*)
    let start ctx ~dbg ~sr ~vdi ~dp ~url ~dest = u "DATA.MIRROR.start"
    let stop ctx ~dbg ~id = u "DATA.MIRROR.stop"
    let stat ctx ~dbg ~id = u "DATA.MIRROR.stat"
    let receive_start ctx ~dbg ~sr ~vdi_info ~id ~similar = u "DATA.MIRROR.receive_start"
    let receive_finalize ctx ~dbg ~id  = u "DATA.MIRROR.receive_finalize"
    let receive_cancel ctx ~dbg ~id = u "DATA.MIRROR.receive_cancel"
			
    let list ctx ~dbg = u "DATA.MIRROR.list"
  end

end

module Policy = struct
  let get_backend_vm ctx ~dbg ~vm ~sr ~vdi = u "Policy.get_backend_vm"
end

module TASK = struct
  let stat ctx ~dbg ~task = u "TASK.stat"
  let cancel ctx ~dbg ~task = u "TASK.cancel"
  let destroy ctx ~dbg ~task = u "TASK.destroy"
  let list ctx ~dbg = u "TASK.list"
end

