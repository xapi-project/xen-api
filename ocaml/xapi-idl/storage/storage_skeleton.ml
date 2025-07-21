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
[@@@ocaml.warning "-27"]

type context = unit

module UPDATES = struct
  let get ctx ~dbg ~from ~timeout =
    (* block forever *)
    while true do
      Thread.delay 5.
    done ;
    ([], "")
end

module Query = struct
  let query ctx ~dbg = Storage_interface.unimplemented __FUNCTION__

  let diagnostics ctx ~dbg = Storage_interface.unimplemented __FUNCTION__
end

module DP = struct
  let create ctx ~dbg ~id = Storage_interface.unimplemented __FUNCTION__

  let destroy ctx ~dbg ~dp ~allow_leak =
    Storage_interface.unimplemented __FUNCTION__

  let destroy2 ctx ~dbg ~dp ~sr ~vdi ~vm ~allow_leak =
    Storage_interface.unimplemented __FUNCTION__

  let attach_info ctx ~dbg ~sr ~vdi ~dp ~vm =
    Storage_interface.unimplemented __FUNCTION__

  let diagnostics ctx () = Storage_interface.unimplemented __FUNCTION__

  let stat_vdi ctx ~dbg ~sr ~vdi () =
    Storage_interface.unimplemented __FUNCTION__
end

module SR = struct
  let create ctx ~dbg ~sr ~name_label ~name_description ~device_config
      ~physical_size =
    Storage_interface.unimplemented __FUNCTION__

  let attach ctx ~dbg ~sr ~device_config =
    Storage_interface.unimplemented __FUNCTION__

  let set_name_label ctx ~dbg ~sr ~new_name_label =
    Storage_interface.unimplemented __FUNCTION__

  let set_name_description ctx ~dbg ~sr ~new_name_description =
    Storage_interface.unimplemented __FUNCTION__

  let detach ctx ~dbg ~sr = Storage_interface.unimplemented __FUNCTION__

  let reset ctx ~dbg ~sr = Storage_interface.unimplemented __FUNCTION__

  let destroy ctx ~dbg ~sr = Storage_interface.unimplemented __FUNCTION__

  let probe ctx ~dbg ~queue ~device_config ~sm_config =
    Storage_interface.unimplemented __FUNCTION__

  let scan ctx ~dbg ~sr = Storage_interface.unimplemented __FUNCTION__

  let scan2 ctx ~dbg ~sr = Storage_interface.unimplemented __FUNCTION__

  let update_snapshot_info_src ctx ~dbg ~sr ~vdi ~url ~dest ~dest_vdi
      ~snapshot_pairs =
    Storage_interface.unimplemented __FUNCTION__

  let update_snapshot_info_dest ctx ~dbg ~sr ~vdi ~src_vdi ~snapshot_pairs =
    Storage_interface.unimplemented __FUNCTION__

  let stat ctx ~dbg ~sr = Storage_interface.unimplemented __FUNCTION__

  let list ctx ~dbg = Storage_interface.unimplemented __FUNCTION__
end

module VDI = struct
  let create ctx ~dbg ~sr ~vdi_info =
    Storage_interface.unimplemented __FUNCTION__

  let set_name_label ctx ~dbg ~sr ~vdi ~new_name_label =
    Storage_interface.unimplemented __FUNCTION__

  let set_name_description ctx ~dbg ~sr ~vdi ~new_name_description =
    Storage_interface.unimplemented __FUNCTION__

  let snapshot ctx ~dbg ~sr ~vdi_info =
    Storage_interface.unimplemented __FUNCTION__

  let clone ctx ~dbg ~sr ~vdi_info =
    Storage_interface.unimplemented __FUNCTION__

  let resize ctx ~dbg ~sr ~vdi ~new_size =
    Storage_interface.unimplemented __FUNCTION__

  let destroy ctx ~dbg ~sr ~vdi = Storage_interface.unimplemented __FUNCTION__

  let stat ctx ~dbg ~sr ~vdi = Storage_interface.unimplemented __FUNCTION__

  let introduce ctx ~dbg ~sr ~uuid ~sm_config ~location =
    Storage_interface.unimplemented __FUNCTION__

  let set_persistent ctx ~dbg ~sr ~vdi ~persistent =
    Storage_interface.unimplemented __FUNCTION__

  let epoch_begin ctx ~dbg ~sr ~vdi ~vm ~persistent = ()

  let attach ctx ~dbg ~dp ~sr ~vdi ~read_write =
    Storage_interface.unimplemented __FUNCTION__

  let attach2 ctx ~dbg ~dp ~sr ~vdi ~read_write =
    Storage_interface.unimplemented __FUNCTION__

  let attach3 ctx ~dbg ~dp ~sr ~vdi ~vm ~read_write =
    Storage_interface.unimplemented __FUNCTION__

  let activate ctx ~dbg ~dp ~sr ~vdi =
    Storage_interface.unimplemented __FUNCTION__

  let activate3 ctx ~dbg ~dp ~sr ~vdi ~vm =
    Storage_interface.unimplemented __FUNCTION__

  let activate_readonly ctx ~dbg ~dp ~sr ~vdi ~vm =
    Storage_interface.unimplemented __FUNCTION__

  let deactivate ctx ~dbg ~dp ~sr ~vdi ~vm =
    Storage_interface.unimplemented __FUNCTION__

  let detach ctx ~dbg ~dp ~sr ~vdi ~vm =
    Storage_interface.unimplemented __FUNCTION__

  let epoch_end ctx ~dbg ~sr ~vdi ~vm = ()

  let get_url ctx ~dbg ~sr ~vdi = Storage_interface.unimplemented __FUNCTION__

  let similar_content ctx ~dbg ~sr ~vdi =
    Storage_interface.unimplemented __FUNCTION__

  let get_by_name ctx ~dbg ~sr ~name =
    Storage_interface.unimplemented __FUNCTION__

  let set_content_id ctx ~dbg ~sr ~vdi ~content_id =
    Storage_interface.unimplemented __FUNCTION__

  let compose ctx ~dbg ~sr ~vdi1 ~vdi2 =
    Storage_interface.unimplemented __FUNCTION__

  let add_to_sm_config ctx ~dbg ~sr ~vdi ~key ~value =
    Storage_interface.unimplemented __FUNCTION__

  let remove_from_sm_config ctx ~dbg ~sr ~vdi ~key =
    Storage_interface.unimplemented __FUNCTION__

  let enable_cbt ctx ~dbg ~sr ~vdi =
    Storage_interface.unimplemented __FUNCTION__

  let disable_cbt ctx ~dbg ~sr ~vdi =
    Storage_interface.unimplemented __FUNCTION__

  let data_destroy ctx ~dbg ~sr ~vdi =
    Storage_interface.unimplemented __FUNCTION__

  let list_changed_blocks ctx ~dbg ~sr ~vdi_from ~vdi_to =
    Storage_interface.unimplemented __FUNCTION__
end

let get_by_name ctx ~dbg ~name = Storage_interface.unimplemented __FUNCTION__

module DATA = struct
  let copy ctx ~dbg ~sr ~vdi ~vm ~url ~dest =
    Storage_interface.unimplemented __FUNCTION__

  let mirror ctx ~dbg ~sr ~vdi ~vm ~dest =
    Storage_interface.unimplemented __FUNCTION__

  let stat ctx ~dbg ~sr ~vdi ~vm ~key =
    Storage_interface.unimplemented __FUNCTION__

  let import_activate ctx ~dbg ~dp ~sr ~vdi ~vm =
    Storage_interface.unimplemented __FUNCTION__

  let get_nbd_server ctx ~dbg ~dp ~sr ~vdi ~vm =
    Storage_interface.unimplemented __FUNCTION__

  module MIRROR = struct
    type context = unit

    let send_start ctx ~dbg ~task_id ~dp ~sr ~vdi ~mirror_vm ~mirror_id
        ~local_vdi ~copy_vm ~live_vm ~url ~remote_mirror ~dest_sr ~verify_dest =
      Storage_interface.unimplemented __FUNCTION__

    let receive_start ctx ~dbg ~sr ~vdi_info ~id ~similar =
      Storage_interface.unimplemented __FUNCTION__

    let receive_start2 ctx ~dbg ~sr ~vdi_info ~id ~similar ~vm =
      Storage_interface.unimplemented __FUNCTION__

    let receive_start3 ctx ~dbg ~sr ~vdi_info ~mirror_id ~similar ~vm ~url
        ~verify_dest =
      Storage_interface.unimplemented __FUNCTION__

    let receive_finalize ctx ~dbg ~id =
      Storage_interface.unimplemented __FUNCTION__

    let receive_finalize2 ctx ~dbg ~id =
      Storage_interface.unimplemented __FUNCTION__

    let receive_finalize3 ctx ~dbg ~mirror_id ~sr ~url ~verify_dest =
      Storage_interface.unimplemented __FUNCTION__

    let receive_cancel ctx ~dbg ~id =
      Storage_interface.unimplemented __FUNCTION__

    let receive_cancel2 ctx ~dbg ~mirror_id ~url ~verify_dest =
      Storage_interface.unimplemented __FUNCTION__

    let pre_deactivate_hook ctx ~dbg ~dp ~sr ~vdi =
      Storage_interface.unimplemented __FUNCTION__

    let has_mirror_failed ctx ~dbg ~mirror_id ~sr =
      Storage_interface.unimplemented __FUNCTION__

    let list ctx ~dbg = Storage_interface.unimplemented __FUNCTION__

    let stat ctx ~dbg ~id = Storage_interface.unimplemented __FUNCTION__
  end
end

module Policy = struct
  let get_backend_vm ctx ~dbg ~vm ~sr ~vdi =
    Storage_interface.unimplemented __FUNCTION__
end

module TASK = struct
  let stat ctx ~dbg ~task = Storage_interface.unimplemented __FUNCTION__

  let cancel ctx ~dbg ~task = Storage_interface.unimplemented __FUNCTION__

  let destroy ctx ~dbg ~task = Storage_interface.unimplemented __FUNCTION__

  let list ctx ~dbg = Storage_interface.unimplemented __FUNCTION__
end
