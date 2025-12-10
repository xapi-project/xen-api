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

open Xenops_interface
open Xenops_utils

let unimplemented x = raise (Xenopsd_error (Unimplemented x))

let init () = ()

module HOST = struct
  let stat () =
    {
      Host.cpu_info=
        {
          Host.cpu_count= 0
        ; socket_count= 0
        ; threads_per_core= 0
        ; nr_nodes= 0
        ; vendor= "unknown"
        ; speed= ""
        ; modelname= ""
        ; family= ""
        ; model= ""
        ; stepping= ""
        ; flags= ""
        ; features= CPU_policy.of_string `host ""
        ; features_pv= CPU_policy.of_string `host ""
        ; features_hvm= CPU_policy.of_string `host ""
        ; features_pv_host= CPU_policy.of_string `host ""
        ; features_hvm_host= CPU_policy.of_string `host ""
        }
    ; hypervisor= {Host.version= ""; capabilities= ""}
    ; chipset_info= {iommu= false; hvm= false}
    }

  let get_console_data () = ""

  let get_total_memory_mib () = 0L

  let send_debug_keys _ = ()

  let update_guest_agent_features _ = ()

  let combine_cpu_policies _ _ = CPU_policy.of_string `host ""

  let is_compatible _ _ = false
end

module VM = struct
  let add _ = ()

  let rename _ _ _ = ()

  let remove _ = ()

  let create _ _ _ _ = unimplemented __FUNCTION__

  let build ?restore_fd:_ _ _ _ _ _ = unimplemented __FUNCTION__

  let create_device_model _ _ _ _ _ = unimplemented __FUNCTION__

  let destroy_device_model _ _ = unimplemented __FUNCTION__

  let destroy _ _ = unimplemented __FUNCTION__

  let pause _ _ = unimplemented __FUNCTION__

  let unpause _ _ = unimplemented __FUNCTION__

  let set_xsdata _ _ _ = unimplemented __FUNCTION__

  let set_vcpus _ _ _ = unimplemented __FUNCTION__

  let set_shadow_multiplier _ _ _ = unimplemented __FUNCTION__

  let set_memory_dynamic_range _ _ _ _ = unimplemented __FUNCTION__

  let request_shutdown _ _ _ _ = unimplemented __FUNCTION__

  let wait_shutdown _ _ _ _ = unimplemented __FUNCTION__

  let assert_can_save _ = unimplemented __FUNCTION__

  let save _ _ _ _ _ _ _ = unimplemented __FUNCTION__

  let restore _ _ _ _ _ _ _ = unimplemented __FUNCTION__

  let resume _ _ = unimplemented __FUNCTION__

  let s3suspend _ _ = unimplemented __FUNCTION__

  let s3resume _ _ = unimplemented __FUNCTION__

  let soft_reset _ _ = unimplemented __FUNCTION__

  let get_state _ = Xenops_utils.halted_vm

  let request_rdp _ _ = unimplemented __FUNCTION__

  let run_script _ _ _ = unimplemented __FUNCTION__

  let set_domain_action_request _ _ = ()

  let get_domain_action_request _ = None

  let get_hook_args _ = []

  let generate_state_string _ = ""

  let get_internal_state _ _ _ = ""

  let set_internal_state _ _ = ()

  let wait_ballooning _ _ = ()

  let minimum_reboot_delay = 0.
end

module PCI = struct
  let get_state _ _ = unplugged_pci

  let dequarantine _ = ()

  let plug _ _ _ = unimplemented __FUNCTION__

  let unplug _ _ _ = unimplemented __FUNCTION__

  let get_device_action_request _ _ = None
end

module VBD = struct
  let set_active _ _ _ _ = ()

  let epoch_begin _ _ _ _ = ()

  let epoch_end _ _ _ = ()

  let attach _ _ _ = unimplemented __FUNCTION__

  let activate _ _ _ = unimplemented __FUNCTION__

  let unplug _ _ _ _ = unimplemented __FUNCTION__

  let deactivate _ _ _ _ = unimplemented __FUNCTION__

  let detach _ _ _ = unimplemented __FUNCTION__

  let insert _ _ _ _ = unimplemented __FUNCTION__

  let eject _ _ _ = unimplemented __FUNCTION__

  let set_qos _ _ _ = ()

  let get_state _ _ = unplugged_vbd

  let get_device_action_request _ _ = None
end

module VIF = struct
  let set_active _ _ _ _ = ()

  let plug _ _ _ = unimplemented __FUNCTION__

  let unplug _ _ _ _ = unimplemented __FUNCTION__

  let move _ _ _ _ = unimplemented __FUNCTION__

  let set_carrier _ _ _ _ = unimplemented __FUNCTION__

  let set_locking_mode _ _ _ _ = unimplemented __FUNCTION__

  let set_ipv4_configuration _ _ _ _ = unimplemented __FUNCTION__

  let set_ipv6_configuration _ _ _ _ = unimplemented __FUNCTION__

  let set_pvs_proxy _ _ _ _ = unimplemented __FUNCTION__

  let get_state _ _ = unplugged_vif

  let get_device_action_request _ _ = None
end

module VGPU = struct
  let start _ _ _ _ = unimplemented __FUNCTION__

  let set_active _ _ _ _ = ()

  let get_state _ _ = unplugged_vgpu
end

module VUSB = struct
  let plug _ _ _ = unimplemented __FUNCTION__

  let unplug _ _ _ = unimplemented __FUNCTION__

  let get_state _ _ = unplugged_vusb

  let get_device_action_request _ _ = None
end

module UPDATES = struct
  let get _ _ =
    while true do
      Thread.delay 5.
    done ;
    assert false
end

module DEBUG = struct let trigger _ _ = unimplemented __FUNCTION__ end
