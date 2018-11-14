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

let simplified = false
let init () = ()

module HOST = struct
  let stat () = {
    Host.cpu_info = {
      Host.cpu_count = 0;
      socket_count = 0;
      vendor = "unknown";
      speed = "";
      modelname = "";
      family = "";
      model = "";
      stepping = "";
      flags = "";
      features = [| |];
      features_pv = [| |];
      features_hvm = [| |];
      features_oldstyle = [| |];
    };
    hypervisor = {
      Host.version = "";
      capabilities = "";
    }
  }
  let get_console_data () = ""
  let get_total_memory_mib () = 0L
  let send_debug_keys _ = ()
  let update_guest_agent_features _ = ()
  let upgrade_cpu_features _ _ = [||]
end
module VM = struct
  let add _ = ()

  let rename _ _ = ()
  let remove _ = ()
  let create _ _ _ _ = unimplemented "VM.create"
  let build ?restore_fd:_ _ _ _ _ _ = unimplemented "VM.build"
  let create_device_model _ _ _ _ _ = unimplemented "VM.create_device_model"
  let destroy_device_model _ _ = unimplemented "VM.destroy_device_model"
  let destroy _ _ = unimplemented "VM.destroy"
  let pause _ _ = unimplemented "VM.pause"
  let unpause _ _ = unimplemented "VM.unpause"
  let set_xsdata _ _ _ = unimplemented "VM.set_xsdata"
  let set_vcpus _ _ _ = unimplemented "VM.set_vcpus"
  let set_shadow_multiplier _ _ _ = unimplemented "VM.set_shadow_multipler"
  let set_memory_dynamic_range _ _ _ _ = unimplemented "VM.set_memory_dynamic_range"
  let request_shutdown _ _ _ _ = unimplemented "VM.request_shutdown"
  let wait_shutdown _ _ _ _ = unimplemented "VM.wait_shutdown"
  let assert_can_save _ = unimplemented "VM.assert_can_save"
  let save _ _ _ _ _ _ _ = unimplemented "VM.save"
  let restore _ _ _ _ _ _ _ = unimplemented "VM.restore"
  let s3suspend _ _ = unimplemented "VM.s3suspend"
  let s3resume _ _ = unimplemented "VM.s3resume"
  let get_state _ = Xenops_utils.halted_vm
  let request_rdp _ _ = unimplemented "VM.request_rdp"
  let run_script _ _ _ = unimplemented "VM.run_script"
  let set_domain_action_request _ _ = ()
  let get_domain_action_request _ = None
  let generate_state_string _ = ""
  let get_internal_state _ _ _ = ""
  let set_internal_state _ _ = ()
  let wait_ballooning _ _ = ()
  let minimum_reboot_delay = 0.
end
module PCI = struct
  let get_state _ _ = unplugged_pci
  let plug _ _ _ = unimplemented "PCI.plug"
  let unplug _ _ _ = unimplemented "PCI.unplug"
  let get_device_action_request _ _ = None
end
module VBD = struct
  let set_active _ _ _ _ = ()
  let epoch_begin _ _ _ _ = ()
  let epoch_end _ _ _ = ()
  let plug _ _ _ = unimplemented "VBD.plug"
  let unplug _ _ _ _ = unimplemented "VBD.unplug"
  let insert _ _ _ _ = unimplemented "VBD.insert"
  let eject _ _ _ = unimplemented "VBD.eject"
  let set_qos _ _ _ = ()
  let get_state _ _ = unplugged_vbd
  let get_device_action_request _ _ = None
end
module VIF = struct
  let set_active _ _ _ _ = ()
  let plug _ _ _ = unimplemented "VIF.plug"
  let unplug _ _ _ _ = unimplemented "VIF.unplug"
  let move _ _ _ _ = unimplemented "VIF.move"
  let set_carrier _ _ _ _ = unimplemented "VIF.set_carrier"
  let set_locking_mode _ _ _ _ = unimplemented "VIF.set_locking_mode"
  let set_ipv4_configuration _ _ _ _ = unimplemented "VIF.set_ipv4_configuration"
  let set_ipv6_configuration _ _ _ _ = unimplemented "VIF.set_ipv6_configuration"
  let set_pvs_proxy _ _ _ _ = unimplemented "VIF.set_pvs_proxy"
  let get_state _ _ = unplugged_vif
  let get_device_action_request _ _ = None
end
module VGPU = struct
  let start _ _ _ _ = unimplemented "VGPU.start"
  let get_state _ _ = unplugged_vgpu
end
module VUSB = struct
  let plug _ _ _  = unimplemented "VUSB.plug"
  let unplug _ _ _ = unimplemented "VUSB.unplug"
  let get_state _ _ = unplugged_vusb
  let get_device_action_request _ _ = None
end
module UPDATES  = struct
  let get _ _ = while true do Thread.delay 5. done; assert false
end
module DEBUG = struct
  let trigger _ _ = unimplemented "DEBUG.trigger"
end
