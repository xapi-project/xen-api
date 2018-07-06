(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

open OUnit2
open Qmp

let my_dir = "."

let files = [
  "capabilities.json",             Command (None, Qmp_capabilities);
  "error.json",                    Error (None, { cls="JSONParsing"; descr="Invalid JSON syntax" });
  "greeting.json",                 Greeting { major = 1; minor = 1; micro = 0; package = " (Debian 1.1.0+dfsg-1)" };
  "event-powerdown.json",          Event { timestamp = (1258551470, 802380); event = "POWERDOWN"; data = None };
  "query-commands.json",           Command (None, Query_commands);
  "query-commands-return.json",    Success (None, Name_list [ "qom-list-types"; "change-vnc-password" ]);
  "query_kvm.json",                Command (Some "example", Query_kvm);
  "query_kvm-return.json",         Success (Some "example", Enabled { enabled=true; present = true });
  "stop.json",                     Command (None, Stop);
  "success.json",                  Success (None, Unit);
  "eject.json",                    Command (None, Eject  ("ide1-cd0", None));
  "eject_force.json",              Command (None, Eject  ("ide1-cd0", Some true));
  "change-cd.json",                Command (None, Change ("ide1-cd0", "/tmp/cdrom.iso", None));
  "change-vnc.json",               Command (None, Change ("vnc", "password", Some "foobar1"));
  "query-status.json",             Command (None, Query_status);
  "query-status-result.json",      Success (None, Status "running");
  "query-vnc.json",                Command (None, Query_vnc);
  "query-vnc-result.json",         Success (None, Vnc {enabled=true; auth="none"; family="ipv4"; service=6034; host="127.0.0.1"});
  "event-block-io-error.json",     Event { timestamp = (1265044230, 450480); event = "BLOCK_IO_ERROR"; data = None };
  "event-rtc-change.json",         Event { timestamp = (1267020223, 435656); event = "RTC_CHANGE"; data = Some (RTC_CHANGE 78L) };
  "event-xen-platform-pv-driver-info.json",
                                   Event { timestamp = (1267020223, 435656); event = "XEN_PLATFORM_PV_DRIVER_INFO";
                                           data = Some (XEN_PLATFORM_PV_DRIVER_INFO { product_num = 3; build_num = 1} ) };
  "xen-save-devices-state.json",   Command (None, Xen_save_devices_state "/tmp/qemu-save");
  "xen-load-devices-state.json",   Command (None, Xen_load_devices_state "/tmp/qemu-resume");
  "xen-set-global-dirty-log.json", Command (None, Xen_set_global_dirty_log true);
  "add-fd-with-fdset.json",        Command (None, Add_fd (Some 4));
  "add-fd-no-fdset.json",          Command (None, Add_fd None);
  "add-fd-return.json",            Success (None, Fd_info { fdset_id=4; fd=21; });
  "add-fd-return-error-notanint.json", Error (None, { cls="JSONParsing"; descr="Expected int, got string \"foo\" in {\"return\": {\"fd\": \"foo\", \"fdset-id\": 4}}"; });
  "remove-fd.json",                Command (None, Remove_fd 4);
  "blockdev-change-medium.json",   Command (None, Blockdev_change_medium ("ide1-cd0", "/dev/fd/21"));
  "query-xen-platform-pv-driver-info.json", Command (None, Query_xen_platform_pv_driver_info);
  "query-xen-platform-pv-driver-info-result.json", Success (None, Xen_platform_pv_driver_info { product_num=3; build_num=1; });
  "query-xen-platform-pv-driver-info-result-error-notanint.json", Error (None, { cls="JSONParsing"; descr="Expected int, got string \"foo\" in {\"return\": {\"product-num\": \"foo\", \"build-num\": 1}}"; });
  "query-hotpluggable-cpus.json", Command (None, Query_hotpluggable_cpus);
  "query-hotpluggable-cpus-result.json", Success (None, Hotpluggable_cpus [ { qom_path=None; driver_type=Device.VCPU.Driver.(string_of QEMU32_I386_CPU); vcpus_count=1; props={id="cpu-1-2-3"; socket_id=1; core_id=2; thread_id=3 }}; { qom_path=Some "/machine/unattached/device[1]"; driver_type=Device.VCPU.Driver.(string_of QEMU32_I386_CPU); vcpus_count=1; props={id="cpu-0-0-0"; socket_id=0; core_id=0; thread_id=0}} ]);
  "device_del.json",               Command (None, Device_del "usb1");
  "device_add_usbcontroller.json", Command (None, Device_add { driver=Device.USB.Driver.(string_of USB_EHCI); device=USB { id="ehci"; params=None}});
  "device_add_usbdevice.json",     Command (None, Device_add { driver=Device.USB.Driver.(string_of USB_HOST); device=USB { id="usb1"; params=Some { bus="ehci.0"; hostbus="2"; hostport="2"}}});
  "device_add_vcpu.json",          Command (None, Device_add { driver=Device.VCPU.Driver.(string_of QEMU32_I386_CPU); device=VCPU {id="cpu-1-2-0";socket_id=1;core_id=2;thread_id=0}});
  "qom_list_peripheral.json",      Command (None, Qom_list "/machine/peripheral");
  "qom_list_peripheral_result.json", Success (None, Qom [{name="usb1"; ty="child"}; {name="type"; ty="string"}]);
]

let string_of_file filename =
  let ic = open_in (Filename.concat my_dir filename) in
  let lines = ref [] in
  (try while true do lines := input_line ic :: !lines done with End_of_file -> ());
  close_in ic;
  String.concat "\n" (List.rev !lines)

let test_message_of_string (filename, expected) test_ctxt =
  let txt = string_of_file filename in
  let actual = Qmp.message_of_string txt in
  assert_equal ~printer:Qmp.string_of_message expected actual

let parse_print expected test_ctxt =
  let actual = Qmp.message_of_string (Qmp.string_of_message expected) in
  assert_equal ~printer:Qmp.string_of_message expected actual

let _ =
  let message_of_string = "message_of_string" >::: (List.map (fun (filename, expected) ->
    filename >:: (test_message_of_string (filename, expected))
  ) files) in
  let parse_print = "parse_print" >::: (List.map (fun (filename, expected) ->
    filename >:: (parse_print expected)
  ) files) in

  let suite = "qmp" >:::
    [
      message_of_string;
      parse_print;
    ] in
  run_test_tt_main suite



