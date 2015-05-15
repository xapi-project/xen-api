open OUnit

(* This was created on a creedence host. I have deliberately removed the field   *)
(*                                                                               *)
(*       "name": "foo"                                                           *)
(*                                                                               *)
(* from just after the 'id' field. This is to test that the field gets defaulted *)
(* to the value specified in the idl.                                            *)

let old_vm_t = "{\"id\": \"bc6b8e8a-f0a5-4746-6489-2745756f21b2\", \"ssidref\": 0, \"xsdata\": {\"vm-data\": \"\"}, \"platformdata\": {\"generation-id\": \"\", \"timeoffset\": \"0\", \"usb\": \"true\", \"usb_tablet\": \"true\"}, \"bios_strings\": {\"bios-vendor\": \"Xen\", \"bios-version\": \"\", \"system-manufacturer\": \"Xen\", \"system-product-name\": \"HVM domU\", \"system-version\": \"\", \"system-serial-number\": \"\", \"hp-rombios\": \"\", \"oem-1\":\"Xen\", \"oem-2\": \"MS_VM_CERT/SHA1/bdbeb6e0a816d43fa6d3fe8aaef04c2bad9d3e3d\"}, \"ty\": [\"HVM\", {\"hap\": true, \"shadow_multiplier\": 1.000000, \"timeoffset\": \"0\", \"video_mib\": 4, \"video\": \"Cirrus\", \"acpi\": true, \"serial\": \"pty\", \"keymap\": \"en-us\", \"pci_emulations\": [], \"pci_passthrough\": false, \"boot_order\": \"cd\", \"qemu_disk_cmdline\": false, \"qemu_stubdom\": false}], \"suppress_spurious_page_faults\": false, \"memory_static_max\": 268435456, \"memory_dynamic_max\": 268435456, \"memory_dynamic_min\": 134217728, \"vcpu_max\": 1, \"vcpus\": 1, \"scheduler_params\": {\"priority\": [256, 0], \"affinity\": []}, \"on_crash\": [\"Shutdown\"], \"on_shutdown\": [\"Shutdown\"], \"on_reboot\": [\"Start\"], \"pci_msitranslate\": true, \"pci_power_mgmt\": false}"

let test_upgrade_rules () = 
	let old_json = old_vm_t in
	let rpc = Jsonrpc.of_string old_json in
	let vm_t = Xenops_interface.Vm.t_of_rpc rpc in
	assert_equal vm_t.Xenops_interface.Vm.name "unnamed" (* this value is the default in xenops_interface.ml *)

let tests =
  "xcp-xen" >:::
    [
      "check upgrade rule" >:: test_upgrade_rules
    ]
