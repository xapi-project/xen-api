open Sexplib.Std

module TopLevel = struct
  type power_state =
    | Running
    | Halted
    | Suspended
    | Paused
  with sexp, rpc

  type disk =
    | Local of string (** path to a local block device *)
    | VDI of string   (** typically "SR/VDI" *)
  with sexp, rpc
end

module Pci = struct
  type address = {
    domain: int;
    bus: int;
    dev: int;
    fn: int;
  }
  with sexp, rpc

end

module Vgpu = struct
  type gvt_g = {
    physical_pci_address: Pci.address;
    low_gm_sz: int64;
    high_gm_sz: int64;
    fence_sz: int64;
    monitor_config_file: string option;
  } with sexp, rpc

  type nvidia = {
    physical_pci_address: Pci.address;
    config_file: string;
  } with sexp, rpc

end

module Vm = struct
  type igd_passthrough =
    | GVT_d
  with sexp, rpc

  type video_card =
    | Cirrus
    | Standard_VGA
    | Vgpu
    | IGD_passthrough of igd_passthrough
  with sexp, rpc

  type hvm_info = {
    hap: bool;
    shadow_multiplier: float;
    timeoffset: string;
    video_mib: int;
    video: video_card;
    acpi: bool;
    serial: string option;
    keymap: string option;
    vnc_ip: string option;
    pci_emulations: string list;
    pci_passthrough: bool;
    boot_order: string;
    qemu_disk_cmdline: bool;
    qemu_stubdom: bool;
  }
  with sexp, rpc

  let default_video_card = Cirrus

  let default_hvm_info = {
    hap = true;
    shadow_multiplier = 1.0;
    timeoffset = "";
    video_mib = 4;
    video = default_video_card;
    acpi = true;
    serial = None;
    keymap = None;
    vnc_ip = None;
    pci_emulations = [];
    pci_passthrough = false;
    boot_order = "";
    qemu_disk_cmdline = false;
    qemu_stubdom = false;
  }

  type pv_direct_boot = {
    kernel: string;
    cmdline: string;
    ramdisk: string option;
  }
  with sexp, rpc

  let default_pv_direct_boot = {
    kernel = "";
    cmdline = "";
    ramdisk = None;
  }

  type pv_indirect_boot = {
    bootloader: string;
    extra_args: string;
    legacy_args: string;
    bootloader_args: string;
    devices: TopLevel.disk list;
  }
  with sexp, rpc

  let default_pv_indirect_boot = {
    bootloader = "";
    extra_args = "";
    legacy_args = "";
    bootloader_args = "";
    devices = [];
  }

  type pv_boot =
    | Direct of pv_direct_boot
    | Indirect of pv_indirect_boot
  with sexp, rpc

  let default_pv_boot = Direct default_pv_direct_boot

  type pv_info = {
    boot: pv_boot;
    framebuffer: bool;
    framebuffer_ip: string option;
    vncterm: bool;
    vncterm_ip: string option;
  }
  with sexp, rpc

  let default_pv_info = {
    boot = default_pv_boot;
    framebuffer = true;
    framebuffer_ip = None;
    vncterm = true;
    vncterm_ip = None;
  }

  type builder_info =
    | HVM of hvm_info
    | PV of pv_info
  with sexp, rpc

  let default_builder_info = HVM default_hvm_info

  type id = string with sexp, rpc

  let default_id = ""

  type action =
    | Coredump
    | Shutdown
    | Start
    | Pause
  with sexp, rpc

  let default_action = Coredump

  type scheduler_params = {
    priority: (int * int) option; (* weight, cap *)
    affinity: int list list (* vcpu -> pcpu list *)
  } with sexp, rpc

  let default_scheduler_params = {
    priority = None;
    affinity = [];
  }

  type t = {
    id: id;
    name: string;
    ssidref: int32;
    xsdata: (string * string) list;
    platformdata: (string * string) list;
    bios_strings: (string * string) list;
    ty: builder_info;
    suppress_spurious_page_faults: bool;
    machine_address_size: int option;
    memory_static_max: int64;
    memory_dynamic_max: int64;
    memory_dynamic_min: int64;
    vcpu_max: int; (* boot-time maximum *)
    vcpus: int;    (* ideal number to use *)
    scheduler_params: scheduler_params;
    on_crash: action list;
    on_shutdown: action list;
    on_reboot: action list;
    pci_msitranslate: bool;
    pci_power_mgmt: bool;
    has_vendor_device: bool;
  } with sexp, rpc

  let default_t = {
    id = default_id;
    name = "unnamed";
    ssidref = 0l;
    xsdata = [];
    platformdata = [];
    bios_strings = [];
    ty = default_builder_info;
    suppress_spurious_page_faults = false;
    machine_address_size = None;
    memory_static_max = 0L;
    memory_dynamic_max = 0L;
    memory_dynamic_min = 0L;
    vcpu_max = 0;
    vcpus = 0;
    scheduler_params = default_scheduler_params;
    on_crash = [];
    on_shutdown = [];
    on_reboot = [];
    pci_msitranslate = false;
    pci_power_mgmt = false;
    has_vendor_device = false;
  }
  let t_of_rpc rpc = Rpc.struct_extend rpc (rpc_of_t default_t) |> t_of_rpc

  type console_protocol =
    | Rfb
    | Vt100
  with sexp, rpc

  type console = {
    protocol: console_protocol;
    port: int;
    path: string;
  } with sexp, rpc

  type state = {
    power_state: TopLevel.power_state;
    domids: int list;
    consoles: console list;
    memory_target: int64;
    memory_actual: int64;
    memory_limit: int64;
    vcpu_target: int; (* actual number of vcpus *)
    shadow_multiplier_target: float; (* actual setting *)
    rtc_timeoffset: string;
    uncooperative_balloon_driver: bool;
    guest_agent: (string * string) list;
    xsdata_state: (string * string) list;
    pv_drivers_detected: bool;
    last_start_time: float;
    hvm: bool;
    nomigrate: bool; (* true: VM must not migrate *)
    nested_virt: bool (* true: VM uses nested virtualisation *)
  } with sexp, rpc

end
