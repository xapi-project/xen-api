open Sexplib.Std
open Xcp_pci

module TopLevel = struct
  type power_state =
    | Running
    | Halted
    | Suspended
    | Paused
  [@@deriving sexp, rpcty]

  type disk =
    | Local of string (** path to a local block device *)
    | VDI of string   (** typically "SR/VDI" *)
  [@@deriving sexp, rpcty]
end

module Vgpu = struct
  type gvt_g = {
    physical_pci_address: address option; (* unused; promoted to Vgpu.t *)
    low_gm_sz: int64;
    high_gm_sz: int64;
    fence_sz: int64;
    monitor_config_file: string option;
  } [@@deriving sexp, rpcty]

  type nvidia = {
    physical_pci_address: address option; (* unused; promoted to Vgpu.t *)
    config_file: string;
  } [@@deriving sexp, rpcty]

  type mxgpu = {
    physical_function: address option; (* unused; promoted to Vgpu.t *)
    vgpus_per_pgpu: int64;
    framebufferbytes: int64;
  } [@@deriving sexp, rpcty]

end

module Nvram_uefi_variables = struct
  type onboot =
    | Persist
    | Reset
    [@@deriving rpcty, sexp]

  type t = {
    on_boot: onboot [@default Persist];
    backend: string [@default "xapidb"];
  } [@@deriving rpcty, sexp]

  let default_t =
    match Rpcmarshal.unmarshal t.Rpc.Types.ty Rpc.(Dict []) with
    | Ok x -> x
    | Error (`Msg m) ->
      failwith (Printf.sprintf "Error creating Nvram_uefi_variables.default_t: %s" m)
end

module Vm = struct
  type igd_passthrough =
    | GVT_d
  [@@deriving rpcty, sexp]

  type video_card =
    | Cirrus
    | Standard_VGA
    | Vgpu
    | IGD_passthrough of igd_passthrough
  [@@default Cirrus]
  [@@deriving rpcty, sexp]

  type firmware_type =
    | Bios
    | Uefi of Nvram_uefi_variables.t
  [@@deriving rpcty, sexp]

  let default_firmware = Bios [@@deriving rpcty]

  type hvm_info = {
    hap: bool [@default true];
    shadow_multiplier: float [@default 1.0];
    timeoffset: string [@default ""];
    video_mib: int [@default 4];
    video: video_card [@default Cirrus];
    acpi: bool [@default true];
    serial: string option [@default None];
    keymap: string option [@default None];
    vnc_ip: string option [@default None];
    pci_emulations: string list [@default []];
    pci_passthrough: bool [@default false];
    boot_order: string [@default ""]; 
    qemu_disk_cmdline: bool [@default false];
    qemu_stubdom: bool [@default false];
    firmware: firmware_type [@default default_firmware];
  }
  [@@deriving rpcty, sexp]

  type pv_direct_boot = {
    kernel: string [@default ""];
    cmdline: string [@default ""];
    ramdisk: string option [@default None];
  }
  [@@deriving rpcty, sexp]

  type pv_indirect_boot = {
    bootloader: string [@default ""];
    extra_args: string [@default ""];
    legacy_args: string [@default ""];
    bootloader_args: string [@default ""];
    devices: TopLevel.disk list [@default []];
  }
  [@@deriving rpcty, sexp]

  type pv_boot =
    | Direct of pv_direct_boot
    | Indirect of pv_indirect_boot
  [@@deriving rpcty, sexp]

  type pv_info = {
    boot: pv_boot;
    framebuffer: bool [@default true];
    framebuffer_ip: string option [@default None];
    vncterm: bool [@default true];
    vncterm_ip: string option [@default None];
  }
  [@@deriving rpcty, sexp]

  type builder_info =
    | HVM of hvm_info
    | PV of pv_info
    | PVinPVH of pv_info
  [@@deriving rpcty, sexp]

  type id = string [@@deriving rpcty, sexp]

  type action =
    | Coredump
    | Shutdown
    | Start
    | Pause
  [@@deriving rpcty, sexp]

  type scheduler_params = {
    priority: (int * int) option; (* weight, cap *)
    affinity: int list list (* vcpu -> pcpu list *)
  } [@@deriving rpcty, sexp]

  type t = {
    id: id;
    name: string [@default "unnamed"];
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
    has_vendor_device: bool [@default false];
  } [@@deriving rpcty, sexp]

  type console_protocol =
    | Rfb
    | Vt100
  [@@deriving rpcty, sexp]

  type console = {
    protocol: console_protocol;
    port: int;
    path: string;
  } [@@deriving rpcty, sexp]

  type domain_type =
    | Domain_HVM
    | Domain_PV
    | Domain_PVinPVH
    | Domain_undefined
  [@@deriving rpcty, sexp]

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
    nested_virt: bool; (* true: VM uses nested virtualisation *)
    domain_type: domain_type;
  } [@@deriving rpcty, sexp]

end
