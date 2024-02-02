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
(** High-level domain management functions *)

type domid = Xenctrl.domid

exception Suspend_image_failure

exception Not_enough_memory of int64

exception Domain_build_failed

exception Domain_restore_failed

exception Xenguest_protocol_failure of string (* internal protocol failure *)

exception Xenguest_failure of string (* an actual error is reported to us *)

exception Emu_manager_protocol_failure (* internal protocol failure *)

exception Emu_manager_failure of string (* an actual error is reported to us *)

exception Timeout_backend

exception Could_not_read_file of string (* eg linux kernel/ initrd *)

type xen_arm_arch_domainconfig = {
    (* Xenctrl.xen_arm_arch_domainconfig = *) gic_version: int
  ; nr_spis: int
  ; clock_frequency: int32
}

type x86_arch_emulation_flags =
  (* Xenctrl.x86_arch_emulation_flags = *)
  | X86_EMU_LAPIC
  | X86_EMU_HPET
  | X86_EMU_PM
  | X86_EMU_RTC
  | X86_EMU_IOAPIC
  | X86_EMU_PIC
  | X86_EMU_VGA
  | X86_EMU_IOMMU
  | X86_EMU_PIT
  | X86_EMU_USE_PIRQ
  | X86_EMU_VPCI

val emulation_flags_pvh : x86_arch_emulation_flags list

val emulation_flags_all : x86_arch_emulation_flags list

type x86_arch_misc_flags = X86_MSR_RELAXED

type xen_x86_arch_domainconfig = {
    (* Xenctrl.xen_x86_arch_domainconfig = *)
    emulation_flags: x86_arch_emulation_flags list
  ; misc_flags: x86_arch_misc_flags list
}

type arch_domainconfig =
  (* Xenctrl.arch_domainconfig = *)
  | ARM of xen_arm_arch_domainconfig
  | X86 of xen_x86_arch_domainconfig

val typ_of_arch_domainconfig : arch_domainconfig Rpc.Types.typ

val arch_domainconfig : arch_domainconfig Rpc.Types.def

type create_info = {
    ssidref: int32
  ; hvm: bool
  ; hap: bool
  ; name: string
  ; xsdata: (string * string) list
  ; platformdata: (string * string) list
  ; bios_strings: (string * string) list
  ; has_vendor_device: bool
  ; is_uefi: bool
  ; pci_passthrough: bool
}

val typ_of_create_info : create_info Rpc.Types.typ

val create_info : create_info Rpc.Types.def

type build_hvm_info = {shadow_multiplier: float; video_mib: int}

val typ_of_build_hvm_info : build_hvm_info Rpc.Types.typ

val build_hvm_info : build_hvm_info Rpc.Types.def

type build_pv_info = {cmdline: string; ramdisk: string option}

val typ_of_build_pv_info : build_pv_info Rpc.Types.typ

val build_pv_info : build_pv_info Rpc.Types.def

type build_pvh_info = {
    cmdline: string  (** cmdline for the kernel (image) *)
  ; pv_shim: bool  (** whether the domain will be run with a PV shim *)
  ; modules: (string * string option) list
        (** list of modules plus optional cmdlines *)
  ; shadow_multiplier: float
  ; video_mib: int
}

val typ_of_build_pvh_info : build_pvh_info Rpc.Types.typ

val build_pvh_info : build_pvh_info Rpc.Types.def

type builder_spec_info =
  | BuildHVM of build_hvm_info
  | BuildPV of build_pv_info
  | BuildPVH of build_pvh_info

val typ_of_builder_spec_info : builder_spec_info Rpc.Types.typ

val builder_spec_info : builder_spec_info Rpc.Types.def

type build_info = {
    memory_max: int64  (** memory max in kilobytes *)
  ; memory_target: int64  (** memory target in kilobytes *)
  ; kernel: string  (** image to load. In HVM case, point to hvmloader *)
  ; vcpus: int  (** vcpus max *)
  ; priv: builder_spec_info
  ; has_hard_affinity: bool
}

val typ_of_build_info : build_info Rpc.Types.typ

val build_info : build_info Rpc.Types.def

val make :
     xc:Xenctrl.handle
  -> xs:Xenstore.Xs.xsh
  -> create_info
  -> int
  -> arch_domainconfig
  -> [`Vm] Uuidx.t
  -> string option
  -> bool (* no_sharept *)
  -> domid
(** Create a fresh (empty) domain with a specific UUID, returning the domain ID *)

(** 'types' of shutdown request *)
type shutdown_reason =
  | PowerOff
  | Reboot
  | Suspend
  | Crash
  | Halt
  | S3Suspend
  | Unknown of int

val string_of_shutdown_reason : shutdown_reason -> string
(** string versions of the shutdown_reasons, suitable for writing into
    control/shutdown *)

val shutdown_reason_of_int : int -> shutdown_reason
(** decodes the shutdown_reason contained within the xc dominfo struct *)

val hard_shutdown : xc:Xenctrl.handle -> domid -> shutdown_reason -> unit
(** Immediately force shutdown the domain with reason 'shutdown_reason' *)

(** Thrown if the domain has disappeared *)
exception Domain_does_not_exist

val shutdown :
  xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> domid -> shutdown_reason -> unit
(** Tell the domain to shutdown with reason 'shutdown_reason'. Don't wait for an
    ack *)

val shutdown_wait_for_ack :
     Xenops_task.Xenops_task.task_handle
  -> timeout:float
  -> xc:Xenctrl.handle
  -> xs:Xenstore.Xs.xsh
  -> domid
  -> [`hvm | `pv | `pvh]
  -> shutdown_reason
  -> unit
(** Tell the domain to shutdown with reason 'shutdown_reason', waiting for an
    ack *)

val sysrq : xs:Xenstore.Xs.xsh -> domid -> char -> unit
(** send a domain a sysrq *)

val destroy :
     Xenops_task.Xenops_task.task_handle
  -> xc:Xenctrl.handle
  -> xs:Xenstore.Xs.xsh
  -> qemu_domid:int
  -> vtpm:Xenops_interface.Vm.tpm option
  -> dm:Device.Profile.t
  -> domid
  -> unit
(** destroy a domain *)

val pause : xc:Xenctrl.handle -> domid -> unit
(** Pause a domain *)

val unpause : xc:Xenctrl.handle -> domid -> unit
(** Unpause a domain *)

val set_action_request : xs:Xenstore.Xs.xsh -> domid -> string option -> unit
(** [set_action_request xs domid None] declares this domain is fully intact. Any
    other string is a hint to the toolstack that the domain is still broken. *)

val get_action_request : xs:Xenstore.Xs.xsh -> domid -> string option

val build :
     Xenops_task.Xenops_task.task_handle
  -> xc:Xenctrl.handle
  -> xs:Xenstore.Xs.xsh
  -> store_domid:int
  -> console_domid:int
  -> timeoffset:string
  -> extras:string list
  -> vgpus:Xenops_interface.Vgpu.t list
  -> build_info
  -> string
  -> domid
  -> bool
  -> unit
(** Restore a domain using the info provided *)

val restore :
     Xenops_task.Xenops_task.task_handle
  -> xc:Xenctrl.handle
  -> xs:Xenstore.Xs.xsh
  -> dm:Device.Profile.t
  -> store_domid:int
  -> console_domid:int
  -> no_incr_generationid:bool
  -> timeoffset:string
  -> extras:string list
  -> build_info
  -> manager_path:string
  -> vtpm:Xenops_interface.Vm.tpm option
  -> domid
  -> Unix.file_descr
  -> Unix.file_descr option
  -> unit
(** Restore a domain using the info provided *)

type suspend_flag = Live | Debug

val suspend :
     Xenops_task.Xenops_task.task_handle
  -> xc:Xenctrl.handle
  -> xs:Xenstore.Xs.xsh
  -> domain_type:[`hvm | `pv | `pvh]
  -> is_uefi:bool
  -> dm:Device.Profile.t
  -> manager_path:string
  -> string
  -> domid
  -> Unix.file_descr
  -> Unix.file_descr option
  -> suspend_flag list
  -> ?progress_callback:(float -> unit)
  -> qemu_domid:int
  -> vtpm:Xenops_interface.Vm.tpm option
  -> (unit -> unit)
  -> unit
(** suspend a domain into the file descriptor *)

val send_s3resume : xc:Xenctrl.handle -> domid -> unit
(** send a s3resume event to a domain *)

val vcpu_affinity_set : xc:Xenctrl.handle -> domid -> int -> bool array -> unit
(** Set cpu affinity of some vcpus of a domain using an boolean array *)

val soft_reset : xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> domid -> unit
(** Perform soft reset of a domain *)

val vcpu_affinity_get : xc:Xenctrl.handle -> domid -> int -> bool array
(** Get Cpu affinity of some vcpus of a domain *)

val get_uuid : xc:Xenctrl.handle -> Xenctrl.domid -> [`Vm] Uuidx.t
(** Get the uuid from a specific domain *)

val set_memory_dynamic_range :
  xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> min:int -> max:int -> domid -> unit
(** Write the min,max values of memory/target to xenstore for use by a memory
    policy agent *)

val add_ioport : xc:Xenctrl.handle -> domid -> int -> int -> unit
(** Grant a domain access to a range of IO ports *)

val del_ioport : xc:Xenctrl.handle -> domid -> int -> int -> unit
(** Revoke a domain's access to a range of IO ports *)

val add_iomem : xc:Xenctrl.handle -> domid -> int64 -> int64 -> unit
(** Grant a domain access to a range of IO memory *)

val del_iomem : xc:Xenctrl.handle -> domid -> int64 -> int64 -> unit
(** Revoke a domain's access to a range of IO memory *)

val add_irq : xc:Xenctrl.handle -> domid -> int -> unit
(** Grant a domain access to a physical IRQ *)

val del_irq : xc:Xenctrl.handle -> domid -> int -> unit
(** Revoke a domain's access to a physical IRQ *)

val set_memory_target : xs:Xenstore.Xs.xsh -> Xenstore.Xs.domid -> int64 -> unit

val wait_xen_free_mem :
  xc:Xenctrl.handle -> ?maximum_wait_time_seconds:int -> int64 -> bool

val allowed_xsdata_prefixes : string list

val set_xsdata : xs:Xenstore.Xs.xsh -> domid -> (string * string) list -> unit

val move_xstree : xs:Xenstore.Xs.xsh -> domid -> string -> string -> unit

val numa_init : unit -> unit
