(** High-level domain management functions *)

open Device_common

type domid = Xc.domid

exception Restore_signature_mismatch
exception Domain_build_failed
exception Domain_restore_failed
exception Xenguest_protocol_failure of string (* internal protocol failure *)
exception Xenguest_failure of string (* an actual error is reported to us *)
exception Timeout_backend
exception Could_not_read_file of string (* eg linux kernel/ initrd *)

type build_hvm_info = {
	pae: bool;
	apic: bool;
	acpi: bool;
	nx: bool;
	viridian: bool;
	shadow_multiplier: float;
	timeoffset: string;
}

type build_pv_info = {
	cmdline: string;
	ramdisk: string option;
}

type builder_spec_info = BuildHVM of build_hvm_info | BuildPV of build_pv_info

type build_info = {
	memory_max: int64;    (* memory max in kilobytes *)
	memory_target: int64; (* memory target in kilobytes *)
	kernel: string;       (* in hvm case, point to hvmloader *)
	vcpus: int;           (* vcpus max *)
	priv: builder_spec_info;
}

type domarch = Arch_HVM | Arch_native | Arch_X64 | Arch_X32

val string_of_domarch : domarch -> string
val domarch_of_string : string -> domarch

(** path to the system hvmloader binary *)
val hvmloader : string

(** Create a fresh (empty) domain with a specific UUID, returning the domain ID *)
val make: xc:Xc.handle -> xs:Xs.xsh -> hvm:bool -> ?name:string ->
          ?xsdata:((string * string) list) ->
          ?platformdata:((string * string) list) -> [`domain] Uuid.t -> domid

(** 'types' of shutdown request *)
type shutdown_reason = PowerOff | Reboot | Suspend | Crash | Halt | Unknown of int

(** string versions of the shutdown_reasons, suitable for writing into control/shutdown *)
val string_of_shutdown_reason : shutdown_reason -> string

(** decodes the shutdown_reason contained within the xc dominfo struct *)
val shutdown_reason_of_int : int -> shutdown_reason

(** Immediately force shutdown the domain with reason 'shutdown_reason' *)
val hard_shutdown: xc:Xc.handle -> domid -> shutdown_reason -> unit

(** Tell the domain to shutdown with reason 'shutdown_reason'. Don't wait for an ack *)
val shutdown: xs:Xs.xsh -> domid -> shutdown_reason -> unit

(** Tell the domain to shutdown with reason ''shutdown_reason', waiting for an ack *)
val shutdown_ack: ?timeout:float -> xc:Xc.handle -> xs:Xs.xsh -> domid -> shutdown_reason -> bool

(** send a domain a sysrq *)
val sysrq: xs:Xs.xsh -> domid -> char -> unit

(** Forcibly close all VBD backends and wait for them to indicate they've flushed
    (only used by the migrate code) *)
val hard_shutdown_all_vbds: xc:Xc.handle -> xs:Xs.xsh -> ?extra_debug_paths:string list -> device list -> unit

(** destroy a domain *)
val destroy: ?preserve_xs_vm : bool -> xc: Xc.handle -> xs:Xs.xsh -> domid -> unit

(** Pause a domain *)
val pause: xc: Xc.handle -> domid -> unit

(** Unpause a domain *)
val unpause: xc: Xc.handle -> domid -> unit

(* val create_channels : xc:Xc.handle -> domid -> int * int *)

(** Builds a linux guest in a fresh domain created with 'make' *)
val build_linux: xc: Xc.handle -> xs: Xs.xsh -> mem_max_kib:Int64.t
              -> mem_target_kib:Int64.t -> kernel:string -> cmdline:string
              -> ramdisk:string option -> vcpus:int -> domid
              -> domarch

(** build an hvm domain in a fresh domain created with 'make' *)
val build_hvm: xc: Xc.handle -> xs: Xs.xsh -> mem_max_kib:Int64.t
            -> mem_target_kib:Int64.t -> shadow_multiplier:float
            -> vcpus:int -> kernel:string
            -> pae:bool -> apic:bool -> acpi:bool -> nx:bool -> viridian:bool
            -> timeoffset:string -> domid
            -> domarch

(** Restore a domain using the info provided *)
val build: xc: Xc.handle -> xs: Xs.xsh -> build_info -> domid -> domarch

(** resume a domain either cooperative or not *)
val resume: xc: Xc.handle -> xs: Xs.xsh -> hvm: bool -> cooperative: bool -> domid -> unit

(** restore a PV domain into a fresh domain created with 'make' *)
val restore: xc: Xc.handle -> xs: Xs.xsh -> mem_max_kib:Int64.t 
          -> mem_target_kib:Int64.t -> vcpus:int -> domid -> Unix.file_descr
          -> unit

(** restore an HVM domain from the file descriptor into a fresh domain created
 *  with 'make' *)
val hvm_restore: xc: Xc.handle -> xs: Xs.xsh -> mem_max_kib:Int64.t
             -> mem_target_kib:Int64.t -> shadow_multiplier:float
             -> vcpus:int -> pae:bool -> viridian:bool -> timeoffset:string
             -> domid -> Unix.file_descr
             -> unit

type suspend_flag = Live | Debug

(** suspend a domain into the file descriptor *)
val suspend: xc: Xc.handle -> xs: Xs.xsh -> hvm: bool -> domid
          -> Unix.file_descr -> suspend_flag list
          -> ?progress_callback: (float -> unit)
          -> (unit -> unit) -> unit

(** send a s3resume event to a domain *)
val send_s3resume: xc: Xc.handle -> domid -> unit

(** Set cpu affinity of some vcpus of a domain using an boolean array *)
val vcpu_affinity_set: xc: Xc.handle -> domid -> int -> bool array -> unit

(** Get Cpu affinity of some vcpus of a domain *)
val vcpu_affinity_get: xc: Xc.handle -> domid -> int -> bool array

(** Get the uuid from a specific domain *)
val get_uuid: xc: Xc.handle -> Xc.domid -> [`domain] Uuid.t

(** Write values (host,port,session,vmref) to grant API access to domain domid *)
val grant_api_access: xs: Xs.xsh -> domid -> string
                   -> string -> int -> string -> string -> unit

(** Get the API parameters that has been granted by domain 0 in the specified domain *)
val get_api_access: xs: Xs.xsh -> domid -> string * int * string * string

(** Get the API parameters that has been granted by domain 0 in domain U *)
val guest_get_api_access: xs: Xs.xsh -> string * int * string * string

(** Grant a domain access to a range of IO ports *)
val add_ioport: xc: Xc.handle -> domid -> int -> int -> unit

(** Revoke a domain's access to a range of IO ports *)
val del_ioport: xc: Xc.handle -> domid -> int -> int -> unit

(** Grant a domain access to a range of IO memory *)
val add_iomem: xc: Xc.handle -> domid -> int64 -> int64 -> unit

(** Revoke a domain's access to a range of IO memory *)
val del_iomem: xc: Xc.handle -> domid -> int64 -> int64 -> unit

(** Grant a domain access to a physical IRQ *)
val add_irq: xc: Xc.handle -> domid -> int -> unit

(** Revoke a domain's access to a physical IRQ *)
val del_irq: xc: Xc.handle -> domid -> int -> unit

(** Restrict a domain to a maximum machine address width *)
val set_machine_address_size: xc: Xc.handle -> domid -> int option -> unit

(** Suppress spurious page faults for this domain *)
val suppress_spurious_page_faults: xc: Xc.handle -> domid -> unit

(** CPUID related functions *)
type cpuid_reg = Eax | Ebx | Ecx | Edx
type cpuid_rtype = Clear | Set | Default | Same | Keep

type cpuid_config = ((int64 * int64 option) * ((cpuid_reg * (cpuid_rtype array)) list)) list

exception Cpuid_unknown_type of char

val cpuid_reg_of_string : string -> cpuid_reg
val cpuid_rtype_of_char : char -> cpuid_rtype

val cpuid_set : xc: Xc.handle -> hvm: bool -> domid -> cpuid_config -> cpuid_config
val cpuid_apply : xc: Xc.handle -> hvm: bool -> domid -> unit
val cpuid_check : cpuid_config -> (bool * ((int64 * int64 option) * (cpuid_reg * cpuid_rtype array) list)) list
