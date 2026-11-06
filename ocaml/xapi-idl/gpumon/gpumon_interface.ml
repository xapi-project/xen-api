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

open Idl

module D = Debug.Make (struct let name = "gpumon_interface" end)

open D

let service_name = "gpumon"

let queue_name = Xcp_service.common_prefix ^ service_name

let xml_path = "/var/xapi/" ^ service_name

(** Uninterpreted string associated with the operation *)
type debug_info = string [@@deriving rpcty]

(* Domain ID of VM *)
type domid = int [@@deriving rpcty]

(** UUID of Nvidia Virtual GPU *)
type vgpu_uuid = string [@@deriving rpcty]

(** Reason for incompatibility *)
type incompatibility_reason = Host_driver | Guest_driver | GPU | Other
[@@deriving rpcty]

(** Compatibility between virtual and physical GPU *)
type compatibility = Compatible | Incompatible of incompatibility_reason list
[@@deriving rpcty]

(** PCI identifier of physical GPU *)
type pgpu_address = string [@@deriving rpcty]

(** Metadata of Nvidia physical GPU *)
type nvidia_pgpu_metadata = string [@@deriving rpcty]

(** Metadata of Nvidia virtual GPU *)
type nvidia_vgpu_metadata = string [@@deriving rpcty]

(** List of Nvidia virtual GPU metadata records *)
type nvidia_vgpu_metadata_list = nvidia_vgpu_metadata list [@@deriving rpcty]

(** Error wrapper *)
type gpu_errors =
  | NvmlInterfaceNotAvailable
      (** Exception raised when gpumon is unable to load the nvml nvidia library *)
  | NvmlFailure of string
      (** Exception raised by the c bindings to the nvml nvidia library*)
  | Internal_error of string
      (** Exception raised if an unexpected error is triggered by the library *)
  | Gpumon_failure  (** Default exception raised upon daemon failure *)
[@@default Gpumon_failure] [@@deriving rpcty]

exception Gpumon_error of gpu_errors

let () =
  (* register printer *)
  let sprintf = Printf.sprintf in
  let string_of_error e =
    Rpcmarshal.marshal gpu_errors.Rpc.Types.ty e |> Rpc.to_string
  in
  let printer = function
    | Gpumon_error e ->
        Some (sprintf "Gpumon_interface.Gpumon_error(%s)" (string_of_error e))
    | _ ->
        None
  in
  Printexc.register_printer printer

(** Error handler *)
let gpu_err =
  Error.
    {
      def= gpu_errors
    ; raiser=
        (fun e ->
          let exn = Gpumon_error e in
          error "%s (%s)" (Printexc.to_string exn) __LOC__ ;
          raise exn
        )
    ; matcher=
        (function
        | Gpumon_error e as exn ->
            error "%s (%s)" (Printexc.to_string exn) __LOC__ ;
            Some e
        | exn ->
            error "%s (%s)" (Printexc.to_string exn) __LOC__ ;
            Some (Internal_error (Printexc.to_string exn))
        )
    }

(** Functor to autogenerate API calls *)
module RPC_API (R : RPC) = struct
  open R

  let param = Param.mk

  let description =
    Interface.
      {
        name= "Gpumon"
      ; namespace= None
      ; description=
          [
            "This interface is used by Xapi and Gpumon to monitor "
          ; "physical and virtual GPUs."
          ]
      ; version= (1, 0, 0)
      }

  let implementation = implement description

  (** Compatibility checking interface for Nvidia vGPUs *)
  module Nvidia = struct
    (** common API call parameters *)

    let unit_p = param Rpc.Types.unit

    let bool_p = param Rpc.Types.bool

    let debug_info_p =
      param ~description:["Uninterpreted string used for debugging."] debug_info

    let domid_p =
      param
        ~description:["Domain ID of the VM in which the vGPU(s) is running."]
        domid

    let vgpu_uuid_p =
      param ~description:["UUID of Nvidia virtual GPU."] vgpu_uuid

    let pgpu_address_p =
      param
        ~description:
          [
            "PCI bus ID of the pGPU in which the VM is currently running"
          ; "in the form `domain:bus:device.function` PCI identifier."
          ]
        pgpu_address

    let nvidia_pgpu_metadata_p =
      param
        ~description:["Metadata of Nvidia physical GPU."]
        nvidia_pgpu_metadata

    let nvidia_vgpu_metadata_p =
      param
        ~description:["Metadata of Nvidia virtual GPU."]
        nvidia_vgpu_metadata

    let nvidia_vgpu_metadata_list_p =
      param
        ~description:["Metadata list of Nvidia virtual GPU."]
        nvidia_vgpu_metadata_list

    let compatibility_p =
      param
        ~description:
          [
            "Value indicating whether two or more GPUs are compatible with \
             each other."
          ]
        compatibility

    let get_pgpu_metadata =
      declare "get_pgpu_metadata"
        ["Gets the metadata for a pGPU, given its address (PCI bus ID)."]
        (debug_info_p
        @-> pgpu_address_p
        @-> returning nvidia_pgpu_metadata_p gpu_err
        )

    let get_pgpu_vm_compatibility =
      declare "get_pgpu_vm_compatibility"
        ["Checks compatibility between a VM's vGPU(s) and another pGPU."]
        (debug_info_p
        @-> pgpu_address_p
        @-> domid_p
        @-> nvidia_pgpu_metadata_p
        @-> returning compatibility_p gpu_err
        )

    let get_vgpu_metadata =
      declare "get_vgpu_metadata"
        ["Obtains metadata for all vGPUs running in a domain."]
        (debug_info_p
        @-> domid_p
        @-> pgpu_address_p
        @-> vgpu_uuid_p
        @-> returning nvidia_vgpu_metadata_list_p gpu_err
        )

    let get_pgpu_vgpu_compatibility =
      declare "get_pgpu_vgpu_compatibility"
        [
          "Checks compatibility between a pGPU (on a host) and a list of vGPUs "
        ; "(assigned to a VM). Note: A VM may use several vGPUs."
        ; "The use case is VM.suspend/VM.resume:"
        ; "before VM.resume [nvidia_vgpu_metadata] of the suspended VM is "
        ; "checked against the [nvidia_pgpu_metadata] on the host where the VM "
        ; "is resumed."
        ]
        (debug_info_p
        @-> nvidia_pgpu_metadata_p
        @-> nvidia_vgpu_metadata_list_p
        @-> returning compatibility_p gpu_err
        )

    let nvml_attach =
      declare "nvml_attach"
        [
          "Attach nVidia cards to Gpumon for metrics and compatibility checking."
        ]
        (debug_info_p @-> returning unit_p gpu_err)

    let nvml_detach =
      declare "nvml_detach"
        ["Detach nVidia cards from Gpumon"]
        (debug_info_p @-> returning unit_p gpu_err)

    let nvml_is_attached =
      declare "nvml_is_attached"
        ["Return true if nVidia cards are currently attached."]
        (debug_info_p @-> returning bool_p gpu_err)
  end
end

(* The reply of the GPU partition helper, which xapi runs and reads.
   Each member is named as on the wire; the helper builds every reply. A later
   minor version only adds members and values, which this reader keeps or
   skips. *)

(* How a member of the reply is read and written *)
module Member = struct
  (* rpclib reads a variant in any case; the wire spells a value only as its
     constructor's name in lower case, so any other string is kept as
     Unknown. *)
  type 'a open_enum = Known of 'a | Unknown of string

  let typ_of_open_enum known =
    let ( let* ) = Result.bind in
    let name v =
      Rpcmarshal.marshal known v |> Rpc.lowerfn |> Rpc.string_of_rpc
    in
    let of_rpc rpc =
      let* s = Rpcmarshal.unmarshal Rpc.Types.(Basic String) rpc in
      match Rpcmarshal.unmarshal known rpc with
      | Ok v when name v = s ->
          Ok (Known v)
      | Ok _ | Error _ ->
          Ok (Unknown s)
    in
    let rpc_of e =
      Rpc.String (match e with Known v -> name v | Unknown s -> s)
    in
    Rpc.Types.Abstract {aname= "open_enum"; test_data= []; rpc_of; of_rpc}

  (* A required member that may have no value is null, which rpclib's option
     would refuse. *)
  type 'a nullable = 'a option

  let typ_of_nullable typ =
    let of_rpc = function
      | Rpc.Null ->
          Ok None
      | rpc ->
          Rpcmarshal.unmarshal typ rpc |> Result.map Option.some
    in
    let rpc_of = Option.fold ~none:Rpc.Null ~some:(Rpcmarshal.marshal typ) in
    Rpc.Types.Abstract {aname= "nullable"; test_data= []; rpc_of; of_rpc}

  (* A member that cannot be read costs only itself, not the reply. It is
     written as an empty object. *)
  type 'a lenient = ('a, [`Msg of string]) result

  let typ_of_lenient typ =
    let of_rpc rpc = Ok (Rpcmarshal.unmarshal typ rpc) in
    let rpc_of =
      Result.fold ~ok:(Rpcmarshal.marshal typ) ~error:(fun _ -> Rpc.Dict [])
    in
    Rpc.Types.Abstract {aname= "lenient"; test_data= []; rpc_of; of_rpc}

  (* rpclib refuses a record whose member is absent before the member's typ
     runs, so each lenient member defaults to this. *)
  let absent = Error (`Msg "absent")

  (* rpclib would also read an integer from a string, or wrap one out of
     range, which the reply's format does not allow. *)
  type integer = int

  let typ_of_integer =
    let of_rpc = function
      | Rpc.Int i when Int64.(equal (of_int (to_int i)) i) ->
          Ok (Int64.to_int i)
      | rpc ->
          Error (`Msg ("expected an integer, got " ^ Rpc.to_string rpc))
    in
    let rpc_of i = Rpc.Int (Int64.of_int i) in
    Rpc.Types.Abstract {aname= "integer"; test_data= []; rpc_of; of_rpc}

  (* rpcty ignores a type's arguments, so each nullable or lenient member type
     is named. *)
  type nullable_int = integer nullable

  let typ_of_nullable_int = typ_of_nullable typ_of_integer

  type nullable_string = string nullable

  let typ_of_nullable_string = typ_of_nullable Rpc.Types.(Basic String)

  type nullable_bool = bool nullable

  let typ_of_nullable_bool = typ_of_nullable Rpc.Types.(Basic Bool)

  type lenient_int = integer lenient

  let typ_of_lenient_int = typ_of_lenient typ_of_integer

  type lenient_string = string lenient

  let typ_of_lenient_string = typ_of_lenient Rpc.Types.(Basic String)

  type lenient_string_list = string list lenient

  let typ_of_lenient_string_list = typ_of_lenient Rpc.Types.(List (Basic String))

  type lenient_nullable_string = nullable_string lenient

  let typ_of_lenient_nullable_string = typ_of_lenient typ_of_nullable_string
end

open Member

(* The kinds a probe reply can carry. A write's kinds and the caller's own
   reasons are not among them, so they read as Unknown. *)
module Kind = struct
  type known =
    | Not_supported
    | Invalid_state
    | Nvml_error
    | Not_found
    | Profile_catalogue_empty
    | Profile_catalogue_incomplete
    | Withdrawn
    | Device_not_found
    | Gpu_lost
    | Reset_required
    | Device_unhealthy
    | Usage
    | Schema_unsupported
    | Nvml_library_absent
    | Nvml_symbol_absent
    | Nvml_init_failed
    | Driver_not_loaded
    | Driver_library_mismatch
    | Nvml_too_old
    | Internal
    | Lock_timeout
    | In_use
    | Not_ready
    | Timeout
    | Mode_changed
    | No_permission
    | Lock_unsafe
    | Gate_key_not_on
    | Mode_unknown
  [@@deriving rpcty]

  type t = known open_enum

  let typ_of = typ_of_open_enum typ_of_known

  type nonrec nullable = t nullable

  let typ_of_nullable = typ_of_nullable typ_of

  type lenient_nullable = nullable lenient

  let typ_of_lenient_nullable = typ_of_lenient typ_of_nullable
end

module Section_status = struct
  type known = Ok | Error | Not_attempted [@@deriving rpcty]

  type t = known open_enum

  let typ_of = typ_of_open_enum typ_of_known
end

(* The status of a name read from the card *)
module String_status = struct
  type known = Invalid [@@deriving rpcty]

  type t = known open_enum

  let typ_of = typ_of_open_enum typ_of_known

  type nonrec lenient = t lenient

  let typ_of_lenient = typ_of_lenient typ_of
end

(* Types of the members every reply carries, whatever its command; device
   and lock have their own modules *)
module Envelope = struct
  module Status = struct
    type known = Ok | Partial | Error [@@deriving rpcty]

    type t = known open_enum

    let typ_of = typ_of_open_enum typ_of_known
  end

  module Backend = struct
    type known = Nvml | Nvidia_smi [@name "nvidia-smi"] | Other
    [@@deriving rpcty]

    type t = known open_enum

    let typ_of = typ_of_open_enum typ_of_known
  end

  type implementation = {name: string; version: string; backend: Backend.t}
  [@@deriving rpcty]

  type lenient_implementation = implementation lenient

  let typ_of_lenient_implementation = typ_of_lenient typ_of_implementation

  type page = {index: integer; count: integer; size: integer; encoding: string}
  [@@deriving rpcty]

  type nvml_call = {
      function_: nullable_string [@key "function"]
    ; code: nullable_int
    ; name: nullable_string
    ; text: nullable_string
  }
  [@@deriving rpcty]

  type lenient_nullable_nvml_call = nvml_call nullable lenient

  let typ_of_lenient_nullable_nvml_call =
    typ_of_lenient (typ_of_nullable typ_of_nvml_call)

  type error = {
      kind: Kind.t
    ; message: string
    ; nvml: lenient_nullable_nvml_call [@default absent]
    ; symbol: lenient_nullable_string [@default absent]
    ; schema_majors: integer list option
  }
  [@@deriving rpcty]

  type nullable_error = error nullable

  let typ_of_nullable_error = typ_of_nullable typ_of_error

  type nvml = {library: string; nvml_version: string; driver_version: string}
  [@@deriving rpcty]

  type lenient_nullable_nvml = nvml nullable lenient

  let typ_of_lenient_nullable_nvml = typ_of_lenient (typ_of_nullable typ_of_nvml)
end

module Device = struct
  module Architecture = struct
    type known =
      | Kepler
      | Maxwell
      | Pascal
      | Volta
      | Turing
      | Ampere
      | Ada
      | Hopper
      | Blackwell
      | Rubin
      | Unknown
    [@@deriving rpcty]

    type t = known open_enum

    let typ_of = typ_of_open_enum typ_of_known

    type nonrec lenient = t lenient

    let typ_of_lenient = typ_of_lenient typ_of
  end

  type t = {
      bdf: string
    ; nvml_bus_id: lenient_string [@default absent]
    ; pci_device_id: lenient_nullable_string [@default absent]
    ; pci_subsystem_id: lenient_nullable_string [@default absent]
    ; name: lenient_string [@default absent]
    ; name_status: String_status.lenient option
    ; architecture: Architecture.lenient [@default absent]
    ; architecture_raw: lenient_int [@default absent]
  }
  [@@deriving rpcty]

  type nonrec nullable = t nullable

  let typ_of_nullable = typ_of_nullable typ_of
end

(* The host lock the helper took or waited for, and who holds it *)
module Lock = struct
  module Mode = struct
    type known = Shared | Exclusive [@@deriving rpcty]

    type t = known open_enum

    let typ_of = typ_of_open_enum typ_of_known
  end

  type holder = {
      pid: integer
    ; name: string
    ; state: string
    ; mode: Mode.t
    ; stuck_threads: integer list option
  }
  [@@deriving rpcty]

  type t = {
      path: string
    ; inode: nullable_int
    ; waited_ms: integer
    ; mode: Mode.t option
    ; holders: holder list option
  }
  [@@deriving rpcty]

  type lenient_nullable = t nullable lenient

  let typ_of_lenient_nullable = typ_of_lenient (typ_of_nullable typ_of)
end

module Mig_mode = struct
  (* Known Mode.Unknown is NVML's answer "unknown"; Unknown s is a value this
     reader does not know. *)
  module Mode = struct
    type known = Enabled | Disabled | Unknown [@@deriving rpcty]

    type t = known open_enum

    let typ_of = typ_of_open_enum typ_of_known

    type nonrec nullable = t nullable

    let typ_of_nullable = typ_of_nullable typ_of
  end

  type t = {
      status: Section_status.t
    ; kind: Kind.nullable
    ; capable: nullable_bool
    ; current: Mode.nullable
    ; pending: Mode.nullable
    ; current_raw: nullable_int
    ; pending_raw: nullable_int
  }
  [@@deriving rpcty]
end

(* The card's GPU and compute instances, and its catalogue of profiles *)
module Geometry = struct
  module Profiles_status = struct
    type known = Ok | Mig_disabled | Error | Not_attempted [@@deriving rpcty]

    type t = known open_enum

    let typ_of = typ_of_open_enum typ_of_known
  end

  type placement = {start: integer; size: integer} [@@deriving rpcty]

  type compute_instance = {
      ci_id: integer
    ; profile_id: integer
    ; placement: placement
  }
  [@@deriving rpcty]

  type gpu_instance = {
      gi_id: integer
    ; profile_id: integer
    ; profile_name: string
    ; name_status: String_status.t option
    ; placement: placement
    ; compute_instances: compute_instance list
  }
  [@@deriving rpcty]

  type signature = {
      slice_count: nullable_int
    ; multiprocessor_count: nullable_int
    ; copy_engine_count: nullable_int
    ; decoder_count: nullable_int
    ; encoder_count: nullable_int
    ; jpeg_count: nullable_int
    ; ofa_count: nullable_int
    ; memory_size_mb: nullable_int
    ; capabilities: nullable_int
  }
  [@@deriving rpcty]

  type nullable_signature = signature nullable

  let typ_of_nullable_signature = typ_of_nullable typ_of_signature

  type profile = {
      profile_id: integer
    ; name: string
    ; name_status: String_status.t option
    ; instance_count: integer
    ; remaining: nullable_int
    ; placements: placement list
    ; signature: nullable_signature
  }
  [@@deriving rpcty]

  type profile_index_bound = {gi: integer; ci: integer} [@@deriving rpcty]

  type type_profile = {
      type_id: integer
    ; gi_profile_id: nullable_int
    ; max_instance_per_gi: nullable_int option
  }
  [@@deriving rpcty]

  type nullable_gpu_instance_list = gpu_instance list nullable

  let typ_of_nullable_gpu_instance_list =
    typ_of_nullable (Rpc.Types.List typ_of_gpu_instance)

  type lenient_nullable_profile_list = profile list nullable lenient

  let typ_of_lenient_nullable_profile_list =
    typ_of_lenient (typ_of_nullable (Rpc.Types.List typ_of_profile))

  type lenient_nullable_profile_index_bound =
    profile_index_bound nullable lenient

  let typ_of_lenient_nullable_profile_index_bound =
    typ_of_lenient (typ_of_nullable typ_of_profile_index_bound)

  type lenient_nullable_type_profile_list = type_profile list nullable lenient

  let typ_of_lenient_nullable_type_profile_list =
    typ_of_lenient (typ_of_nullable (Rpc.Types.List typ_of_type_profile))

  type t = {
      status: Section_status.t
    ; kind: Kind.lenient_nullable [@default absent]
    ; gpu_instances: nullable_gpu_instance_list
    ; profiles_status: Profiles_status.t
    ; profiles: lenient_nullable_profile_list [@default absent]
    ; profile_index_bound: lenient_nullable_profile_index_bound
          [@default absent]
    ; type_profiles: lenient_nullable_type_profile_list [@default absent]
  }
  [@@deriving rpcty]
end

module Vgpu = struct
  type entry = {uuid: string; gi_id: nullable_int; type_id: integer}
  [@@deriving rpcty]

  type nullable_entry_list = entry list nullable

  let typ_of_nullable_entry_list = typ_of_nullable (Rpc.Types.List typ_of_entry)

  type t = {
      status: Section_status.t
    ; kind: Kind.nullable
    ; vgpus: nullable_entry_list
  }
  [@@deriving rpcty]

  type nonrec lenient = t lenient

  let typ_of_lenient = typ_of_lenient typ_of
end

(* The reply to a probe of one card: its sections, and the case it found *)
module Probe = struct
  (* Not Result, which rpcty's code for a [@default] member refers to *)
  module Case = struct
    type known = Partitions | None_carved | Unreadable | Not_applicable
    [@@deriving rpcty]

    type t = known open_enum

    let typ_of = typ_of_open_enum typ_of_known
  end

  type t = {result: Case.t; reason: Kind.nullable} [@@deriving rpcty]

  (* A usage error has no probe; the reader of the reply decides which of
     these members a status requires. *)
  type data = {
      mig_mode: Mig_mode.t option
    ; geometry: Geometry.t option
    ; vgpu: Vgpu.lenient option
    ; probe: t option
  }
  [@@deriving rpcty]

  (* Another major version's data need not fit these types. *)
  type lenient_data = data lenient

  let typ_of_lenient_data = typ_of_lenient typ_of_data

  type reply = {
      schema: string
    ; schema_version: integer * integer
    ; page: Envelope.page option
    ; implementation: Envelope.lenient_implementation [@default absent]
    ; command: lenient_string_list [@default absent]
    ; status: Envelope.Status.t
    ; error: Envelope.nullable_error
    ; nvml: Envelope.lenient_nullable_nvml [@default absent]
    ; device: Device.nullable
    ; lock: Lock.lenient_nullable [@default absent]
    ; data: lenient_data
  }
  [@@deriving rpcty]
end

(* The reply to --version: the probe reply's envelope with its own data *)
module Version = struct
  module Verb_status = struct
    type known = Supported | Deprecated | Withdrawn [@@deriving rpcty]

    type t = known open_enum

    let typ_of = typ_of_open_enum typ_of_known
  end

  (* The reader of --version decides what a non-positive maximum means. *)
  type verb = {
      command: string
    ; status: Verb_status.t
    ; max_duration_ms: integer option
  }
  [@@deriving rpcty]

  type data = {
      schema_versions: (integer * integer) list
    ; verbs: verb list
    ; page_encodings: string list
  }
  [@@deriving rpcty]

  type lenient_data = data lenient

  let typ_of_lenient_data = typ_of_lenient typ_of_data

  type reply = {
      schema: string
    ; schema_version: integer * integer
    ; page: Envelope.page option
    ; implementation: Envelope.lenient_implementation [@default absent]
    ; command: lenient_string_list [@default absent]
    ; status: Envelope.Status.t
    ; error: Envelope.nullable_error
    ; nvml: Envelope.lenient_nullable_nvml [@default absent]
    ; device: Device.nullable
    ; lock: Lock.lenient_nullable [@default absent]
    ; data: lenient_data
  }
  [@@deriving rpcty]
end
