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

(* The replies of the GPU partition helper, a program that reports how a GPU
   is divided into partitions. The helper writes them and its readers read
   them with these types, so these types define the replies' JSON. A later
   minor version only adds members and values, which a reader keeps or skips. *)
module Partition_helper = struct
  (* The types of the replies of schema version 1.0. Once 1.0 is released,
     none of its types changes, nor how a reply or a helper argument reads or
     is written *)
  module V1_0 = struct
    (* The version these shapes are written in: the draft 0.0 until 1.0 is
       released, then 1.0 *)
    let schema_version = (0, 0)

    (* How a member is written and read, by the form of its type:
       - plain: always sent with a value.
         Example: "waited_ms": 0
       - nullable: always sent, and null when it has no value.
         Example: "inode": null
       - option: left out when it has no value.
         Example: a lock without "holders"
       - nullable option: left out, or sent as null or as a value.
         Example: "max_instance_per_gi" left out, null or 1
       A member sent otherwise, or a value its type cannot read, is a fault:
       no type that declares the member can read the reply, unless the
       member's comment says otherwise. So a reader reads Envelope.Core first,
       then views of its own, one for each group of members it can do without:
       records that declare only the members it uses, or records of a snapshot
       reused whole. Each view names one snapshot; only a writer names the
       newest, through Partition_helper. A minor version that adds a member or
       an enumeration value restates that type, and every type that contains it,
       in the next snapshot. *)
    module Member = struct
      (* A value of an enumeration: Known when it has the exact spelling the
         writer uses (the constructor's name or its [@name], in lower case),
         and Unknown with any other string, as rpclib reads a variant in any
         case. *)
      type 'a open_enum = Known of 'a | Unknown of string

      let typ_of_open_enum known =
        let name v =
          Rpcmarshal.marshal known v |> Rpc.lowerfn |> Rpc.string_of_rpc
        in
        let of_rpc rpc =
          Rpcmarshal.unmarshal Rpc.Types.(Basic String) rpc
          |> Result.map (fun s ->
              match Rpcmarshal.unmarshal known rpc with
              | Ok v when name v = s ->
                  Known v
              | Ok _ | Error _ ->
                  Unknown s
          )
        in
        let rpc_of e =
          Rpc.String (match e with Known v -> name v | Unknown s -> s)
        in
        Rpc.Types.Abstract {aname= "open_enum"; test_data= []; rpc_of; of_rpc}

      (* The value of a member that is always sent: Null, written null, when
         it has no value, else Value. Its own constructors keep it apart from
         option's None, a member left out. *)
      type 'a nullable = Null | Value of 'a

      let typ_of_nullable typ =
        let of_rpc = function
          | Rpc.Null ->
              Ok Null
          | rpc ->
              Rpcmarshal.unmarshal typ rpc |> Result.map (fun v -> Value v)
        in
        let rpc_of = function
          | Null ->
              Rpc.Null
          | Value v ->
              Rpcmarshal.marshal typ v
        in
        Rpc.Types.Abstract {aname= "nullable"; test_data= []; rpc_of; of_rpc}

      (* A JSON integer within the range of int. A string, or an integer out of
         range, which rpclib's own int reader would read, is refused. *)
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

      (* The nullable types of members, each with a name of its own: rpcty
         gives a member of type t the typ typ_of_t, whatever t's arguments *)
      type nullable_int = integer nullable

      let typ_of_nullable_int = typ_of_nullable typ_of_integer

      type nullable_string = string nullable

      let typ_of_nullable_string = typ_of_nullable Rpc.Types.(Basic String)

      type nullable_bool = bool nullable

      let typ_of_nullable_bool = typ_of_nullable Rpc.Types.(Basic Bool)
    end

    open Member

    (* The kinds a probe or --version reply can carry. A reader's own reasons,
       and the kinds of later commands that change the GPU, are not among them
       and read as Unknown; a later command's kinds get their own type. *)
    module Kind = struct
      type known =
        | Not_supported
        | Invalid_state
        | Library_error
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
        | Library_absent
        | Library_symbol_absent
        | Library_init_failed
        | Driver_not_loaded
        | Driver_library_mismatch
        | Library_too_old
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
    end

    module Device = struct
      module Architecture = struct
        (* Known Architecture.Unknown is the helper's answer "unknown";
           Unknown s is a value these types do not know, kept as s. *)
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
      end

      (* The GPU the reply is about; bdf is its PCI address *)
      type t = {
          bdf: string
        ; library_bus_id: string
        ; pci_device_id: nullable_string
        ; pci_subsystem_id: nullable_string
        ; name: string
        ; name_status: String_status.t option
        ; architecture: Architecture.t
        ; architecture_raw: integer
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

      type nonrec nullable = t nullable

      let typ_of_nullable = typ_of_nullable typ_of
    end

    (* Types of the members every reply carries, whatever its command *)
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

      type page = {
          index: integer
        ; count: integer
        ; size: integer
        ; encoding: string
      }
      [@@deriving rpcty]

      type library_call = {
          function_: nullable_string [@key "function"]
        ; code: nullable_int
        ; name: nullable_string
        ; text: nullable_string
      }
      [@@deriving rpcty]

      type nullable_library_call = library_call nullable

      let typ_of_nullable_library_call = typ_of_nullable typ_of_library_call

      (* The helper's own error; it sends null when the status is "ok", which
         these types do not check. *)
      type error = {
          kind: Kind.t
        ; message: string
        ; library_call: nullable_library_call
        ; symbol: nullable_string
        ; schema_majors: integer list option
      }
      [@@deriving rpcty]

      type nullable_error = error nullable

      let typ_of_nullable_error = typ_of_nullable typ_of_error

      type library = {name: string; version: string; driver_version: string}
      [@@deriving rpcty]

      type nullable_library = library nullable

      let typ_of_nullable_library = typ_of_nullable typ_of_library

      (* The members of a page in the identity encoding, and of its error,
         that stay the same in every major version and for every command:
         what every reader reads first. *)
      module Core = struct
        type error = {
            kind: Kind.t
          ; message: string
          ; schema_majors: integer list option
        }
        [@@deriving rpcty]

        type nullable_error = error nullable

        let typ_of_nullable_error = typ_of_nullable typ_of_error

        type t = {
            schema: string
          ; schema_version: integer * integer
          ; page: page option
          ; status: Status.t
          ; error: nullable_error
        }
        [@@deriving rpcty]
      end

      (* The reply as the helper writes it. A page after the first carries only
         Envelope.Core's members, the device's bdf and the slices it holds of
         the split lists. Read with this type, a reply is refused at its first
         fault, and its major version is not checked: a reader reads Core first,
         checks schema_version, then its own views. *)
      module Reply (Data : sig
        type t

        val typ_of : t Rpc.Types.typ
      end) =
      struct
        type reply = {
            schema: string
          ; schema_version: integer * integer
          ; page: page option
          ; implementation: implementation
          ; command: string list
          ; status: Status.t
          ; error: nullable_error
          ; library: nullable_library
          ; device: Device.nullable
          ; lock: Lock.nullable
          ; data: Data.t
        }
        [@@deriving rpcty]
      end
    end

    module Mig_mode = struct
      module Mode = struct
        (* Known Mode.Unknown is the helper's answer "unknown";
           Unknown s is a value these types do not know, kept as s. *)
        type known = Enabled | Disabled | Unknown [@@deriving rpcty]

        type t = known open_enum

        let typ_of = typ_of_open_enum typ_of_known

        type nonrec nullable = t nullable

        let typ_of_nullable = typ_of_nullable typ_of
      end

      (* Whether the card can use MIG, and its MIG mode now and after its next
         reset, by name and as the driver's raw value *)
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
        type known = Ok | Mig_disabled | Error | Not_attempted
        [@@deriving rpcty]

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

      (* A GPU instance of the card, with its compute instances *)
      type gpu_instance = {
          gi_id: integer
        ; profile_id: integer
        ; profile_name: string
        ; name_status: String_status.t option
        ; placement: placement
        ; compute_instances: compute_instance list
      }
      [@@deriving rpcty]

      (* The resource counts, memory size and capabilities the driver reports
         for a profile. They can tell profiles apart beyond their names and ids,
         and show when a driver renumbers them. A member the driver does not
         report is null, and so is the whole signature when the helper cannot
         read it. *)
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

      (* max_instance_per_gi: absent when the helper did not read it; null when
         the type is not MIG-backed or the driver could not report it; else the
         most vGPUs of this type that one GPU instance can host. A value that is
         not an integer is a fault; a reader that keeps the other entries
         declares this member with [@default None] *)
      type type_profile = {
          type_id: integer
        ; gi_profile_id: nullable_int
        ; max_instance_per_gi: nullable_int option
      }
      [@@deriving rpcty]

      type nullable_gpu_instance_list = gpu_instance list nullable

      let typ_of_nullable_gpu_instance_list =
        typ_of_nullable (Rpc.Types.List typ_of_gpu_instance)

      type nullable_profile_list = profile list nullable

      let typ_of_nullable_profile_list =
        typ_of_nullable (Rpc.Types.List typ_of_profile)

      type nullable_profile_index_bound = profile_index_bound nullable

      let typ_of_nullable_profile_index_bound =
        typ_of_nullable typ_of_profile_index_bound

      type nullable_type_profile_list = type_profile list nullable

      let typ_of_nullable_type_profile_list =
        typ_of_nullable (Rpc.Types.List typ_of_type_profile)

      (* kind only says why the section failed *)
      type t = {
          status: Section_status.t
        ; kind: Kind.nullable
        ; gpu_instances: nullable_gpu_instance_list
        ; profiles_status: Profiles_status.t
        ; profiles: nullable_profile_list
        ; profile_index_bound: nullable_profile_index_bound
        ; type_profiles: nullable_type_profile_list
      }
      [@@deriving rpcty]
    end

    module Vgpu = struct
      type entry = {uuid: string; gi_id: nullable_int; type_id: integer}
      [@@deriving rpcty]

      type nullable_entry_list = entry list nullable

      let typ_of_nullable_entry_list =
        typ_of_nullable (Rpc.Types.List typ_of_entry)

      type t = {
          status: Section_status.t
        ; kind: Kind.nullable
        ; vgpus: nullable_entry_list
      }
      [@@deriving rpcty]
    end

    (* The reply to a probe of one card: its sections, and the case it found *)
    module Probe = struct
      (* The case the probe found; named Case, as Result would hide Stdlib's
         Result *)
      module Case = struct
        type known = Partitions | None_carved | Unreadable | Not_applicable
        [@@deriving rpcty]

        type t = known open_enum

        let typ_of = typ_of_open_enum typ_of_known
      end

      type t = {result: Case.t; reason: Kind.nullable} [@@deriving rpcty]

      (* The probe's sections, each left out when the helper did not read it;
         a usage error has no probe. Which of them a status requires is for the
         reader of the reply to decide. *)
      type data = {
          mig_mode: Mig_mode.t option
        ; geometry: Geometry.t option
        ; vgpu: Vgpu.t option
        ; probe: t option
      }
      [@@deriving rpcty]

      (* Builds a probe's data; a member added later is one more optional
         argument, so its callers do not change *)
      let make_data ?mig_mode ?geometry ?vgpu ?probe () =
        {mig_mode; geometry; vgpu; probe}

      include Envelope.Reply (struct
        type t = data

        let typ_of = typ_of_data
      end)
    end

    (* The reply to --version: the probe reply's envelope with its own data. A
       usage error's data is {}, which only Envelope.Core reads *)
    module Version = struct
      module Verb_status = struct
        type known = Supported | Deprecated | Withdrawn [@@deriving rpcty]

        type t = known open_enum

        let typ_of = typ_of_open_enum typ_of_known
      end

      (* A command the helper serves, its status and its maximum duration. A
         maximum that is absent, null or not an integer reads as None, on
         purpose: a bad maximum then costs only its own command, which keeps
         the reader's default. The reader decides what a maximum that is not
         positive, or larger than the reader allows, means. *)
      type verb = {
          command: string
        ; status: Verb_status.t
        ; max_duration_ms: integer option [@default None]
      }
      [@@deriving rpcty]

      type data = {
          schema_versions: (integer * integer) list
        ; verbs: verb list
        ; page_encodings: string list
      }
      [@@deriving rpcty]

      (* Builds a --version reply's data; a member added later is one more
         optional argument, so its callers do not change *)
      let make_data ~schema_versions ~verbs ~page_encodings () =
        {schema_versions; verbs; page_encodings}

      include Envelope.Reply (struct
        type t = data

        let typ_of = typ_of_data
      end)
    end
  end

  include V1_0
end
