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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

(** The guidance of metadata for one update in updateinfo *)
module Guidance : sig
  type t =
    | RebootHost
    | RestartToolstack
    | EvacuateHost
    | RestartDeviceModel
    | RebootHostOnLivePatchFailure
    | RebootHostOnKernelLivePatchFailure
    | RebootHostOnXenLivePatchFailure
    | RestartVM

  type kind = Mandatory | Recommended | Full | Livepatch

  val kind_to_string : kind -> string

  val compare : t -> t -> int

  val to_string : t -> string

  val to_json : t -> Yojson.Basic.t

  (* may fail *)
  val of_string : string -> t

  val of_pending_guidance :
       [< `reboot_host
       | `reboot_host_on_livepatch_failure
       | `reboot_host_on_kernel_livepatch_failure
       | `reboot_host_on_xen_livepatch_failure
       | `restart_device_model
       | `restart_toolstack
       | `restart_vm ]
    -> t

  val to_pending_guidance :
       t
    -> [> `reboot_host
       | `reboot_host_on_livepatch_failure
       | `reboot_host_on_kernel_livepatch_failure
       | `reboot_host_on_xen_livepatch_failure
       | `restart_device_model
       | `restart_toolstack
       | `restart_vm ]
       option
end

(** The applicability of metadata for one update in updateinfo *)
module Applicability : sig
  type inequality = Lt | Eq | Gt | Lte | Gte

  type t = {
      name: string
    ; arch: string
    ; inequality: inequality option
    ; epoch: Rpm.Epoch.t
    ; version: string
    ; release: string
  }

  exception Invalid_inequality

  val inequality_of_string : string -> inequality

  val eval :
       epoch:Rpm.Epoch.t
    -> version:string
    -> release:string
    -> applicability:t
    -> bool

  val to_string : t -> string

  val of_xml : Xml.xml -> t option
end

(** The repository metadata *)
module RepoMetaData : sig
  type t = {checksum: string; location: string}

  type datatype = UpdateInfo | Group

  val of_xml : Xml.xml -> datatype -> t

  val of_xml_file : string -> datatype -> t
end

(** The live patch of metadata for one update in updateinfo *)
module LivePatch : sig
  type t = {
      component: Livepatch.component
    ; base_build_id: string
    ; base_version: string
    ; base_release: string
    ; to_version: string
    ; to_release: string
  }

  val compare : t -> t -> int

  val to_json : t -> Yojson.Basic.t

  val to_string : t -> string

  val of_xml : Xml.xml list -> t list
end

module Severity : sig
  type t = None | High

  val to_string : t -> string

  (* may fail *)
  val of_string : string -> t
end

(** The type of [guidance] in updateinfo metadata. *)
module GuidanceInUpdateInfo : sig
  type t = (Guidance.kind * Guidance.t list) list

  val default : t

  val of_xml : Xml.xml list -> t

  val to_json : t -> Yojson.Basic.t

  val to_string : t -> string
end

(** The metadata of one update in updateinfo. *)
module UpdateInfo : sig
  type t = {
      id: string
    ; summary: string
    ; description: string
    ; guidance: GuidanceInUpdateInfo.t
    ; guidance_applicabilities: Applicability.t list
    ; spec_info: string
    ; url: string
    ; update_type: string
    ; livepatches: LivePatch.t list
    ; issued: Xapi_stdext_date.Date.t
    ; severity: Severity.t
    ; title: string
  }

  val to_json : t -> Yojson.Basic.t

  val guidance_to_string : Guidance.t option -> string

  val of_xml : Xml.xml -> (string * t) list

  val of_xml_file : string -> (string * t) list

  val get_guidances_of_kind : kind:Guidance.kind -> t -> Guidance.t list
end

module HostUpdates : sig
  type t = {
      host: string
    ; guidance: GuidanceInUpdateInfo.t
    ; rpms: Rpm.Pkg.t list
    ; update_ids: string list
    ; livepatches: LivePatch.t list
  }

  val to_json : t -> Yojson.Basic.t
end
