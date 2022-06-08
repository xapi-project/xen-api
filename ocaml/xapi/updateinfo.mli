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

  type guidance_kind = Absolute | Recommended

  val compare : t -> t -> int

  val to_string : t -> string

  val of_string : string -> t
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

  val to_json : t -> Yojson.Basic.t

  val to_string : t -> string

  val of_xml : Xml.xml list -> t list
end

(** The metadata of one update in updateinfo *)
module UpdateInfo : sig
  type t = {
      id: string
    ; summary: string
    ; description: string
    ; rec_guidance: Guidance.t option
    ; abs_guidance: Guidance.t option
    ; guidance_applicabilities: Applicability.t list
    ; spec_info: string
    ; url: string
    ; update_type: string
    ; livepatch_guidance: Guidance.t option
    ; livepatches: LivePatch.t list
  }

  val to_json : t -> Yojson.Basic.t

  val guidance_to_string : Guidance.t option -> string

  val of_xml : Xml.xml -> (string * t) list

  val of_xml_file : string -> (string * t) list
end
