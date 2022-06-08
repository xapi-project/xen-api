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

type component = Xen | Kernel

(** The status of running live patch on a host.
 * [component]: identify the component a runing live patch refers;
 * [base_build_id]: the build id of the running component;
 * [base_version]: the RPM version of the running component;
 * [base_release]: the RPM release of the running component;
 * [to_version]: the target component's RPM version of the running live patch;
 * [to_release]: the target component's RPM release of the running live patch.
 *
 * If there is no running live patch for the running component,
 * the "to_version" and "to_release" would be 'None'.
 *)
type t = {
    component: component
  ; base_build_id: string
  ; to_version: string option
  ; to_release: string option
}

val component_of_string : string -> component

val string_of_component : component -> string

val to_json : t -> Yojson.Basic.t

val of_json : Yojson.Basic.t -> t

val get_applied_livepatches : unit -> t list

(** The Xen live patch utility functions *)
module XenLivePatch : sig
  val get_running_livepatch' :
    string -> (string * string * string * string) option
end

(** The Kernel live patch utility functions *)
module KernelLivePatch : sig
  val get_running_livepatch' :
    string -> (string * string * string * string) option
end
