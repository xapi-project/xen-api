(*
 * Copyright (C) 2023 Cloud Software Group
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

(** Xapi_observer_components module contains generic operations on components
  * instrumented with tracing. These operations act on components independently
  * of observers.
  *)

(** Type for components that are instrumented with tracing.
  *)
type t = Xapi | Xenopsd | Xapi_clusterd | SMApi [@@deriving ord]

val all : t list
(** List of all components available.
  *)

val to_string : t -> string
(** Converts a component type to its string equivalent.
  *)

val of_string : string -> t
(** Convert a string to a component.
  * Raises Unsupported_Component if the component is not supported.
  *)

val startup_components : unit -> t list
(** List of observed components on startup.
  *)

val assert_valid_components : string list -> unit
(** Checks if a given list is made of only string equivalents of supported
  * components.
  * Raises Server_error if at least one of the elements is invalid.
  *)

val observed_components_of : t list -> t list
(** Transforms a list of components to a list of components that are expected
  * to be observed. If the list is empty we expect all startup components.
  *)

val is_component_enabled : component:t -> bool
(** Returns [true] if the given component is enabled, [false] otherwise.
  *)

val is_smapi_enabled : unit -> bool
(** Returns [true] if [SMApi] component is enabled, [false] otherwise.
  *)

val dir_name_of_component : t -> string
(** Returns the directory path that python scripts check to see if a component
  * is enabled.
  *)

val env_exe_args_of :
     component:t
  -> exe:string
  -> args:string list
  -> string array option * string * string list
(** Returns an array option of environment variables and the modified exe and
  *  args used by python scripts to configure the python observers.
  *)
