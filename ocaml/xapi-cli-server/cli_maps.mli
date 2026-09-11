(*
 * Copyright (C) 2026 Vates.
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

(** Named views onto the map-valued parameters of an [xe] command line (the
    [name:key=value] syntax), and the set-valued parameters -- which are maps
    whose values are ignored, so read the result with {!Cli_args.keys} rather
    than {!Cli_args.map_contents}.

    Each accessor is [fun params -> Cli_args.view "name" params]: a lens onto the
    [name:*] entries of [params], presenting their keys with the [name:] prefix
    removed. This is the one place, in code, that lists which map/set parameter
    names the CLI understands. *)

val args : 'a Cli_args.t -> 'a Cli_args.t

val config : 'a Cli_args.t -> 'a Cli_args.t

val database : 'a Cli_args.t -> 'a Cli_args.t

val device_config : 'a Cli_args.t -> 'a Cli_args.t

val driver_params : 'a Cli_args.t -> 'a Cli_args.t

val ha_config : 'a Cli_args.t -> 'a Cli_args.t

val image_format : 'a Cli_args.t -> 'a Cli_args.t

val other_config : 'a Cli_args.t -> 'a Cli_args.t

val power_on_config : 'a Cli_args.t -> 'a Cli_args.t

val properties : 'a Cli_args.t -> 'a Cli_args.t

val remote_config : 'a Cli_args.t -> 'a Cli_args.t

val schedule : 'a Cli_args.t -> 'a Cli_args.t

val sm_config : 'a Cli_args.t -> 'a Cli_args.t

val vdi : 'a Cli_args.t -> 'a Cli_args.t

val vgpu : 'a Cli_args.t -> 'a Cli_args.t

val vif : 'a Cli_args.t -> 'a Cli_args.t

val xenstore_data : 'a Cli_args.t -> 'a Cli_args.t

val tags : 'a Cli_args.t -> 'a Cli_args.t
(** Set-valued: read the result with {!Cli_args.keys}. *)
