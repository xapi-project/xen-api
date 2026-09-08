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
    than {!Cli_args.to_pairs}.

    Each entry is [fun params -> Cli_args.view "name" params]: a lens onto the
    [name:*] entries of [params], presenting their keys with the [name:] prefix
    removed. This is the one place, in code, that lists which map/set parameter
    names the CLI understands. *)

let args params = Cli_args.view "args" params

let config params = Cli_args.view "config" params

let database params = Cli_args.view "database" params

let device_config params = Cli_args.view "device-config" params

let driver_params params = Cli_args.view "driver-params" params

let ha_config params = Cli_args.view "ha-config" params

let image_format params = Cli_args.view "image-format" params

let other_config params = Cli_args.view "other-config" params

let power_on_config params = Cli_args.view "power-on-config" params

let properties params = Cli_args.view "properties" params

let remote_config params = Cli_args.view "remote-config" params

let schedule params = Cli_args.view "schedule" params

let sm_config params = Cli_args.view "sm-config" params

let vdi params = Cli_args.view "vdi" params

let vgpu params = Cli_args.view "vgpu" params

let vif params = Cli_args.view "vif" params

let xenstore_data params = Cli_args.view "xenstore-data" params

(* Set-valued: read with Cli_args.keys. *)
let tags params = Cli_args.view "tags" params
