(*
 * Copyright (C) 2026 Cloud Software Group
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

type axis = {capable: bool; enabled: bool; after: bool}

let absent = {capable= false; enabled= false; after= false}

let axis ~enabled ~after = {capable= true; enabled; after}

let combine_axis a b =
  {
    capable= a.capable || b.capable
  ; enabled= a.enabled || b.enabled
  ; after= a.after || b.after
  }

type mode =
  | Not_supported
  | Disabled
  | Enable_on_reboot
  | Enabled
  | Disable_on_reboot
[@@deriving show]

let string_of_mode = show_mode

let axis_to_mode x =
  if not x.capable then
    Not_supported
  else
    match (x.enabled, x.after) with
    | false, false ->
        Disabled
    | false, true ->
        Enable_on_reboot
    | true, true ->
        Enabled
    | true, false ->
        Disable_on_reboot

let axis_of_mode = function
  | Not_supported ->
      absent
  | Disabled ->
      axis ~enabled:false ~after:false
  | Enable_on_reboot ->
      axis ~enabled:false ~after:true
  | Enabled ->
      axis ~enabled:true ~after:true
  | Disable_on_reboot ->
      axis ~enabled:true ~after:false

let combine m n = axis_to_mode (combine_axis (axis_of_mode m) (axis_of_mode n))
