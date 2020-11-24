(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

(** Types used to store vpx: *****************************************************************)

type t = XenServer | ESXServer | VirtualCenter | HyperVServer

let of_string = function
  | "default" | "XenServer" ->
      XenServer
  | "ESXServer" ->
      ESXServer
  | "VirtualCenter" ->
      VirtualCenter
  | "HyperVServer" ->
      HyperVServer
  | x ->
      raise (Api_errors.Server_error (Api_errors.invalid_value, [x]))

let to_string = function
  | XenServer ->
      "XenServer"
  | ESXServer ->
      "ESXServer"
  | VirtualCenter ->
      "VirtualCenter"
  | HyperVServer ->
      "HyperVServer"
