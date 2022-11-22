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

(* Note: this used to be in Helpers; moved due to cyclic dependencies relating to License *)

module D = Debug.Make (struct let name = __MODULE__ end)

open D

let log_if_not_empty format_string value =
  if value <> "" then debug format_string value

let dump_config () =
  let open Xapi_version in
  debug "Server configuration:" ;
  log_if_not_empty "product_version: %s" (product_version ()) ;
  log_if_not_empty "product_brand: %s" (product_brand ()) ;
  debug "platform_version: %s" (platform_version ()) ;
  debug "platform_name: %s" (platform_name ()) ;
  debug "build_number: %s" (build_number ()) ;
  debug "version: %s" version

(* debug "License filename: %s" !License_file.filename *)
