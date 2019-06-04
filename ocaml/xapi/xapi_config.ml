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

open Xapi_globs
open Printf

module D=Debug.Make(struct let name="xapi_config" end)
open D

let log_if_not_empty format_string value =
  if value <> "" then debug format_string value

let dump_config () =
  debug "Server configuration:";
  log_if_not_empty "product_version: %s" (Xapi_version.product_version ());
  log_if_not_empty "product_brand: %s" (Xapi_version.product_brand ());
  debug "platform_version: %s" (Xapi_version.platform_version ());
  debug "platform_name: %s" (Xapi_version.platform_name ());
  debug "build_number: %s" (Xapi_version.build_number ());
  debug "git changeset: %s" Xapi_version.git_id;
  debug "version: %d.%d" version_major version_minor;
  (* debug "License filename: %s" !License_file.filename *)
