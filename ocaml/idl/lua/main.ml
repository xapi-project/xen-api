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
open Pervasiveext
open Printf
open Stringext
open Str

open Datamodel_types
open Dm_api
module DT = Datamodel_types
module DU = Datamodel_utils

let gen_error file name params =
  fprintf file "xenapi_exceptions[\"%s\"] = \"%s\"\n" name params.err_doc

let gen_exception_translation_database () =
  let file = open_out "xenapi_errors.lua" in
    fprintf file "xenapi_exceptions = {}\n";
    Hashtbl.iter (gen_error file) Datamodel.errors

let _ = gen_exception_translation_database ()

