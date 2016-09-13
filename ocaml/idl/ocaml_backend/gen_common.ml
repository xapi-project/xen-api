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

(** Common definitions used across the ocaml code *)
module O = Ocaml_syntax
module DT = Datamodel_types
module DU = Datamodel_utils
module DM = Datamodel
module OU = Ocaml_utils

(* XXX: probably should move stuff out of Gen_client and put it here instead *)

let context = "__context"
let context_with_correct_database = "(Context.check_for_foreign_database ~__context)"
let context_arg = O.Named(context, "Context.t")

