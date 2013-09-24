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
(**
 * @group Host Management
 *)
 
open Pervasiveext
open Stringext
open Unixext
open Http
open Forkhelpers

module D = Debug.Make(struct let name="xapi" end)
open D

let destroy ~__context ~self = 
  Db.Host_patch.destroy ~__context ~self

let apply ~__context ~self =
  raise (Api_errors.Server_error (Api_errors.message_deprecated, 
                                  []))
