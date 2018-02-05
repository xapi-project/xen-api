(*
 * Copyright (C) Citrix Systems Inc.
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

type backtrace = {
  error: string;
  (* Python json.dumps and rpclib are not very friendly *)
  files: string list;
  lines: int list;
} [@@deriving rpc]

(* This matches xapi.py:exception *)
type error = {
  code: string;
  params: string list;
  backtrace: backtrace;
} [@@deriving rpc]
