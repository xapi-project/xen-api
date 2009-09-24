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

type op =        
    Cli_printer.print_fn ->
      (Xml.xml -> Xml.xml) ->
      API.ref_session -> ((string*string) list) -> unit

type imp = 
    With_fd of (Unix.file_descr -> op)
  | With_fd_local_session of (Unix.file_descr -> op)
  | No_fd of op
  | No_fd_local_session of op

(* FIXME: print warnings to the standard error channel when a user invokes a deprecated command. *)

type flag = Vm_selectors | Host_selectors | Standard | Neverforward | Hidden | Deprecated of string list

type cmd_spec =
    {reqd:string list;
     optn:string list; (* optional arguments *)
     help:string;
     implementation: imp;
     flags:flag list}


