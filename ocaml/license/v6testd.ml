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

module P = V6rpc.V6process(Fakev6)

module D=Debug.Debugger(struct let name="v6daemon" end)
open D

let _ =
	Debug.set_facility Syslog.Local4;
	debug "V6testd started";
	V6daemon.startup (fun () -> ()) P.process

