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

module PV_Vnc :
sig
  exception Failed_to_start
  val start : ?statefile:string -> xs:Xenstore.Xs.xsh -> ?ip:string -> Xenctrl.domid -> unit
  val stop : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> unit
end

val get_vnc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option
val get_tc_port : xs:Xenstore.Xs.xsh -> Xenctrl.domid -> int option

