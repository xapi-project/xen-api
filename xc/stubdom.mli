(*
 * Copyright (C) 2011 Citrix Systems Inc.
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

val memory_kib: int64

val create: xc:Xenctrl.handle -> xs:Xenstore.Xs.xsh -> Xenctrl.domid -> Xenctrl.domid

val build: Xenops_task.Xenops_task.t
  -> xc:Xenctrl.handle
  -> xs:Xenstore.Xs.xsh
  -> store_domid:int
  -> console_domid:int
  -> Device.Dm.info
  -> Xenctrl.domid
  -> Xenctrl.domid
  -> unit
