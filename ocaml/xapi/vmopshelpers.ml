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
(** XC, XS and XAL interface helpers.
 * @group Virtual-Machine Management
 *)

open Pervasiveext
open Xenstore

module D = Debug.Debugger(struct let name="xapi" end)
open D

let with_xc f = Xenctrl.with_intf f

let with_xs f =
	let xs = Xs.daemon_open () in
	finally (fun () -> f xs) (fun () -> Xs.close xs)

let with_xc_and_xs f =
	Xenctrl.with_intf (fun xc -> with_xs (fun xs -> f xc xs))

let get_uuid ~xc domid =
	Uuid.uuid_of_int_array (Xenctrl.domain_getinfo xc domid).Xenctrl.handle
