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

module Xs = Xs_client_unix.Client(Xs_transport_unix_client)

(** {2 XC and XS interface helpers.} *)
let with_xc_and_xs xs_client f =
	Xenctrl.with_intf (fun xc -> Xs.immediate xs_client (fun xs -> f xc xs))

let with_xc_and_xs_final xs_client f cf =
	with_xc_and_xs xs_client (fun xc xs -> Xapi_stdext_pervasives.Pervasiveext.finally (fun () -> f xc xs) cf)
